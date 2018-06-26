#' @name forage
#' @title Obtain various outputs using a trained tree ensemble on new data
#' @description Obtain terminalNodesMatrix, dissimilarity, proximity,
#'   outlyingness, depth on new data using a tree ensemble. Currently, ensembles
#'   from 'ranger' and 'randomForest' packages are supported. See details for
#'   the explanation about various outputs.
#' @param object Object of class 'ranger', 'randomForest'
#' @param newdata A dataframe
#' @param what (string) Type of output. Following are implemented:
#'   terminalNodesMatrix, dissimilarity, proximity, outlyingness, depth. Default
#'   is 'dissimilarity'
#' @param method (string) Method to obtain the output. Following are
#'   implemented: terminalNodes. Default is 'terminalNodes'
#' @param context (string) Specify whether output should be computed for
#'   'observations' or 'trees'. Default is 'observations'
#' @param classes (factor) Required when 'what' is 'outlyingness'
#' @param nproc (positive integer) Number of processors to use
#' @param ... Currently not in use.
#' @return The following are returned depending on 'what':
#'
#'   \itemize{
#'
#'   \item \strong{terminalNodesMatrix}: A matrix with dimension number of
#'   observations times the number of trees. An entry is ID of the terminal
#'   node.
#'
#'   \item \strong{dissimilarity}: A 'dist' object with size of number of
#'   observations.
#'
#'   \item \strong{proximity}: A 'simil/dist' object(from 'proxy' package) with
#'   size of number of observations.
#'
#'   \item \strong{outlyingness}: A vector of measure of outlyingness of each
#'   observation from its class.
#'
#'   \item \strong{depth}: A matrix with dimension number of observations times
#'   the number of trees. An entry is depth of tgher
#'
#'   }
#' @details \itemize{
#'
#'   \item For 'terminalNodes' method, proximity between two observations is
#'   defined as the propotion of trees in which both observations end up in the
#'   same terminal node. Similarly, proximity between two trees is defined as
#'   the rand index of the terminalnode vectors of the trees for all
#'   observations.
#'
#'   \item Dissimilarity is defined as sqrt of (1 - proximity) in case of
#'   observations and (1 - proximity) in the case of trees.
#'
#'   \item When a vector of classes is given, outlyingness of a observation from
#'   its class is defined as the reciprocal sum of squared proximities of the
#'   observations of the class.
#'
#'   }
#' @examples
#' library("magrittr")
#' library("ggplot2")
#'
#' # partiton iris data ----
#' set.seed(1)
#' index      <- sample.int(nrow(iris), floor(0.7 * nrow(iris)))
#' iris_train <- iris[index, ]
#' iris_test  <- iris[-index, ]
#'
#' # grow a randomforest ensembles ----
#' model_ranger_supervised <- ranger::ranger(Species ~., data = iris_train, seed = 1)
#' model_ranger_supervised
#'
#' model_ranger_unsupervised <- synthetic_forest(iris_train)
#' model_ranger_unsupervised$prediction.error
#'
#' # obtain terminal nodes matrix ----
#' tn_supervised  <- forage(model_ranger_supervised
#'                          , newdata = iris_test
#'                          , what = "terminalNodesMatrix"
#'                          )
#' dim(tn_supervised)
#'
#' tn_unsupervised <- forage(model_ranger_unsupervised
#'                           , newdata = iris_test
#'                           , what = "terminalNodesMatrix"
#'                           )
#' dim(tn_unsupervised)
#'
#' # obtain similarity/distance between observations ----
#' di_supervised  <- forage(model_ranger_supervised
#'                          , newdata = iris_test
#'                          , what  = "dissimilarity" # or "proximity"
#'                          )
#' di_supervised %>% attr("Size")
#' di_supervised %>% as.matrix() %>% image()
#'
#' di_unsupervised  <- forage(model_ranger_unsupervised
#'                            , newdata = iris_test
#'                            , what  = "dissimilarity" # or "proximity"
#'                            )
#' di_unsupervised %>% attr("Size")
#' di_unsupervised %>% as.matrix() %>% image()
#'
#' # clustering using hclust partitions by species
#' hclust(di_unsupervised, method = "average") %>% cutree(k = 3) %>% table(unclass(iris_test$Species))
#'
#' # explore outliers in train data ----
#' di_unsupervised_train  <- forage(model_ranger_unsupervised
#'                                  , newdata = iris_train
#'                                  , what  = "dissimilarity"
#'                                  )
#'
#' outIndex <- forage(model_ranger_unsupervised
#'                    , newdata = iris_train
#'                    , what = "outlyingness"
#'                    , classes = iris_train$Species
#'                    )
#'
#' # quick and rough outlier exploration
#' outs   <- outIndex %in% grDevices::boxplot.stats(outIndex)$out
#' which(outs)
#' labels <- (1:nrow(iris_train))
#' labels[outs] <- NA
#' set.seed(1)
#' to <- Rtsne::Rtsne(di_unsupervised_train, is_distance = TRUE, perplexity = 10)
#' to$Y %>% as.data.frame() %>%
#'   ggplot(aes(V1, V2, color = iris_train$Species, size = as.integer(outs))) +
#'   geom_point(alpha = 0.5) +
#'   geom_label(aes(label = labels))
#'
#' # Look at the depth of the terminal nodes of the observations across trees
#' depth_observations <- forage(synthetic_forest(iris_train, splitrule = "extratrees")
#'                              , iris_train
#'                              , what = "depth"
#'                              )
#' depth_observations_sweep <- sweep(depth_observations
#'                                   , 2
#'                                   , matrixStats::colMaxs(depth_observations)
#'                                   , "/"
#'                                   )
#' avg_depth  <- matrixStats::rowMedians(depth_observations_sweep)
#' depthframe <- data.frame(index = 1:nrow(iris_train), depthratio = avg_depth)
#' averages   <- depthframe %>%
#'   dplyr::mutate(Species = iris_train$Species) %>%
#'   dplyr::group_by(Species) %>%
#'   dplyr::summarise(mean = median(depthratio), sd = mad(depthratio))
#'
#' depthframe %>%
#'   ggplot(aes(index, depthratio)) +
#'   geom_point(aes(color = iris_train$Species)) +
#'   geom_label(aes(label = 1:nrow(iris_train), color = iris_train$Species)) +
#'   geom_hline(aes(yintercept = mean, colour = Species), averages) +
#'   geom_hline(aes(yintercept = mean + 1.5 * sd, colour = Species), linetype = 2, averages) +
#'   geom_hline(aes(yintercept = mean - 1.5 * sd, colour = Species), linetype = 2, averages) +
#'   scale_x_continuous(breaks = seq(1, nrow(iris_train))) +
#'   coord_flip()
#' # we might want to examine points of a class
#' # which have very small or large depth compared to the class average.
#'
#' hierar <- stats::dist(depth_observations_sweep, method = "manhattan") %>% hclust()
#' hierar %>% plot()
#' hierar %>% cutree(h = 199) %>% table()
#' # observe that observation '29' which is a global outlier forms the only
#' # singleton cluster(8) at height 199.
#'
#' # dissimilarity(rand index) matrix of trees ----
#' di_supervised_trees  <- forage(model_ranger_supervised
#'                                , newdata = iris_train
#'                                , what  = "dissimilarity" #' or "proximity
#'                                , context = "trees"
#'                                )
#' di_supervised_trees %>% as.matrix() %>% image()
#' di_supervised_trees %>% as.matrix() %>% density() %>% plot()
#' # indication of low 'correlation' between trees.
#' @export
forage <- function(object
                   , newdata
                   , what    = "dissimilarity"
                   , method  = "terminalNodes"
                   , context = "observations"
                   , classes = NULL
                   , nproc   = 1
                   , ...
                   ){
  # assertions ----
  objectValid <- c("ranger", "randomForest")
  assertthat::assert_that(any(inherits(object, objectValid))
                          , msg = paste0("Objects with these classes are supported: "
                                         , toString(objectValid)
                                         )
                          )
  assertthat::assert_that(inherits(newdata, "data.frame")
                          , msg = "'newdata' should inherit the 'data.frame' class.")
  assertthat::assert_that(!anyNA(newdata), msg = "'newdata' cannot contain missing values.")
  assertthat::assert_that(
    all(vapply(newdata
               , function(x) is.numeric(x) || is.factor(x) || is.character(x)
               , logical(1)
               )
        )
    , msg = "Columns of dataset should be one of these types: numeric, factor, character."
    )
  whatValid <- c("terminalNodesMatrix", "dissimilarity", "proximity", "outlyingness", "depth")
  assertthat::assert_that(what %in% whatValid
                          , msg = paste0("Following inputs for 'what' are implemented: "
                                         , toString(whatValid)
                                         )
                          )
  methodValid <- c("terminalNodes")
  assertthat::assert_that(method %in% methodValid
                         , msg = paste0("Following methods are implemented: "
                                        , toString(methodValid)
                                        )
                         )

  contextValid <- c("observations", "trees")
  assertthat::assert_that(context %in% contextValid
                         , msg = paste0("Following contexts are implemented: "
                                        , toString(contextValid)
                                        )
                         )
  assertthat::assert_that(assertthat::is.count(nproc))
  nproc <- max(1, min(nproc, parallel::detectCores() - 1))

  if(what != "outlyingness"){
    if(!is.null(classes)){
      message("'classes' argument is ignored as 'what' is not 'outlyingness'.")
    }
  } else {
    if(!is.null(classes)){
      assertthat::assert_that(
        is.factor(classes) && length(classes) == nrow(newdata)
        , msg = "'classes' has to be a factor with the length equal to number of rows of 'newdata'."
        )
    } else {
      stop("'classes' cannot be NULL.")
    }
    if(context == "trees"){
      stop("outlyingness is valid only in 'observations' context.")
    }
  }

  # create the function_string and call it ----
  # only supported method as of now is 'terminalNodes'
  function_string <- dplyr::case_when(
    what == "terminalNodesMatrix"
      ~ paste("predict"
                , what
                , method
                , sep = "_"
                )
    , what == "depth"
       ~ paste("predict"
                , what
                , sep = "_"
                )
    , what != "depth" && what != "terminalNodesMatrix"
        ~ paste("predict"
                 , what
                 , context
                 , method
                 , sep = "_"
                 )
  )

  if(what == "outlyingness"){
    predictResult <- do.call(function_string
                               , list(object, newdata, classes, nproc)
                             )
  } else {

      if(context == "trees" || what == "terminalNodesMatrix"){
        predictResult <- do.call(function_string, list(object, newdata))
      } else {
          predictResult <- do.call(function_string, list(object, newdata, nproc))
      }
  }

  # return ----
  return(predictResult)
}


