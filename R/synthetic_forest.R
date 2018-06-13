#' @name synthetic_forest
#' @title Grow a tree ensemble on synthetic data
#' @description Builds a random forest model to classify actual vs synthetic
#'   data where synthetic data is created by sampling each covariate as
#'   suggested in
#'   \href{https://www.stat.berkeley.edu/~breiman/Using_random_forests_v4.0.pdf}{Understaning
#'    random forests} by Brieman.
#'
#' @param dataset A dataframe
#' @param prop (flag) Random sampling of covariates (when prop = TRUE) to
#'   generate synthetic data. Else, uniform sampling is used.
#' @param seed (a positive integer) Seed for sampling.
#' @param implementation (string) Implemenation to use to build the model. The
#'   following are supported: 'ranger', 'randomForest'.
#' @param ... Arguments to be passed to \code{\link[ranger]{ranger}}.
#' @return A tree ensemble with one these classes: 'ranger', 'randomForest'
#' @details
#' \href{https://www.stat.berkeley.edu/~breiman/Using_random_forests_v4.0.pdf}{Understanding
#' random forests} by Brieman involves creating synthetic data by sampling
#' randomly from unvariate distributions of each covariate(feature). This
#' supports two methods: First, where proportions or distribution is taken into
#' account when sampling at random, second where the data is sampled assuming
#' uniform distribution. The former corresponds to "Addcl1" from Horvath's
#' \href{https://doi.org/10.1198/106186006X94072}{paper} and latter corresponds
#' to "addc2". A random forest model is built using \pkg{ranger} or
#' \pkg{randomForest} to learn what separates the actual data from the synthetic
#' data. Default value of number of trees grown is 1000 and  minimum node size
#' to split is set to 5.
#' @references \itemize{
#'
#'   \item \href{https://doi.org/10.1198/106186006X94072}{Unsupervised Learning
#'   With Random Forest Predictors} by Tao Shi & Steve Horvath.
#'
#'   \item
#'   \href{https://www.stat.berkeley.edu/~breiman/Using_random_forests_v4.0.pdf}{Understanding
#'    random forests} by Brieman.
#'
#'   }
#' @examples
#'
#' # ranger
#' model_ranger <- synthetic_forest(iris, implementation = "ranger")
#' summary(model_ranger)
#' model_ranger$prediction.error # OOB prediction error
#'
#' # randomForest
#' model_randomForest <- synthetic_forest(iris, implementation = "randomForest")
#' summary(model_randomForest)
#' mean(model_randomForest$err.rate[,1]) # OOB prediction error
#'
#' # extratrees
#' model_et <- synthetic_forest(iris, implementation = "ranger", splitrule = "extratrees")
#' summary(model_et)
#' model_et$prediction.error # OOB prediction error
#'
#' @export

synthetic_forest <- function(dataset
                             , prop   = TRUE
                             , seed   = 1L
                             , implementation = "ranger"
                             , ...
                             ){

  # assertions                                                                ----
  assertthat::assert_that(inherits(dataset, "data.frame")
                          , msg = "'dataset' should inherit the 'data.frame' class.")
  assertthat::assert_that(!anyNA(dataset), msg = "'dataset' cannot contain missing values.")
  assertthat::assert_that(
    all(vapply(dataset
               , function(x) is.numeric(x) || is.factor(x) || is.character(x)
               , logical(1)
               )
        )
    , msg = "Columns of dataset should be one of these types: numeric, factor, character."
    )
  assertthat::assert_that(assertthat::is.flag(prop))
  assertthat::assert_that(assertthat::is.count(seed))
  assertthat::assert_that(implementation %in% c("ranger", "randomForest"))

  arguments <- list(...)

  # extend data with synthetic                                                ----
  extdata  <- data.table::rbindlist(
    list(dataset
         , generate_synthetic_data(dataset, prop, seed)
         )
    )
  nr <- nrow(dataset)
  while(TRUE){
    response <- paste0(sample(c(letters, LETTERS), 20), collapse = "")
    if(!(response %in% colnames(dataset))){
      break
    }
  }

  extdata[, (response) := factor(c(rep(1L, nr), rep(0L, nr))
                                 , levels = c(1L, 0L)
                               )
          ]
  data.table::setDF(extdata)
  if(implementation == "ranger"){
    # prep arguments for ranger and call it                                     ----
    if(is.null(arguments[["num.trees"]])){
      arguments[["num.trees"]] <- 1000L
    }

    if(is.null(arguments[["min.node.size"]])){
      arguments[["min.node.size"]] <- 5L
    }

    model <- do.call(ranger::ranger
                     , c(list(dependent.variable.name = response
                              , data                  = extdata
                              , seed                  = seed
                              )
                         , arguments
                        )
                     )
  } else {
    # prep arguments for randomForest and call it                               ----
    if(is.null(arguments[["ntree"]])){
      arguments[["ntree"]] <- 1000L
    }

    if(is.null(arguments[["nodesize"]])){
      arguments[["nodesize"]] <- 5L
    }

    if(is.null(arguments[["keep.forest"]])){
      arguments[["keep.forest"]] <- 5L
    }

    set.seed(seed)
    model <- do.call(randomForest::randomForest
                     , c(list(x = extdata[, setdiff(colnames(extdata), response)]
                              , y = extdata[[response]]
                              )
                         , arguments
                        )
                     )
  }

  # return                                                                    ----
  return(model)
}
