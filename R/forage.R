#' TODO
#' Add details
#' comments please
#' Set links in expected style
#' Provide examples
#' Add references

#' @name forage
#' @title Obtain various outputs using a trained random forest or extremely
#'   randomized trees on new data.
#' @description Obtain terminalNodesMatrix, dissimilarity, proximity,
#'   outlyingness on new data using a trained random forest. Currently, trained
#'   random forests from 'ranger' and 'randomForest' packages are supported. See
#'   details for the explanation about various outputs.
#' @param object Trained random forest model. Currently, only trained random
#'   forests from 'ranger' and 'randomForest' are supported.
#' @param newdata (object inheriting 'data.frame' class) A dataframe.
#' @param what (string) Type of output. Following are implemented:
#'   terminalNodesMatrix, dissimilarity, proximity, outlyingness, depth. Default is
#'   'dissimilarity'.
#' @param method (string) Method to obtain the output. Following are
#'   implemented: terminalNodes. Default is 'terminalNodes'.
#' @param classes (factor) Required when 'what' is 'outlyingness'.
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
#'   \item \strong{depth}: A matrix with dimension number of
#'   observations times the number of trees. An entry is depth of tgher
#'
#'   }
#' @export
forage <- function(object
                       , newdata
                       , what    = "dissimilarity"
                       , method  = "terminalNodes"
                       , context = "observations"
                       , classes = NULL
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
  if(method == "terminalNodes" & what != "depth"){
    if(what == "terminalNodesMatrix"){
      function_string <- paste("predict", what, method, sep = "_")
    } else {
      function_string <- paste("predict", what, context, method, sep = "_")
    }
  } else {
    function_string <- paste("predict", what, context, sep = "_")
  }

  if(what == "outlyingness"){
    predictResult <- do.call(function_string, list(object, newdata, classes))
  } else {
    predictResult <- do.call(function_string, list(object, newdata))
  }

  # return ----
  return(predictResult)
}

# predict workhorses

#' @name predict_terminalNodesMatrix_terminalNodes
#' @title predict_terminalNodesMatrix_terminalNodes
#' @description predict_terminalNodesMatrix_terminalNodes
predict_terminalNodesMatrix_terminalNodes <- function(object, newdata){

  if(inherits(object, "ranger")){
    predObject          <- stats::predict(object    = object
                                          , data    = newdata
                                          , type    = "terminalNodes"
                                          , verbose = FALSE
                                          )
    terminalNodesMatrix <- predObject[["predictions"]]
  }

  if(inherits(object, "randomForest")){
    predictedRandomForest <- stats::predict(object
                                            , newdata = newdata
                                            , type    = "response"
                                            , nodes   = TRUE
                                            )
    terminalNodesMatrix   <- attr(predictedRandomForest, "nodes")
  }

  return( terminalNodesMatrix )
}

#' @name predict_dissimilarity_observations_terminalNodes
#' @title predict_dissimilarity_observations_terminalNodes
#' @description predict_dissimilarity_observations_terminalNodes
predict_dissimilarity_observations_terminalNodes <- function(object, newdata){

  terminalNodesMatrix <- predict_terminalNodesMatrix_terminalNodes(object, newdata)
  distObject          <- proxy::dist(terminalNodesMatrix
                                     , method = function(x, y) sqrt(1 - mean(x == y))
                                     )

  return( distObject )
}

#' @name predict_proximity_observations_terminalNodes
#' @title predict_proximity_observations_terminalNodes
#' @description predict_proximity_observations_terminalNodes
predict_proximity_observations_terminalNodes <- function(object, newdata){

  terminalNodesMatrix <- predict_terminalNodesMatrix_terminalNodes(object, newdata)
  similObject         <- proxy::simil(terminalNodesMatrix
                                      , method = function(x, y) mean(x == y)
                                      )

  return( similObject )
}

#' @name predict_proximity_trees_terminalNodes
#' @title predict_proximity_trees_terminalNodes
#' @description predict_proximity_trees_terminalNodes
predict_proximity_trees_terminalNodes <- function(object, newdata){

  terminalNodesMatrix <- predict_terminalNodesMatrix_terminalNodes(object, newdata)
  similObject         <- proxy::simil(t(terminalNodesMatrix)
                                      , method = function(x, y) mean(x == y)
                                      )

  return( similObject )
}

#' @name predict_dissimilarity_trees_terminalNodes
#' @title predict_dissimilarity_trees_terminalNodes
#' @description predict_dissimilarity_trees_terminalNodes
predict_dissimilarity_trees_terminalNodes <- function(object, newdata){

  terminalNodesMatrix <- predict_terminalNodesMatrix_terminalNodes(object, newdata)
  distObject         <- proxy::dist(
    t(terminalNodesMatrix)
    , method = function(x, y) sqrt(1 - mean(x == y))
    )

  return( distObject )
}

#' @name predict_outlyingness_observations_terminalNodes
#' @title predict_outlyingness_observations_terminalNodes
#' @description predict_outlyingness_observations_terminalNodes
predict_outlyingness_observations_terminalNodes <- function(object, newdata, classes){

  similObject <- predict_proximity_observations_terminalNodes(object, newdata)
  subset.dist <- utils::getFromNamespace("subset.dist", "proxy")

  classwiseOut <- function(aClass){
    classIndex <- which(classes == aClass)
    classSimil <- (as.matrix(subset.dist(similObject, classIndex)))^2
    return( data.table::data.table(index = classIndex
                                   , outlyingness = 1/colSums(classSimil)
                                   )
            )
  }

  outlyingness <- data.table::rbindlist(lapply(levels(classes), classwiseOut))
  data.table::setDT(outlyingness)
  index <- NULL
  outlyingness <- outlyingness[order(index), ]

  return(outlyingness[["outlyingness"]])
}

#' @name predict_depth_observations.ranger
#' @title predict_depth_observations.ranger
#' @description predict_depth_observations.ranger
predict_depth_observations.ranger <- function(object, data, parallel = TRUE){

  tnm <- predict_terminalNodesMatrix_terminalNodes(object, data)

  if(.Platform$OS.type == "unix" && parallel){
    outList     <- parallel::mclapply(
      1:ncol(tnm)
      , function(x) depth_from_ranger(ranger::treeInfo(object, x))[tnm[, x] + 1L]
      , mc.cores = parallel::detectCores()
      )
  } else {
    outList     <- lapply(
      1:ncol(tnm)
      , function(x) depth_from_ranger(ranger::treeInfo(object, x))[tnm[, x] + 1L]
      )
  }

  outMat      <- unlist(outList)
  dim(outMat) <- dim(tnm)
  return(outMat)
}

#' @name predict_depth_observations.randomForest
#' @title predict_depth_observations.randomForest
#' @description predict_depth_observations.randomForest
predict_depth_observations.randomForest <- function(object, data, parallel = TRUE){

  tnm <- predict_terminalNodesMatrix_terminalNodes(object, data)

  if(.Platform$OS.type == "unix" && parallel){
    outList     <- parallel::mclapply(
      1:ncol(tnm)
      , function(x) depth_from_randomForest(randomForest::getTree(object, x, labelVar = TRUE))[tnm[, x]]
      , mc.cores = parallel::detectCores()
      )
  } else {
    outList     <- lapply(
      1:ncol(tnm)
      , function(x) depth_from_randomForest(randomForest::getTree(object, x, labelVar = TRUE))[tnm[, x]]
      )
  }

  outMat      <- unlist(outList)
  dim(outMat) <- dim(tnm)
  return(outMat)
}

#' @name predict_depth_observations
#' @title predict_depth_observations
#' @description predict_depth_observations
predict_depth_observations <- function(object, data, parallel = TRUE){
  UseMethod("predict_depth_observations", object)
}

