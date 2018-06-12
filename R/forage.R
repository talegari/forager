#' @name forage
#' @title Obtain various outputs using a trained tree ensembles on new data.
#' @description Obtain terminalNodesMatrix, dissimilarity, proximity,
#'   outlyingness on new data using a tree ensemble. Currently, ensembles from
#'   'ranger' and 'randomForest' packages are supported. See details for the
#'   explanation about various outputs.
#' @param object Object of class 'ranger' and 'randomForest'.
#' @param newdata (object inheriting 'data.frame' class) A dataframe.
#' @param what (string) Type of output. Following are implemented:
#'   terminalNodesMatrix, dissimilarity, proximity, outlyingness, depth. Default
#'   is 'dissimilarity'.
#' @param method (string) Method to obtain the output. Following are
#'   implemented: terminalNodes. Default is 'terminalNodes'.
#' @param context (string) Specify whether output should be computed for
#'   'observations' or 'trees'.
#' @param classes (factor) Required when 'what' is 'outlyingness'
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
#' @details TODO
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


