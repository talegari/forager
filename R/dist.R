#' @name dist
#' @title Distance matrix from unsupervised tree ensemble
#' @description Compute 'dist' object from unsupervised tree ensemble
#' @param dataset A dataframe
#' @param method (string) Method to build the tree ensemble. These are
#'   supported: 'synthetic'
#' @param predictMethod Method to predict. These are supported: 'terminalNodes'
#' @param ... Arguments for the tree ensembler
#' @return A object of class 'dist'
#' @details If method is 'synthetic', a tree ensemble is grown to seperate
#'   actual data from synthetic data using 'synthetic_forest' function. If
#'   predictMethod is 'terminalNodes', distance matrix is computed from
#'   cooccurance in terminal nodes using 'forage' function.
#' @examples
#' dm <- dist(iris[, 1:4])
#' attr(dm, "Size")
#' @export
dist <- function(dataset
                 , method        = "synthetic"
                 , predictMethod = "terminalNodes"
                 , ...
                 ){
  arguments <-  list(...)

  if(method == "synthetic"){
    model <- do.call(synthetic_forest, c(list(dataset = dataset), arguments))
  }

  distObject <- forage(object = model
                       , newdata = dataset
                       , what = "dissimilarity"
                       , method = predictMethod
                       , context = "observations"
                       )

  return( distObject )
}

#' @name proximity
#' @aliases similarity
#' @title Proximity matrix from unsupervised tree ensemble
#' @description Compute 'dist/simil' object from unsupervised tree ensemble
#' @param dataset A dataframe
#' @param method (string) Method to build the tree ensemble. These are
#'   supported: 'synthetic'
#' @param predictMethod Method to predict. These are supported: 'terminalNodes'
#' @param ... Arguments for the tree ensembler
#' @return A object of class 'dist/simil'
#' @details If method is 'synthetic', a tree ensemble is grown to seperate
#'   actual data from synthetic data using 'synthetic_forest' function. If
#'   predictMethod is 'terminalNodes', similarity matrix is computed from
#'   cooccurance in terminal nodes using 'forage' function.
#' @examples
#' dm <- proximity(iris[, 1:4])
#' attr(dm, "Size")
#' @export
proximity <- function(dataset
                      , method        = "synthetic"
                      , predictMethod = "terminalNodes"
                      , ...
                      ){
  arguments <-  list(...)

  if(method == "synthetic"){
    model <- do.call(synthetic_forest, c(list(dataset = dataset), arguments))
  }

  distObject <- forage(object = model
                       , newdata = dataset
                       , what = "proximity"
                       , method = predictMethod
                       , context = "observations"
                       )

  return( distObject )
}

similarity <- proximity