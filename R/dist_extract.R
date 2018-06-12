subset_dist <- utils::getFromNamespace("subset.dist", "proxy")

#' @name distExtractPair
#' @title distExtractPair
#' @description distExtractPair
#' @param object distance object
#' @param index1 index
#' @param index2 index
#' @return (A number) Distance between index1 and index2.
distExtractPair <- function(object, index1, index2){
  dplyr::if_else(index1 == index2
                 , 0
                 , subset_dist(object, c(index1, index2))[1]
                 )
}

#' @name dist_extract
#' @title dist_extract
#' @description dist_extract
#' @param object distance object
#' @param i index
#' @param j index
#' @param product (string) product type. One among: 'inner', 'outer'.
#' @return A vector of distances when 'inner' is used. A matrix of distances
#'   when 'outer' is used.
dist_extract <- function(object
                         , i
                         , j
                         , product = "inner"
                         ){

  size <- attr(object, "Size")
  assertthat::assert_that(all(sapply(i, assertthat::is.count)))
  assertthat::assert_that(!any(i > size))
  if(missing(j)){
    j <- 1:size
  } else {
    assertthat::assert_that(all(sapply(j, assertthat::is.count)))
    assertthat::assert_that(!any(j > size))
  }
  assertthat::assert_that(assertthat::is.string(product) &&
                            product %in% c("inner", "outer")
                          )
  pair <- function(x, y) distExtractPair(object, x, y)

  if(product == "inner"){
    out <- sapply(i, j, pair)
  } else { # outer case
    out <- outer(i, j, Vectorize(pair, vectorize.args = c("x", "y")))
  }

  return(out)
}



