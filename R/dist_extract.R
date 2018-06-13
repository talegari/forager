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
#' @title Extract distances between specified indexes from a 'dist' object
#' @description Extract distances as a vector when product is inner and as a
#'   matrix when the product is outer
#' @param object distance object
#' @param i index
#' @param j index. If missing, this defaults to 1:size of the dist object
#' @param product (string) product type. One among: 'inner', 'outer'.
#' @details When j is missing, it defaults to 1:size of the dist object. When
#'   the product is inner and lengths of i and j mismatch, the smaller one is
#'   extended to the size of the onger one.
#' @return A vector of distances when 'inner' is used. A matrix of distances
#'   when 'outer' is used.
#' @examples
#' temp <- stats::dist(datasets::mtcars)
#' dist_extract(temp, 1:2, 3:5, "inner")
#' dist_extract(temp, 1:2, 3:5, "outer")
#' @export
dist_extract <- function(object
                         , i
                         , j
                         , product = "inner"
                         ){

  size <- attr(object, "Size")
  assertthat::assert_that(all(vapply(i, assertthat::is.count, logical(1))))
  assertthat::assert_that(!any(i > size))
  if(missing(j)){
    j <- 1:size
  } else {
    assertthat::assert_that(all(vapply(j, assertthat::is.count, logical(1))))
    assertthat::assert_that(!any(j > size))
  }
  assertthat::assert_that(assertthat::is.string(product) &&
                            product %in% c("inner", "outer")
                          )
  pair <- function(x, y) distExtractPair(object, x, y)

  if(product == "inner"){
    fullLength <- max(length(i), length(j))
    i <- rep(i, length.out = fullLength)
    j <- rep(j, length.out = fullLength)

    out <- unlist(Map(pair, i, j))

  } else { # outer case

    out <- outer(i, j, Vectorize(pair, vectorize.args = c("x", "y")))

    labels <- attr(object, "Labels")
    if(!is.null(labels)){
      rownames(out) <- labels[i]
      colnames(out) <- labels[j]
    }
  }

  return(out)
}



