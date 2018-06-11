#' @name dist_extract
#' @title dist_extract
#' @description dist_extract

dist_extract <- function(x, i, j){

  assertthat::assert_that(inherits(x, "dist"))
  n <- attr(x, "Size")
  assertthat::assert_that(length(i) == 1L && assertthat::is.count(i) && i <= n)

  d <- function(i, j){
    if(i == j){
      return(0)
    } else {
      return( x[n*(i-1) - i*(i-1)/2 + j-i] ) # where i < j <= n
    }
  }

  if(!missing(j)){
    assertthat::assert_that(length(j) == 1L && assertthat::is.count(j) && j <= n)

    if(i > j){
      return(d(j, i))
    } else {
      return(d(i, j))
    }

  } else {
    return(Vectorize(d, "j", SIMPLIFY = TRUE)(i, 1:n))
  }
}
