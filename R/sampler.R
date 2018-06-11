#' @name sampler
#' @title Sample a categorical or numeric vector using random or uniform sampling.
#' @description This is intended to be used for synthetic data generation.
#' @param x The vector.
#' @param prop Random sampling when prop = TRUE. Else, uniform sampling is used.
#' @param seed Seed for sampling.
#' @return A sampled vector of the same length.

sampler <- function(x, prop, seed){

  set.seed(seed)
  if(is.numeric(x)){
    if(prop){
      res <- sample(x, size = length(x), replace = TRUE)
    } else {
      res <- stats::runif(n     = length(x)
                          , min = min(x, na.rm = TRUE)
                          , max = max(x, na.rm = TRUE)
                          )
    }
  } else {

    freq <- function(x){

      n     <- NULL
      xdf   <- data.frame(x = x)
      props <- xdf %>%
        dplyr::count(x) %>%
        dplyr::mutate(n = n/nrow(xdf))

      return( dplyr::full_join(xdf, props, by = "x")[["n"]] )
    }

    if(prop){
      res <- sample(x
                , size    = length(x)
                , replace = TRUE
                , prob    = freq(x)
                )
    } else {
      res <- sample(x
                , size    = length(x)
                , replace = TRUE
                , prob    = NULL
                )
    }
  }

  return(res)
}
