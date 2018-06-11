#' @name impute_stack
#' @title impute_stack
#' @description impute_stack

impute_stack <- function(dataset
                         , newdata
                         , method = "mf"
                         , seed   = 1L
                         , ...
                         ){

  if(anyNA(dataset)){
  stop("dataset cannot contain NA.")
  }

  if(!anyNA(newdata)){
  message("No NA to impute. Input dataset is returned asis.")
  return(newdata)
  }

  if(method == "mf"){
  res <- do.call(missRanger::missRanger
             , c(list(data      = rbind(dataset, newdata)
                      , seed    = seed
                      , verbose = 0
                      )
                 , ...
                 )
             )

  resNewdata <- tail(res, nrow(newdata))
  return(resNewdata)
  }

  if(method == "proximity"){
  res <- do.call(unsupervised_impute_proximity
             , c(list(dataset = rbind(dataset, newdata)
                      , seed  = seed
                      )
                 , ...
                 )
             )

  resNewdata <- tail(res[["data"]], nrow(newdata))
  return(resNewdata)
  }
}
