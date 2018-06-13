#' @name impute_stack
#' @title Unsupervised imputation by stacking complete and incomplete data
#' @description impute_stack
#' @param dataset dataset
#' @param newdata newdata
#' @param method One among: 'mf', 'proximity'
#' @param seed seed
#' @param ... dotdotdot
#' @return completed dataset
#' @export

impute_stack <- function(dataset
                         , newdata
                         , method = "missforest"
                         , seed   = 1L
                         , ...
                         ){

  if(anyNA(dataset)){
  stop("dataset cannot contain NA")
  }

  if(!anyNA(newdata)){
    message("Data is not missing in newdata. Input dataset is returned asis")
    return(newdata)
  }

  if(method == "missforest"){
  res <- do.call(missRanger::missRanger
             , c(list(data      = rbind(newdata, dataset)
                      , seed    = seed
                      , verbose = 0
                      )
                 , ...
                 )
             )

  resNewdata <- resNewdata[1:nrow(newdata), ]
  return(resNewdata)
  }

  if(method == "proximity"){
  res <- do.call(forest_impute
                 , c(list(dataset = rbind(newdata, dataset)
                          , seed  = seed
                          )
                     , ...
                     )
                )

  resNewdata <- resNewdata[1:nrow(newdata), ]
  return(resNewdata)
  }
}
