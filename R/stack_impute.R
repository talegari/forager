#' @name stack_impute
#' @title Imputation by stacking complete and incomplete data
#' @description impute_stack
#' @param dataset (dataframe) dataset
#' @param newdata (dataframe) newdata
#' @param method (string )One among: 'missforest', 'proximity'
#' @param seed (positive integer) seed
#' @param ... Arguments to be passed to \code{\link[missRanger]{missRanger}} when method is 'missforest',  \code{\link{forest_impute}} when method is 'proximity'
#' @return (dataframe) completed dataset
#' @examples
#' \dontrun{
#' # divide isis data into test and train
#' set.seed(1)
#' index <- sample.int(150, 100)
#'
#' iris_train <- iris[index, ]
#' iris_test  <- iris[-index, ]
#'
#' # create some holes in test data
#' iris_test_missing <- missRanger::generateNA(iris_test, p = 0.2, seed = 2)
#'
#' # stack imputation
#'
#' # use missforest method
#' imputed_mf <- stack_impute(iris_train
#'                            , iris_test_missing
#'                            , method = "missforest"
#'                            , seed = 3
#'                            )
#'
#' # metric: rmse for numeric, proportion of mismatches for categorical
#' metric_relative <- function(x, y, z){
#'
#'   if(sum(z) == 0){
#'     return(0)
#'   }
#'
#'   if(is.numeric(x)){
#'     mean(abs((y[z] - x[z])/y[z]))
#'   } else {
#'     sum(x[z] != y[z])/sum(z)
#'   }
#'
#' }
#'
#' # compare
#' mapply(metric_relative
#'        , iris_test
#'        , imputed_mf
#'        , as.data.frame(is.na(iris_test_missing))
#'        )
#'
#' # use proximity method
#' imputed_pr <- stack_impute(iris_train
#'                            , iris_test_missing
#'                            , method = "proximity"
#'                            , seed = 3
#'                            )
#'
#' # compare
#' mapply(metric_relative
#'        , iris_test
#'        , imputed_pr
#'        , as.data.frame(is.na(iris_test_missing))
#'        )
#' }
#' @export

stack_impute <- function(dataset
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
             , c(list(data      = dplyr::bind_rows(newdata, dataset)
                      , seed    = seed
                      , verbose = 0
                      )
                 , ...
                 )
             )

  resNewdata <- res[1:nrow(newdata), ]
  return(resNewdata)
  }

  if(method == "proximity"){

  binded       <- dplyr::bind_rows(newdata, dataset)
  roughImputed <- roughImpute(binded)
  naPositions  <- as.data.frame(is.na(binded))

  res <- do.call(forest_impute
                 , c(list(dataset = list(roughImputed, naPositions)
                          , seed  = seed
                          )
                     , ...
                     )
                )

  resNewdata <- res[["data"]][1:nrow(newdata), ]
  return(resNewdata)
  }
}
