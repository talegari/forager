#' @name roughImpute
#' @title Unsupervised median/mode imputation for dataframes
#' @description Imputes missing values integer/numeric columns with their
#'   median. Imputes missing calues in character/factor columns with their mode.
#'   If a column does not contain even one non-missing value, then error is
#'   thrown.
#' @param dataset A dataframe
#' @return A dataframe (also inherits data.table class)
#' @examples
#' iris_missing <- missRanger::generateNA(iris, p = 0.2, seed = 1)
#' roughImpute(iris_missing)
#' @export

roughImpute <- function(dataset){

  cc <- vapply(dataset, function(x) class(x)[1], character(1))
  assertthat::assert_that(all(cc %in% c("numeric", "integer", "character", "factor", "ordered")))

  dataset <- data.table::copy(dataset)
  data.table::setDT(dataset)

  nc  <- which(cc %in% c("integer", "numeric"))
  nnc <- setdiff(1:ncol(dataset), nc)

  for(ci in nc){
    data.table::set(dataset
                    , i     = which(is.na(dataset[[ci]]))
                    , j     = ci
                    , value = stats::median(dataset[[ci]], na.rm = TRUE)
                    )
  }

  for(ci in nnc){
    data.table::set(dataset
                    , i     = which(is.na(dataset[[ci]]))
                    , j     = ci
                    , value = names(which.max(table(dataset[[ci]])))
                    )
  }

  return(dataset)
}