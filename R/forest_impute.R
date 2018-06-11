#' @name forest_impute
#' @title Impute using a random forest (or extratrees) in un/supervised setting.
#' @description In the unsupervised case, random forest models are built on the
#'   imputed data (of the previous iteration) and used to impute data in the
#'   iteration until a stopping criteria is reached. In the supervised case, one
#'   random forest model is used to impute for every iteration. See details.
#' @param dataset A list with two components:
#'
#'   \itemize{
#'
#'   \item First item (datasetComplete) should be a dataframe without missing
#'   values.
#'
#'   \item Second item (datasetMissingBoolean) should be a dataframe with TRUE
#'   at the position where data is missing, FALSE otherwise. The dimension and
#'   column names should be identical to datasetComplete.
#'
#'   }
#' @param object A randomForest/ranger model.
#' @param method (string) A method to build the random forest model when object
#'   is missing. Currently, only "synthetic" is implemented.
#' @param predictMethod (string) Method to to compute the proximity matrix.
#'   Currently, only "terminalNodes" is implemented.
#' @param tol (number between 0 and 1) Threshold for the percentage change of
#'   the metric_relative.
#' @param maxIter (positive integer) Maximum number of iterations.
#' @param seed (positive integer) seed for growing a forest.
#' @return A list with these elements:
#'
#'   \itemize{
#'
#'   \item data: The imputed dataset.
#'
#'   \item iter: Number of iterations.
#'
#'   \item errors: A vector of metric_relative of the last iteration
#'   corresponding to each covariate.
#'
#'   }
#' @details
#'
#' \itemize{ \item In the unsupervised case, when "synthetic" method is chosen,
#' a random forest is grown to separate actual data from synthetic data. When
#' the predictMethod is "terminalNodes", the proximity matrix is obtained using
#' \code{\link{predict_proximity_terminalNodes}}. In the supervised case, a
#' random forest model is provided to the function.
#'
#' \item The missing data in each covariate is imputed by averaging non-missing
#' values of the covariate where the weights are the proximities.
#'
#' \item This is repeated until maximum number of iterations specified by
#' "maxiter" unless for consecutive iterations the percentage change in the
#' metric_relative (RMSE for continuous data, Proportion of disagreements) for
#' each covariate is less than a threshold ("tol").
#'
#' }
#' @seealso \code{\link[randomForest]{rfImpute}}
#' @export

forest_impute <- function(dataset
                          , object        = NULL
                          , method        = "synthetic"
                          , predictMethod = "terminalNodes"
                          , tol           = 0.05
                          , maxIter       = 10L
                          , seed          = 1L
                          , ...
                          ){
  # TODO add assertions
  arguments <- list(...)

  datasetComplete       <- dataset[[1]]
  datasetMissingBoolean <- dataset[[2]]

  assertthat::assertthat(inherits(datasetComplete, "data.frame"))
  assertthat::assertthat(inherits(datasetMissingBoolean, "data.frame"))
  assertthat::assert_that(!anyNA(datasetComplete))
  assertthat::assert_that(
    all(vapply(datasetComplete
               , function(x) is.numeric(x) || is.factor(x) || is.character(x)
               , logical(1)
               )
        )
    , msg = "Columns of dataset should be one of these types: numeric, factor, character."
    )
  assertthat::assert_that(!anyNA(datasetMissingBoolean))
  assertthat::assert_that(unique(sapply(datasetMissingBoolean, class)) == "logical")
  assertthat::assert_that(colnames(datasetComplete) == colnames(datasetMissingBoolean))

  if(!is.null(object)){
    objectValid <- c("ranger", "randomForest")
  assertthat::assert_that(any(inherits(object, objectValid))
                          , msg = paste0("Objects with these classes are supported: "
                                         , toString(objectValid)
                                         )
                          )
  }

  methodValid <- c("terminalNodes")
  assertthat::assert_that(predictMethod %in% methodValid
                         , msg = paste0("Following methods are implemented: "
                                        , toString(methodValid)
                                        )
                         )
  assertthat::assert_that(assertthat::is.number(tol))
  assertthat::assert_that(assertthat::is.count(maxIter) && maxIter >= 2)
  assertthat::assert_that(assertthat::is.count(seed))

  if(is.null(object)){
    if(method == "synthetic"){
      trainFun <- synthetic_forest
    }
    object <- do.call(
      trainFun
      , c(list(dataset = datasetComplete, seed =  seed), arguments)
      )
  }

  # first iter
  if(predictMethod == "terminalNodes"){
    predictFun <- predict_proximity_observations_terminalNodes
  }
  dO <- predictFun(object, newdata = datasetComplete)


  # function to impute a column
  # df is the dataframe which is imputed from the previous iteration
  impute_column <- function(columnNumber, df){

    # imputed from prev iteration
    imputed <- df[[columnNumber]]

    # function to compute weighted average or weighted category
    weighted <- function(x){

      # index is flags of missing values of the column
      index <- datasetMissingBoolean[, columnNumber]

      if(is.numeric(imputed)){

        if(index[x]){ # the position has missing value
          proximities <- dist_extract(dO, x)
          # weighted average of the non-missing obervations
          proximitiesComplete <- proximities * (!index)
          imputeEstimate      <- sum(proximitiesComplete * imputed)/sum(proximitiesComplete)
          return(imputeEstimate)
        } else {
          return(imputed[x])
        }

      } else { # factor or character case

        if(index[x]){
          proximities  <- dist_extract(dO, x)
          # most frequent class
          classWeights <- tapply(proximities[!index]
                                 , imputed[!index]
                                 , sum
                                 , simplify = FALSE
                                 )
          return(names(which.max(classWeights)))
        } else {
          return(as.character(imputed[x]))
        }
      }
    }

    imputedRes <- unlist(lapply(1:nrow(datasetComplete), weighted))
    if(inherits(imputed, "factor")){
      imputedRes <- factor(imputedRes
                           , levels = levels(imputed)
                           , ordered = is.ordered(imputed)
                           )
    }

    return(imputedRes)
  }

  datasetCurrent <- lapply(1:ncol(datasetComplete), impute_column, datasetComplete)
  data.table::setDF(datasetCurrent)
  data.table::setnames(datasetCurrent
                       , colnames(datasetCurrent)
                       , colnames(datasetComplete)
                       )

  # loop through imputing iterations ----
  for(i in 2:maxIter){

    if(is.null(object)){
      object <- do.call(
        trainFun
        , c(list(dataset = datasetCurrent, seed =  seed), arguments)
        )
    }

    dO             <- predictFun(object, newdata = datasetCurrent)
    datasetImputed <- lapply(1:ncol(datasetComplete), impute_column, datasetCurrent)
    data.table::setDF(datasetImputed)

    # check if iterations need a stop
    percErrors        <- unlist(Map(metric_relative
                                    , datasetCurrent
                                    , datasetImputed
                                    , datasetMissingBoolean
                                    )
                                )
    names(percErrors) <- colnames(datasetCurrent)

    datasetCurrent <- datasetImputed
    data.table::setnames(datasetCurrent
                         , colnames(datasetCurrent)
                         , colnames(datasetComplete)
                         )

    if(max(percErrors) <= tol){
      break
    }
  }

  # return ----
  return(list(data           = datasetCurrent
              , iter         = i
              , errors       = percErrors
              )
         )
}
