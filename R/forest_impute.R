#' @name forest_impute
#' @title Impute using a tree ensemble in un/supervised setting.
#' @description In the unsupervised case, tree ensemble built on the imputed
#'   data (of the previous iteration) and used to impute data until a stopping
#'   criteria is reached. In the supervised case, trained tree ensemble is used
#'   to impute for every iteration. See 'details'.
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
#' @param object An object of class ranger or randomForest. If missing, the
#'   imputation is done in unsupervised mode.
#' @param method (string) A method to build the tree ensemble when object is
#'   missing. Currently, only "synthetic" is implemented.
#' @param predictMethod (string) Method to to compute the proximity matrix.
#'   Currently, only "terminalNodes" is implemented.
#' @param tol (number between 0 and 1) Threshold for the change of the metric.
#'   See 'details'.
#' @param maxIter (positive integer) Maximum number of iterations.
#' @param seed (positive integer) seed for growing a forest.
#' @param ... Arguments to be passed to synthetic_forest in the unsupervised
#'   case.
#' @return A list with these elements:
#'
#'   \itemize{
#'
#'   \item data: The imputed dataset.
#'
#'   \item iter: Number of iterations.
#'
#'   \item errors: A vector of metric of the last iteration corresponding to
#'   each covariate.
#'
#'   }
#' @details
#'
#' \itemize{
#'
#' \item In the unsupervised case, when "synthetic" method is chosen, a random
#' forest is grown using 'datasetComplete' to separate actual data from
#' synthetic data. When the predictMethod is "terminalNodes", the proximity
#' matrix is computed.
#'
#' \item The missing data in each covariate is imputed by averaging non-missing
#' values of the covariate where the weights are the proximities. This is the
#' new 'datasetComplete'.
#'
#' \item This is repeated until maximum number of iterations specified by
#' "maxiter" unless for consecutive iterations the change in the metric (MAPE
#' for continuous data, Proportion of disagreements for factors) for each
#' covariate is less than a threshold ("tol").
#'
#' }
#' @seealso \code{\link[randomForest]{rfImpute}}
#' @export

forest_impute <- function(dataset
                          , responseVarName
                          , method          = "synthetic"
                          , predictMethod   = "terminalNodes"
                          , implementation  = "ranger"
                          , tol             = 0.05
                          , maxIter         = 10L
                          , seed            = 1L
                          , nproc           = 1L
                          , ...
                          ){
  # assertions ----
  arguments <- list(...)

  datasetComplete       <- dataset[[1]]
  datasetMissingBoolean <- dataset[[2]]

  assertthat::assert_that(inherits(datasetComplete, "data.frame"))
  assertthat::assert_that(inherits(datasetMissingBoolean, "data.frame"))
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
  assertthat::assert_that(all.equal(colnames(datasetComplete), colnames(datasetMissingBoolean)))

  if(missing(responseVarName)){
    imputationType <- "unsupervised"
    assertthat::assert_that(method %in% c("synthetic"))
  } else {
    imputationType <- "supervised"
    assertthat::assert_that(assertthat::is.string(responseVarName))
    assertthat::assert_that(responseVarName %in% colnames(datasetComplete))
    assertthat::assert_that(
      inherits(datasetComplete[[responseVarName]]
               , c("integer", "numeric", "factor")
               )
      , msg = "response has to be one of these types: integer, numeric, factor"
      )
  }

  methodValid <- c("terminalNodes")
  assertthat::assert_that(predictMethod %in% methodValid
                         , msg = paste0("Following methods are implemented: "
                                        , toString(methodValid)
                                        )
                         )
  assertthat::assert_that(assertthat::is.string(implementation))
  implementation <- tolower(implementation)
  implementationMethods <- c("ranger", "randomforest")
  assertthat::assert_that(implementation %in% implementationMethods
                         , msg = paste0("Following implementations are implemented: "
                                        , toString(methodValid)
                                        )
                         )
  assertthat::assert_that(assertthat::is.number(tol))
  assertthat::assert_that(assertthat::is.count(maxIter) && maxIter >= 2)
  assertthat::assert_that(assertthat::is.count(seed))
  assertthat::assert_that(assertthat::is.count(nproc))

  # setup ----
  nproc <- max(1, min(nproc, parallel::detectCores() - 1))

  implementationFunction <- dplyr::case_when(
    implementation   == "ranger" ~ ranger::ranger
    , implementation == "randomforest" ~ randomForest::randomForest
    )

  if(imputationType == "unsupervised"){
    if(method == "synthetic"){
      trainFun <- synthetic_forest
    }
    object <- do.call(
      trainFun
      , c(list(dataset = datasetComplete, seed =  seed), arguments)
      )
  } else {
    object <- do.call(
      implementationFunction
      , c(list(dependent.variable.name  = responseVarName
               , data = datasetComplete
               , seed =  seed
               )
          , arguments
          )
      )
  }

  # first iter ----
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
                                 , simplify = TRUE
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

  if(.Platform$OS.type == "unix" && nproc > 1){
    datasetCurrent <- parallel::mclapply(
      1:ncol(datasetComplete)
      , impute_column
      , datasetComplete
      , mc.cores = nproc
      )
  } else {
    datasetCurrent <- lapply(1:ncol(datasetComplete)
                             , impute_column
                             , datasetComplete
                             )
  }

  data.table::setDF(datasetCurrent)
  data.table::setnames(datasetCurrent
                       , colnames(datasetCurrent)
                       , colnames(datasetComplete)
                       )

  # loop through imputing iterations ----
  for(i in 2:maxIter){

    if(imputationType == "unsupervised"){
      if(method == "synthetic"){
        trainFun <- synthetic_forest
      }
      object <- do.call(
        trainFun
        , c(list(dataset = datasetComplete, seed =  seed), arguments)
        )
    } else {
      object <- do.call(
        implementationFunction
        , c(list(dependent.variable.name  = responseVarName
                 , data = datasetComplete
                 , seed =  seed
                 )
            , arguments
            )
        )
    }

    dO             <- predictFun(object, newdata = datasetCurrent)

    if(.Platform$OS.type == "unix" && nproc > 1){
      datasetImputed <- parallel::mclapply(
        1:ncol(datasetComplete)
        , impute_column
        , datasetCurrent
        , mc.cores = nproc
        )
    } else {
      datasetImputed <- lapply(1:ncol(datasetComplete)
                               , impute_column
                               , datasetCurrent
                               )
    }
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
