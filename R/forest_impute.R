#' @name forest_impute
#' @title Impute using a tree ensemble in un/supervised setting
#' @description In the unsupervised case, tree ensemble built on the imputed
#'   data (of the previous iteration) in an unsupervised way and used to impute
#'   data until a stopping criteria is reached. In the supervised case, forest
#'   is grown in a supervised way (a response is used) to impute for every
#'   iteration. See 'details'.
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
#' @param responseVarName (string) Name of the response variable (supervised case)
#' @param method (string) A method to build the tree ensemble when object is
#'   missing. Currently, only "synthetic" is implemented.
#' @param predictMethod (string) Method to to compute the proximity matrix.
#'   Currently, only "terminalNodes" is implemented.
#' @param implementation (string) One among: 'ranger', 'randomForest'
#' @param tol (number between 0 and 1) Threshold for the change of the metric.
#'   See 'details'.
#' @param maxIter (positive integer) Maximum number of iterations.
#' @param seed (positive integer) seed for growing a forest.
#' @param nproc (positive integer) Number of parallel processes to be used
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
#' matrix is computed. In the supervised case, forest is grown with a specified
#' response.
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
#'
#' @examples
#' \dontrun{
#' # example of unsupervised imputation
#'
#' library("magrittr")
#'
#' # create 20% artificial missings values at random
#' iris_with_na  <- missRanger::generateNA(iris, 0.2, seed = 1)
#' # impute with mean/mode
#' iris_complete <- randomForest::na.roughfix(iris_with_na)
#' # dataframe of missing positions
#' iris_missing  <- is.na(iris_with_na) %>% as.data.frame()
#'
#' imp1        <- forest_impute(list(iris_complete, iris_missing)
#'                              , implementation = "ranger"
#'                              )
#'
#' imp1        <- forest_impute(list(iris_complete, iris_missing)
#'                              , implementation = "randomForest"
#'                              )
#'
#' imp1$iter # number of iterations
#' imp1$errors # errors of the last iteration
#'
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
#' compare_roughimpute_with_actual <-
#'   Map(metric_relative, iris_complete, iris, iris_missing) %>%
#'     unlist()
#' compare_forest_impute_with_actual <-
#'   Map(metric_relative, imp1$data, iris, iris_missing) %>%
#'     unlist()
#'
#' perf <- data.frame(
#'   colnames = names(compare_forest_impute_with_actual)
#'   , rough  = round(compare_roughimpute_with_actual, 2)
#'   , forest = round(compare_forest_impute_with_actual, 2)
#'   )
#' rownames(perf) <- NULL
#' perf
#'
#' # example of supervised imputation
#'
#' # create data for supervised case
#' iris_complete2         <- iris_complete
#' iris_complete2$Species <- iris$Species
#'
#' iris_missing2 <- iris_missing
#' iris_missing2$Species <- rep(FALSE, length(iris_missing))
#'
#' imp2        <- forest_impute(list(iris_complete2, iris_missing2)
#'                              , "Species"
#'                              , implementation = "ranger"
#'                              )
#'
#'
#' imp2        <- forest_impute(list(iris_complete2, iris_missing2)
#'                              , "Species"
#'                              , implementation = "randomForest"
#'                              )
#'
#' compare_forest_impute_sup_with_actual <-
#'   Map(metric_relative, imp2$data, iris, iris_missing2) %>% unlist()
#'
#' perf2 <- data.frame(
#'   colnames     = names(compare_forest_impute_sup_with_actual)
#'   , rough      = round(compare_roughimpute_with_actual, 2)
#'   , forest_sup = round(compare_forest_impute_sup_with_actual, 2)
#'   )
#' rownames(perf2) <- NULL
#' perf2
#' cbind(perf, forest_sup = perf2[,3])
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
    assertthat::assert_that(!anyNA(datasetComplete[[responseVarName]])
                            , msg = "response should not have missing values")
  }

  methodValid <- c("terminalNodes")
  assertthat::assert_that(predictMethod %in% methodValid
                         , msg = paste0("Following methods are implemented: "
                                        , toString(methodValid)
                                        )
                         )
  assertthat::assert_that(assertthat::is.string(implementation))
  implementationMethods <- c("ranger", "randomForest")
  assertthat::assert_that(implementation %in% implementationMethods
                         , msg = paste0("Following implementations are implemented: "
                                        , toString(implementationMethods)
                                        )
                         )
  assertthat::assert_that(assertthat::is.number(tol))
  assertthat::assert_that(assertthat::is.count(maxIter) && maxIter >= 2)
  assertthat::assert_that(assertthat::is.count(seed))
  assertthat::assert_that(assertthat::is.count(nproc))

  # setup ----
  nc    <- ncol(datasetComplete)
  nr    <- nrow(datasetComplete)
  cn    <- colnames(datasetComplete)
  nproc <- max(1, min(nproc, parallel::detectCores() - 1))

  ooberrors <- numeric(0)

  implementationFunction <-
    switch(implementation
           , ranger       = ranger::ranger
           , randomForest = randomForest::randomForest
           )

  predictFun <- switch(predictMethod
                       , terminalNodes = predict_proximity_observations_terminalNodes
                       )

  # separate imputations for un/supervised cases ----
  if(imputationType == "unsupervised"){

    # setup the train function
    trainFun <- switch(method
                       , synthetic = synthetic_forest
                       )
    # for the first iteration
    datasetCurrent <- data.table::copy(datasetComplete)
    data.table::setDT(datasetCurrent)

    # loop to iterate and impute ----
    for(i in 1:maxIter){

      message("iteration: ", i, ", ", appendLF = FALSE)

      # grow a forest
      object <- do.call(
        trainFun
        , c(list(dataset = datasetCurrent, seed =  seed, implementation = implementation), arguments)
        )

      ooberror  <- extract_ooberror(object)
      ooberrors <- append(ooberrors, ooberror)
      message("oobError: ", round(ooberror, 2))

      # predict and compute distance matrix
      dO <- as.matrix(predictFun(object, newdata = datasetCurrent))

      # function to impute a column (works on datasetCurrent)
      imputer <- function(j){

        miss <- datasetMissingBoolean[[j]]
        if(sum(miss) == 0){
          return(datasetCurrent[[j]])
        }

        # imputing factor
        if(is.factor(datasetCurrent[[j]])){
          lvls    <- levels(datasetCurrent[[j]])
          imputed <- apply(
            dO[-miss, miss, drop = FALSE]
            , 2
            , function(x) lvls[which.max(tapply(x, datasetCurrent[[j]][-miss], mean))]
            )

          imp        <- as.character(datasetCurrent[[j]])
          imp[miss]  <- imputed
          imp        <- factor(imp
                              , levels = lvls
                              , ordered = is.ordered(datasetCurrent[[j]])
                              )
        } else {

          weights <- colSums(dO[-miss, miss, drop = FALSE])
          imputed <- (dO[miss, -miss, drop = FALSE] %*% datasetCurrent[[j]][-miss]) / (1e-8 + weights)
          imp        <- datasetCurrent[[j]]
          imp[miss]  <- imputed
        }

        return(imp)
      }

      # run imputation in parallel (result: datasetImputed)
      if(.Platform$OS.type == "unix" && nproc > 1){
        datasetImputed <- parallel::mclapply(1:nc
                                             , imputer
                                             , mc.cores = nproc
                                             )
      } else {
        datasetImputed <- lapply(1:nc, imputer)
      }

      # cleanup datasetImputed
      data.table::setDT(datasetImputed)
      data.table::setnames(datasetImputed, colnames(datasetImputed), cn)


      # check for loop break
      metrics <- mapply(metric_relative
                        , datasetImputed
                        , datasetCurrent
                        , datasetMissingBoolean
                        )
      if(all(metrics < tol)){
        break
      }

      # assign datasetImputed to datasetCurrent
      datasetCurrent <- data.table::copy(datasetImputed)
    }
  } else {

    # for the first iteration
    datasetCurrent <- data.table::copy(datasetComplete)
    data.table::setDT(datasetCurrent)
    datasetCurrentX <- datasetCurrent[, -c(responseVarName), with = FALSE]

    for(i in 1:maxIter){

      message("iteration: ", i, ", ", appendLF = FALSE)

      # train the forest
      if(implementation == "ranger"){
        object <- do.call(
          implementationFunction
          , c(list(dependent.variable.name  = responseVarName
                   , data = datasetCurrent
                   , seed =  seed
                   )
              , arguments
              )
          )
      }

      if(implementation == "randomForest"){
        set.seed(seed)
        object <- do.call(
          implementationFunction
          , c(list(x = datasetCurrent[, -c(responseVarName), with = FALSE]
                   , y = datasetCurrent[[responseVarName]]
                   )
              , arguments
              )
          )
      }

      ooberror  <- extract_ooberror(object)
      ooberrors <- append(ooberrors, ooberror)
      message("oobError: ", round(ooberror, 2))

      # predict and compute distance matrix
      dO <- as.matrix(predictFun(object
                                 , newdata = datasetCurrent[, -c(responseVarName), with = FALSE])
                      )

      # function to impute a column (works on datasetCurrent except responseVarName)
      imputer <- function(j){

        miss <- datasetMissingBoolean[[j]]
        if(sum(miss) == 0){
          return(datasetCurrent[[j]])
        }

        # imputing factor
        if(is.factor(datasetCurrent[[j]])){
          lvls    <- levels(datasetCurrent[[j]])
          imputed <- apply(
            dO[-miss, miss, drop = FALSE]
            , 2
            , function(x) lvls[which.max(tapply(x, datasetCurrent[[j]][-miss], mean))]
            )

          imp        <- as.character(datasetCurrent[[j]])
          imp[miss]  <- imputed
          imp        <- factor(imp
                              , levels = lvls
                              , ordered = is.ordered(datasetCurrent[[j]])
                              )
        } else {

          weights <- colSums(dO[-miss, miss, drop = FALSE])
          imputed <- (dO[miss, -miss, drop = FALSE] %*% datasetCurrent[[j]][-miss]) / (1e-8 + weights)
          imp        <- datasetCurrent[[j]]
          imp[miss]  <- imputed
        }

        return(imp)
    }

      # run imputation in parallel (result: datasetImputed)
      if(.Platform$OS.type == "unix" && nproc > 1){
        datasetImputed <- parallel::mclapply(1:nc
                                             , imputer
                                             , mc.cores = nproc
                                             )
    } else {
        datasetImputed <- lapply(1:nc, imputer)
      }

      # cleanup datasetImputed
      data.table::setDT(datasetImputed)
      data.table::setnames(datasetImputed, colnames(datasetImputed), cn)

      # check for loop break
      metrics <- mapply(metric_relative
                        , datasetImputed
                        , datasetCurrent
                        , datasetMissingBoolean
                        )
      if(all(metrics < tol)){
        break
    }

      # assign datasetImputed to datasetCurrent
      datasetCurrent <- data.table::copy(datasetImputed)
    }
  }

  # return ----
  return(list(data        = datasetImputed
              , iter      = i
              , ooberrors = ooberrors
              , errors    = metrics
              )
         )
}
