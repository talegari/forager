#' @name forest_rfe
#' @title lightweight implementation of RFE using ranger
#' @description For datasets with large number of predictors, this
#'   implementation has these modifications to regular recursive feature
#'   elimination procedure:
#'
#'   \itemize{
#'
#'   \item Use oob prediction error as a proxy to model performance.
#'
#'   \item Build forests \code{\link[ranger]{ranger}} on samples of data and
#'   average variable importance and oob prediction error.
#'
#'   }
#'
#'   For a comprehensive RFE procedure with resampling, use
#'   \code{\link[caret]{rfe}}
#' @references \itemize{
#'
#'   \item
#'   \href{https://topepo.github.io/caret/recursive-feature-elimination.html}{RFE
#'    using caret}
#'
#'   \item
#'   \href{http://scikit-learn.org/stable/modules/generated/sklearn.feature_selection.RFE.html}{RFE
#'    using scikit learn}
#'
#'   }
#' @param dataset (object inheriting data.frame class) A dataframe
#' @param responseVarName (string) Name of the response variable
#' @param sizes (integer vector) Vector of number of variables. When missing, sizes will be sequence of nc/2^i where the sequnce ranges from nc(number of columns) to 2.
#' @param sampleprop (A real number between 0 and 1 or a vector) Proportion of observations. If not a single number and sizes is specified, this vector should have same length as sizes.
#'   per sample
#' @param nsamples (positive integer or a vector) Number of samples. If not a single number and sizes is specified, this vector should have same length as sizes.
#' @param seed (positive integer) Seed
#' @param ... Arguments to be passed to \code{\link[ranger]{ranger}}
#' @return A tibble with three columns: \itemize{
#'
#'   \item size: Number of variables used \item ooberror: Out-of-box error of
#'   the forest \item varimp: A list-column where each item is a data.frame with
#'   variable names and importance
#'
#'   }
#' @examples
#' temp <- forest_rfe(iris, "Species")
#' temp
#'
#' temp <- forest_rfe(iris
#'                    , "Species"
#'                    , sizes = c(5,4,3)
#'                    , sampleprop = c(0.1, 0.2, 0.3)
#'                    , nsamples = c(10, 20, 30)
#'                    )
#' temp
#'
#' temp <- forest_rfe(iris
#'                    , "Species"
#'                    , sizes = c(5,4,3)
#'                    , sampleprop = 0.1
#'                    , nsamples = c(10, 20, 30)
#'                    )
#' temp
#'
#' temp <- forest_rfe(iris
#'                    , "Species"
#'                    , sizes = c(5,4,3)
#'                    , sampleprop = c(0.1, 0.2, 0.3)
#'                    , nsamples = 10
#'                    )
#' temp
#'
#' temp <- forest_rfe(iris
#'                    , "Species"
#'                    , sizes = c(5, 4, 3)
#'                    , sampleprop = c(0.1, 0.2, 0.3)
#'                    , nsamples = 10
#'                    , mtry = list(4, 3, 2)
#'                    , num.trees = list(300, 500, 1000)
#'                    , case.weights = replicate(3, runif(150), simplify = FALSE)
#'                    )
#' temp
#'
#' @export
forest_rfe <- function(dataset
                       , responseVarName
                       , sizes
                       , sampleprop = 0.2
                       , nsamples = 10
                       , seed = 1
                       , ...
                       ){

  # assertions ----
  assertthat::assert_that(inherits(dataset, "data.frame"))
  assertthat::assert_that(!is.null(colnames(dataset)))
  assertthat::assert_that(assertthat::is.string(responseVarName))
  assertthat::assert_that(responseVarName %in% colnames(dataset))

  nc <- ncol(dataset)
  if(!missing(sizes)){

    assertthat::assert_that(all(sapply(sizes, assertthat::is.count)))
    assertthat::assert_that(length(sizes) == dplyr::n_distinct(sizes))
    sizes <- sort(sizes, decreasing = TRUE)
    assertthat::assert_that(all(sizes <= nc))
    assertthat::assert_that(nc %in% sizes)
    assertthat::assert_that(length(sizes) == length(sampleprop) ||
                              length(sampleprop) == 1
                            )
    if(length(sampleprop) == 1){
      sampleprop <- rep(sampleprop, length(sizes))
    }
    assertthat::assert_that(length(sizes) == length(nsamples) ||
                              length(nsamples) == 1
                            )
    if(length(nsamples) == 1){
      nsamples <- rep(nsamples, length(sizes))
    }
    assertthat::assert_that(
      all(sapply(sampleprop, function(x) dplyr::between(x, 1e-8, 1)))
      )
    assertthat::assert_that(
      all(sapply(nsamples, function(x) assertthat::is.count(x)))
      )

  } else {
    sizes <- unique(ceiling(sapply(0:floor(log(nc, 2)), function(x) nc/2^x)))
    assertthat::assert_that(dplyr::between(sampleprop, 1e-8, 1))
    assertthat::assert_that(assertthat::assert_that(assertthat::is.count(nsamples)))
    sampleprop <- rep(sampleprop, length(sizes))
    nsamples   <- rep(nsamples, length(sizes))
  }

  arguments      <-  list(...)
  if(length(arguments) > 1){
    assertthat::assert_that(
    all(sapply(arguments, function(x) inherits(x, "list")))
    )
  arguments <- lapply(arguments, function(x) rep_len(x, length(sizes)))
  }

  assertthat::assert_that(assertthat::assert_that(assertthat::is.count(seed)))

  # setup ----
  nr             <- nrow(dataset)
  data.table::setDT(dataset)
  predictorNames <- setdiff(colnames(dataset), responseVarName)
  if(is.null(arguments[["importance"]])){
    arguments[["importance"]] <- as.list(rep("impurity", length(sizes)))
  }
  if(is.null(arguments[["write.forest"]])){
    arguments[["write.forest"]] <- as.list(rep(FALSE, length(sizes)))
  }

  # given a resample index, extractImp return the vector of variable importance and oobError
  extractImp <- function(resampleIndex, iter){

    arguments_local <- lapply(arguments, function(x) `[[`(x, iter))
    resampledData   <- dataset[resampleIndex, ]

    if(!is.null(arguments_local[["case.weights"]])){
      arguments_local[["case.weights"]] <-
        arguments_local[["case.weights"]][resampleIndex]

      model <- do.call(
        ranger::ranger
        , c(list(data = resampledData
                 , dependent.variable.name = responseVarName
                 )
            , arguments_local
            )
        )
    } else {

      model <- do.call(
        ranger::ranger
        , c(list(data = resampledData
                 , dependent.variable.name = responseVarName
                 )
            , arguments_local
            )
        )
    }
    return(list(model[["variable.importance"]], model[["prediction.error"]]))
  }

  # All topvars for first iteration
  topVarsList        <- vector("list", length = length(sizes))
  names(topVarsList) <- as.character(sizes)
  topVarsList[[as.character(sizes[1])]] <-
    data.frame(variable = setdiff(colnames(dataset), responseVarName)
               , value  = 1
               )

  oobErrorsList        <- numeric(length = length(sizes))
  names(oobErrorsList) <- as.character(sizes)

  # loop over sizes ----
  for(asizeIndex in 1:length(sizes)){

    set.seed(seed)
    seeds <- sample.int(1e6, nsamples[asizeIndex])

    # choose only the required columns
    removeVars <-
      setdiff(predictorNames
              , topVarsList[[as.character(sizes[max(1, asizeIndex - 1)])]][["variable"]][1:sizes[asizeIndex]]
              )

    if(length(removeVars) > 0){
      suppressWarnings(dataset[,  c(removeVars) := NULL])
    }

    imps      <- vector("list", nsamples[asizeIndex])
    oobErrors <- numeric(length = nsamples[asizeIndex])

    # compute importance over bootstraps
    for(i in 1:(nsamples[asizeIndex])){
      set.seed(seeds[[i]])
      extracted <- extractImp(sample.int(nr, floor(sampleprop[asizeIndex] * nr))
                              , asizeIndex
                              )
      imps[[i]] <- extracted[[1]]
      oobErrors <- extracted[[2]]
    }

    # get overall importance
    imps <- lapply(imps, function(x) data.frame(variable = names(x), value = x))
    merger <- function(x, y){
      suppressWarnings(
        merge(x
              , y
              , by = "variable"
              , all = TRUE
              , incomparables = NA
              )
        )
    }
    # compute average of importances over bootstraps and create a dataframe
    varImp       <- Reduce(merger, imps)
    varImpSummed <- sort(matrixStats::rowMedians(as.matrix(varImp[, -1]), na.rm = TRUE)
                         , decreasing = TRUE
                         )
    topVars      <- data.frame(
      variable     = as.character(varImp[,1][order(varImpSummed ,decreasing = TRUE)])
      , importance = sort(varImpSummed, decreasing = TRUE)
      )

    topVarsList[[as.character(sizes[asizeIndex])]] <- topVars
    oobErrorsList[as.character(sizes[asizeIndex])] <- stats::median(oobErrors, na.rm = TRUE)
    message("size: "
            , sizes[asizeIndex]
            , " , "
            , "oobError: "
            , round(oobErrorsList[as.character(sizes[asizeIndex])], 2)
            )
  }

  # return ----
  result <- tibble::tibble(size         = as.integer(sizes)
                           , ooberror   = oobErrorsList
                           , varimp     = topVarsList
                           , sampleprop = sampleprop
                           , nsamples   = as.integer(nsamples)
                           )

  return(result)
}
