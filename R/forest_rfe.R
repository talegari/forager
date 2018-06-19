#' @name forest_rfe
#' @title lightweight implementation of RFE using ranger random forest implementation
#' @description Modifications to regular recursive feature elimination procedure:
#' \itemize{
#'
#' \item Use oob prediction error as a proxy to model performance
#'
#' \item Build forests on samples of data and average them
#'
#' }
#' @param dataset (object inheriting data.frame class) A dataframe
#' @param responseVarName (string) Name of the response variable
#' @param sizes (integer vector) Vector of number of variables
#' @param sampleprop (real number between 0 and 1) Proportion of observations per sample
#' @param nsamples (positive integer) Number of samples
#' @param seed (positive integer) Seed
#' @param ... Arguments to be passed to `ranger::ranger`
#' @return A tibble with three columns:
#' \itemize{
#'
#' \item size: Number of variables used
#' \item ooberror: Out-of-box error of the forest
#' \item varimp: A list-column where each item is a data.frame with variable names and importance
#'
#' }
#' @examples
#' temp <- forest_rfe(iris, "Species")
#' temp
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
    sizes <- sort(unique(sizes), decreasing = TRUE)
    assertthat::assert_that(all(sizes <= nc))
    if(sizes[1] != nc){
      sizes <- c(nc, sizes)
    }
  } else {
    sizes <- unique(ceiling(sapply(0:floor(log(nc, 2)), function(x) nc/2^x)))
  }

  assertthat::assert_that(dplyr::between(sampleprop, 1e-8, 1))
  assertthat::assert_that(assertthat::assert_that(assertthat::is.count(nsamples)))
  assertthat::assert_that(assertthat::assert_that(assertthat::is.count(seed)))

  # setup ----
  arguments      <-  list(...)
  nr             <- nrow(dataset)
  data.table::setDT(dataset)
  predictorNames <- setdiff(colnames(dataset), responseVarName)
  if(is.null(arguments[["importance"]])){
    arguments[["importance"]] <- "permutation"
  }
  set.seed(seed)
  seeds <- sample.int(1e6, nsamples)

  # given a resample index, extractImp return the vector of variable importance and oobError
  extractImp <- function(resampleIndex){

    resampledData <- dataset[resampleIndex, ]
    if(!is.null(arguments[["case.weights"]])){
      resampledCaseWeights <- arguments[["case.weights"]][resampleIndex]
      model <- do.call(
        ranger::ranger
        , c(list(data = dataset
                 , dependent.variable.name = responseVarName
                 , resampledCaseWeights
                 )
            , arguments
            )
        )
    } else {
      model <- do.call(
        ranger::ranger
        , c(list(data = dataset
                 , dependent.variable.name = responseVarName
                 )
            , arguments
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

  oobErrorsList       <- numeric(length = length(sizes))
  names(oobErrorsList) <- as.character(sizes)

  # loop over sizes ----
  for(asizeIndex in 1:length(sizes)){

    # choose only the required columns
    removeVars <-
      setdiff(predictorNames
              , topVarsList[[as.character(sizes[max(1, asizeIndex - 1)])]][["variable"]][1:sizes[asizeIndex]]
              )

    if(length(removeVars) > 0){
      suppressWarnings(dataset[,  c(removeVars) := NULL])
    }

    imps      <- vector("list", nsamples)
    oobErrors <- numeric(length = nsamples)

    # compute importance over bootstraps
    for(i in 1:nsamples){
      set.seed(seeds[[i]])
      extracted <- extractImp(sample.int(nr, floor(sampleprop * nr)))
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
  result <- tibble::tibble(size       = as.integer(sizes)
                           , ooberror = oobErrorsList
                           , varimp   = topVarsList
                           )

  return(result)
}
