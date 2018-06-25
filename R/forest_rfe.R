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
#' @return A list with:
#'
#' \itemize{
#'
#'   \item (rfeTable) A tibble with three columns:
#'
#'   \itemize{
#'
#'     \item size: Number of variables used \item ooberror: Out-of-box error of
#'   the forest
#'
#'     \item varimp: A list-column where each item is a data.frame with
#'   variable names and importance
#'
#'     }
#'
#'  \item (oobchangeTable) A dataframe with five columns sorted by absolute value of the variable 'oepc'.
#'
#'  \itemize{
#'
#'    \item variable: Name of the variable that got removed at some stage
#'
#'    \item size: Number of variables that were considered before removing the variable
#'
#'    \item reducedSize: Number of the variables at next stage. Gives an idea of how many variables were reduced at that stage.
#'
#'    \item oepc: OOB error percentage change
#'
#'    \item importance: Importance of the variable at the stage when the variable was decided to be removed.
#'
#'   }
#' }
#' @examples
#' temp <- forest_rfe(iris, "Species")
#' temp
#'
#' temp <- forest_rfe(iris
#'                    , "Species"
#'                    , sizes = c(4,2)
#'                    , sampleprop = c(0.2, 0.3)
#'                    , nsamples = c(20, 30)
#'                    )
#' temp
#'
#' temp <- forest_rfe(iris
#'                    , "Species"
#'                    , sizes = c(4,2)
#'                    , sampleprop = 0.1
#'                    , nsamples = c(20, 30)
#'                    )
#' temp
#'
#' temp <- forest_rfe(iris
#'                    , "Species"
#'                    , sizes = c(4,2)
#'                    , sampleprop = c(0.2, 0.3)
#'                    , nsamples = 10
#'                    )
#' temp
#'
#' temp <- forest_rfe(iris
#'                    , "Species"
#'                    , sizes = c(4,2)
#'                    , sampleprop = c(0.2, 0.3)
#'                    , nsamples = 10
#'                    , mtry = list(3, 2)
#'                    , num.trees = list(500, 1000)
#'                    , case.weights = replicate(2, runif(150), simplify = FALSE)
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
    assertthat::assert_that(all(sizes <= (nc - 1)))
    assertthat::assert_that((nc - 1) %in% sizes)
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
    sizes <- unique(ceiling(sapply(0:floor(log(nc - 1, 2)), function(x) (nc - 1)/2^x)))
    assertthat::assert_that(dplyr::between(sampleprop, 1e-8, 1))
    assertthat::assert_that(assertthat::assert_that(assertthat::is.count(nsamples)))
    sampleprop <- rep(sampleprop, length(sizes))
    nsamples   <- rep(nsamples, length(sizes))
  }

  arguments      <-  list(...)
  if(length(arguments) > 0){
    assertthat::assert_that(
    all(sapply(arguments, function(x) inherits(x, "list")))
    )
  arguments <- lapply(arguments, function(x) rep_len(x, length(sizes)))
  }

  assertthat::assert_that(assertthat::assert_that(assertthat::is.count(seed)))

  # setup ----
  nr      <- nrow(dataset)
  dataset <- data.table::copy(dataset)
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
  rfeTable <- tibble::tibble(size         = as.integer(sizes)
                             , ooberror   = oobErrorsList
                             , varimp     = topVarsList
                             , sampleprop = sampleprop
                             , nsamples   = as.integer(nsamples)
                             )

  rfeTableu <- rfeTable[nrow(rfeTable):1, ]

  varRemoved <- function(df1, df2){
    setdiff(df1[["variable"]], df2[["variable"]])
  }

  computeOobErrorChange <- function(i){
    variables <- varRemoved(rfeTableu[["varimp"]][[i + 1]], rfeTableu[["varimp"]][[i]])
    variable  <- NULL
    data.frame(
      variable      = variables
      , size        = rep_len(rfeTableu[["size"]][(i + 1)], length(variables))
      , reducedSize = rep_len(rfeTableu[["size"]][i], length(variables))
      , oepc        = (rfeTableu[["ooberror"]][i] - rfeTableu[["ooberror"]][i + 1]) %>%
                        magrittr::divide_by((rfeTableu[["ooberror"]][i + 1] + 1e-8))
      , importance  = subset(rfeTableu[["varimp"]][[i + 1]], variable %in% variables)[["importance"]]
               )
  }

  oobchangeTable <- data.table::rbindlist(lapply(1:(nrow(rfeTable) - 1)
                                                 , computeOobErrorChange
                                                 )
                                          )

  return(list(rfeTable = rfeTableu
              , oobchangeTable = oobchangeTable[order(abs(oobchangeTable[["oepc"]])
                                                      , oobchangeTable[["importance"]]
                                                      , decreasing = TRUE
                                                      )
                                                , ]
              )
         )
}
