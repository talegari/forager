#' @name dist
#' @title Distance matrix from unsupervised random forest.
#' @description Distance matrix from unsupervised random forest.
#' @param dataset A dataframe
#' @param method (string) See 'method' in synthetic_forest
#' @param prop (flag) See 'prop' in synthetic_forest
#' @param predictMethod See 'method' in forage
#' @export
dist <- function(dataset
                 , method        = "synthetic"
                 , prop          = TRUE
                 , predictMethod = "terminalNodes"
                 ){

  model <- synthetic_forest(dataset  = dataset
                            , method = method
                            , prop   = prop
                            )

  distObject <- forage(object = model
                       , what = "dissimilarity"
                       , method = predictMethod
                       , context = "observations"
                       )

  return( distObject )
}

#' @name proximity
#' @aliases similarity
#' @title Proximity matrix from unsupervised random forest.
#' @description Proximity matrix from unsupervised random forest.
#' @param dataset A dataframe
#' @param method (string) See 'method' in synthetic_forest
#' @param prop (flag) See 'prop' in synthetic_forest
#' @param predictMethod See 'method' in forage
#' @export
proximity <- function(dataset
                      , method        = "synthetic"
                      , predictMethod = "terminalNodes"
                      , prop          = TRUE
                      ){

  model <- synthetic_forest(dataset  = dataset
                            , method = method
                            , prop   = prop
                            )

  distObject <- forage(object = model
                       , what = "proximity"
                       , method = predictMethod
                       , context = "observations"
                       )

  return( distObject )
}

similarity <- proximity