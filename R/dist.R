#' @name dist
#' @title Dsitance matrix from unsupervised random forest.
#' @export
dissimilarity <- function(dataset
                          , method        = "synthetic"
                          , predictMethod = "terminalNodes"
                          , prop          = TRUE
                          ){

  model <- suppressMessages(
    unsupervised(dataset  = dataset
                 , method = method
                 , prop   = prop
                 )
    )
  distObject <- stats::predict(model
                              , newdata = dataset
                              , what    = "dissimilarity"
                              , method  = predictMethod
                              )

  return( distObject )
}

#' @name proximity
#' @title Proximity matrix from unsupervised random forest.
#' @export
proximity <- function(dataset
                      , method        = "synthetic"
                      , predictMethod = "terminalNodes"
                      , prop          = TRUE
                      ){

  model <- suppressMessages(
    unsupervised(dataset      = dataset
                 , method     = method
                 , prop       = prop
                 )
    )
  similObject <- stats::predict(model
                                , newdata = dataset
                                , what    = "proximity"
                                , method  = predictMethod
                                )

  return( similObject )
}

similarity <- proximity