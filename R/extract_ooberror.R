#' @name extract_ooberror.ranger
#' @title Extract OOB error from a ranger object
#' @description Extract OOB error from a ranger object
#' @param object forest object of class: ranger
#' @export
extract_ooberror.ranger <- function(object){
  res                 <- object[["prediction.error"]]
  attr(res, "measure") <- switch(object[["treetype"]]
                              , classification = "fraction of misclassified samples"
                              , regression     = "root mean squared error"
                              , survival       = "one minus Harrell's C-index"
                              )


  return(res)
}

#' @name extract_ooberror.randomForest
#' @title Extract OOB error from a randomForest object
#' @description Extract OOB error from a randomForest object
#' @param object forest object of class: randomForest
#' @export
extract_ooberror.randomForest <- function(object){
  res <- switch(object[["type"]]
                , classification = mean(object[["err.rate"]][,1], na.rm = TRUE)
                , regression     = sqrt(mean(object[["mse"]]))
                )
  attr(res, "measure") <- switch(object[["type"]]
                                 , classification = "fraction of misclassified samples"
                                 , regression     = "root mean squared error"
                                 )

  return(res)
}

#' @name extract_ooberror
#' @title Extract OOB error from a forest object
#' @description Extract Out of box error from a forest object. Supports objects
#'   of these classes: ranger, randomForest. The result adds an attribute
#'   'measure' indicating what the result is measuring.
#' @param object forest object of class: ranger, randomForest
#' @return  A numerical value with an attribute. See description.
#' @examples
#' temp <- ranger::ranger(Species ~., data = iris)
#' res <- extract_ooberror(temp)
#' res
#' attr(res, "measure")
#'
#' temp <- ranger::ranger(Sepal.Length ~., data = iris)
#' res <- extract_ooberror(temp)
#' res
#' attr(res, "measure")
#'
#' temp <- randomForest::randomForest(Species ~., data = iris)
#' res <- extract_ooberror(temp)
#' res
#' attr(res, "measure")
#'
#' temp <- randomForest::randomForest(Sepal.Length ~., data = iris)
#' res <- extract_ooberror(temp)
#' res
#' attr(res, "measure")
#'
#' @export
extract_ooberror <- function(object){
  UseMethod("extract_ooberror")
}
