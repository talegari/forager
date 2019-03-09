# treewise predictions (nobs X ntree matrix)  or (nobs X nclass X ntree) ----
predictions <- function(model, data, type = "response"){
  UseMethod("predictions")
}

predictions.ranger <- function(model, data, type = "response"){

  # assertions
  modelTypes <- c("classification", "regression", "probability estimation")
  modelType  <- forest_type(model)
  assertthat::assert_that(modelType %in% modelTypes)
  assertthat::assert_that(type %in% c("response", "probability"))
  if(modelType != "probability estimation" && type == "probability"){
    stop("If type is 'probability', then the ranger model has to be a probability forest. This can be built by setting 'probability = TRUE' in ranger.")
  }

  # predictions
  #   - classification: nobs X ntree matrix
  #   - probability estimation: nobs X nclass X ntree array
  pred <- predict(model, data, predict.all = TRUE)[["predictions"]]

  # create a response matrix from array when type is response
  if(modelType == "probability estimation" && type == "response"){
    pred <- apply(pred, 3, function(x) apply(x, 1, which.max))
  }

  # attach levels for non-regression model types
  if(modelType != "regression"){
    attr(pred, "levels") <- model[["forest"]][["levels"]]
  }

  return(pred)
}

predictions.randomForest <- function(model, data, type = "response"){

  modelTypes <- c("classification", "regression")
  modelType  <- forest_type(model)
  assertthat::assert_that(modelType %in% modelTypes)
  if(type != "response"){
    stop("randomForest model only supports 'response' type")
  }

  # pred is nobs X ntree matrix
  pred <- predict(model
                  , data
                  , predict.all = TRUE
                  , type        = "response" # this is imp unlike ranger
                  )[["individual"]]

  if(model[["type"]] == "classification"){
    dp        <- dim(pred)
    pred      <- factor(pred, levels = model[["classes"]]) %>% unclass()
    dim(pred) <- dp
  }

  return(pred)
}

# forest_type ----
forest_type <- function(model){
  UseMethod("forest_type")
}

forest_type.ranger <- function(model){
  model[["treetype"]] %>%
    tolower()
}

forest_type.randomForest <- function(model){
  model[["type"]] %>%
    tolower()
}

# oob_error ----

#' @name ooberror.ranger
#' @title Extract OOB error from a ranger model
#' @description Extract OOB error from a ranger model
#' @param model forest model of class: ranger
#' @export
oob_error.ranger <- function(model){
  res                  <- model[["prediction.error"]]
  attr(res, "measure") <- switch(forest_type(model)
                              , classification = "fraction of misclassified samples"
                              , regression     = "root mean squared error"
                              , survival       = "one minus Harrell's C-index"
                              )


  return(res)
}

#' @name ooberror.randomForest
#' @title Extract OOB error from a randomForest model
#' @description Extract OOB error from a randomForest model
#' @param model forest model of class: randomForest
#' @export
oob_error.randomForest <- function(model){
  res <- switch(forest_type(model)
                , classification = mean(model[["err.rate"]][,1], na.rm = TRUE)
                , regression     = sqrt(mean(model[["mse"]]))
                )
  attr(res, "measure") <- switch(forest_type(model)
                                 , classification = "fraction of misclassified samples"
                                 , regression     = "root mean squared error"
                                 )

  return(res)
}

#' @name oob_error
#' @title Extract OOB error from a forest model
#' @description Extract Out of box error from a forest model. Supports models
#'   of these classes: ranger, randomForestø. The result adds an attribute
#'   'measure' indicating what the result is measuring.
#' @param model forest model of class: ranger, randomForest
#' @return  A numerical value with an attribute. See description.
#' @examples
#' temp <- ranger::ranger(Species ~., data = iris)
#' res <- oob_error(temp)
#' res
#' attr(res, "measure")
#'
#' temp <- ranger::ranger(Sepal.Length ~., data = iris)
#' res <- oob_error(temp)
#' res
#' attr(res, "measure")
#'
#' temp <- randomForest::randomForest(Species ~., data = iris)
#' res <- oob_error(temp)
#' res
#' attr(res, "measure")
#'
#' temp <- randomForest::randomForest(Sepal.Length ~., data = iris)
#' res <- oob_error(temp)
#' res
#' attr(res, "measure")
#'
#' @export
oob_error <- function(model){
  UseMethod("oob_error")
}
