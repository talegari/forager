# TODO: documentation and tests

# margin of a ranger model
forest_margin <- function(model
                          , data
                          , responseVariableName
                          , type = "observation"
                          ){

  # margin: For an observation, diff between the proportion of trees that vote
  #         for the true label and max(proportion of trees that vote for a false
  #         label) where max is computed over all the false labels.
  #
  #         For a tree, margin is defined as the average margin (observations).
  #         For an observation, margin is the proportion of observations of
  #         correct class in the terminal node minus proportion of observations
  #         of best wrong class in the terminal node.

  # assertions ----
  modelType  <- forest_type(model)
  modelTypes <- c("classification", "probability estimation")
  assertthat::assert_that(modelType %in% modelTypes)
  assertthat::assert_that(type %in% c("observation", "tree"))

  if(modelType == "classification" && type == "tree"){
    stop("If type is 'tree', then the ranger model has to be a probability forest. This can be built by setting 'probability = TRUE' in ranger.")
  }

  # array of predicted labels ----
  pred <- predictions(model
                      , data
                      , switch(type
                               , tree        = "probability"
                               , observation = "response"
                               )
                      )

  y  <- data[[responseVariableName]]
  yu <- unclass(y)

  # compute margin ----
  if(type == "observation"){

    nTree     <- ncol(pred)
    nObs      <- nrow(pred)
    nDistinct <- length(levels(y))
    yuIter    <- iterators::iter(yu)

    res <- future.apply::future_apply(
      pred
      , 1
      , function(obs){

          # proportions of the levels in y
          vec <- obs %>%
            factor(levels = 1:nDistinct) %>%
            table() %>%
            as.integer() %>%
            magrittr::divide_by(nTree)

          trueVal <- iterators::nextElem(yuIter)

          # diff in prop of true label versus the max prob false label
          vec[trueVal] - max(vec[-trueVal])
        }
    )
  } else {

    # average margin per tree
    margin_tree <- function(mat){

      # for each obs
      mar <- function(row, index){

        prob_actual <- row[yu[index]]
        prob_other  <- max(row[-yu[index]])
        prob_diff   <- prob_actual - prob_other

        return(prob_diff)
      }

      # apply over observations and average
      matIter    <- iterators::iapply(mat, 1)
      obsMargins <- sapply(1:nrow(mat)
                           , function(rn) mar(iterators::nextElem(matIter), rn)
                           )

      return(obsMargins)
    }

    res <- future.apply::future_apply(pred, 3, margin_tree)

  }
  # return ----
  return(res)
}

# strength of the ranger model
forest_strength <- function(model, data, responseVariableName){

  # strength: average value of margins over observations
  return(mean(forest_margin(model, data, responseVariableName)))
}

# correlation of a ranger model
forest_correlation <- function(model, data, responseVariableName){

  # 1. vector: If predictions from a tree matches actual y, assign 1 , else -1
  # 2. Correlation for a tree pair: Pearson correlation between vectors respectively
  # 3. Average correlation among all tree pairs (n choose 2)

  assertthat::assert_that(forest_type(model) == "classification")

  # nObs times nTree matrix of predicted labels
  pred <- predictions(model, data)

  nTree <- ncol(pred)
  nObs  <- nrow(pred)
  y     <- unclass(data[[responseVariableName]])

  # raw margin: get 1 and -1 matrix depending on the match
  rawMargin <- apply(pred, 2, function(x) x == y)
  rawMargin <- ifelse(rawMargin, 1L, -1L)

  # aggregate pairwise correlation between trees
  cors <- proxy::simil(rawMargin, method = "correlation", by_rows = FALSE)

  return(mean(cors))

}


# # test example
# data(attrition, package = "rsample")
#
# model_ranger <- ranger::ranger(Attrition ~ .
#                                , data = attrition
#                                , seed = 1
#                                )
# model_ranger$prediction.error # OOB error are conservative comprated to gen error
#
# model_rf <- randomForest::randomForest(Attrition ~ .
#                                        , data = attrition
#                                        , seed = 1
#                                        )
#
# forest_margin(model_ranger, attrition, "Attrition")   # margin
# forest_strength(model_ranger, attrition, "Attrition") # strength
# forest_correlation(model_ranger, attrition, "Attrition")  # correlation
#
# forest_margin(model_rf, attrition, "Attrition")   # margin
# forest_strength(model_rf, attrition, "Attrition") # strength
# forest_correlation(model_rf, attrition, "Attrition")  # correlation
