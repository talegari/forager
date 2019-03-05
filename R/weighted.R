# weighted rf based on proximity
# from sikonja's paper
predict_weighted <- function(model
                             , trainData
                             , testData
                             , k = 10
                             , responseVariableName
                             ){

  nObsTrain <- nrow(trainData)
  nObsTest  <- nrow(testData)
  nTree     <- model[["num.trees"]]

  allTreePredictions <- predictions(model
                                    , rbind(trainData, testData)
                                    , type = "response"
                                    )

  # compute pairwise hamming distance between train and test
  trainTestCrossDist <- proxy::dist(allTreePredictions[1:nObsTrain, ]
                                    , allTreePredictions[1:nObsTest, ]
                                    , method = function(x, y) sum(x != y)
                                    )

  # for each test instance, get k neighbors
  # TODO: this needs a generalization, frnn?
  neighborsIndex <- future.apply::future_apply(trainTestCrossDist
                                               , 2
                                               , function(x) order(x)[1:k]
                                               )

  # obtain treewise weights for each observation
  margins <- forest_margin(model, trainData, responseVariableName, type = "tree")

  # for which trees were these neighbors were oob
  # know which obs were not used by a tree: nobs X ntree boolean matrix
  obsTreeExclusion <- sapply(model[["inbag.counts"]], function(x) x == 0)

  # function to obtain tree weights for a test observation
  # input is a index of neighbors for a test obs: columns of neighborsIndex
  getTreeWeight <- function(index){

    # know for which trees these train obs are in oob (note the negation)
    treeBoolean <- !apply(obsTreeExclusion[index, ], 2, any)

    # compute average marin over selected trees
    weights              <- double(length = nTree)
    weights[treeBoolean] <- apply(margins[index, treeBoolean], 2, mean)

    return(weights)
  }

  # matrix of tree weights for each test observation: nObsTest X nTree matrix
  treeWeights <- future.apply::future_apply(neighborsIndex, 2, getTreeWeight) %>%
    t()

  # compute weighted predictions for test data
  predWeighted <- rowSums(allTreePredictions[-(1:nObsTrain), ] * treeWeights) %>%
    magrittr::divide_by(rowSums(treeWeights)) %>%
    round()

  # assign
  predWeightedLabels <- attr(allTreePredictions, "levels")[predWeighted]

  return(predWeightedLabels)
}
