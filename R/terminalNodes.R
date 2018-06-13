# terminalNodes workhorses

#' @name predict_terminalNodesMatrix_terminalNodes
#' @title predict_terminalNodesMatrix_terminalNodes
#' @description predict_terminalNodesMatrix_terminalNodes
#' @param object object
#' @param newdata newdata
predict_terminalNodesMatrix_terminalNodes <- function(object, newdata){

  if(inherits(object, "ranger")){
    predObject          <- stats::predict(object    = object
                                          , data    = newdata
                                          , type    = "terminalNodes"
                                          , verbose = FALSE
                                          )
    terminalNodesMatrix <- predObject[["predictions"]]
  }

  if(inherits(object, "randomForest")){
    predictedRandomForest <- stats::predict(object
                                            , newdata = newdata
                                            , type    = "response"
                                            , nodes   = TRUE
                                            )
    terminalNodesMatrix   <- attr(predictedRandomForest, "nodes")
  }

  return( terminalNodesMatrix )
}

#' @name predict_dissimilarity_observations_terminalNodes
#' @title predict_dissimilarity_observations_terminalNodes
#' @description predict_dissimilarity_observations_terminalNodes
#' @param object object
#' @param newdata newdata
predict_dissimilarity_observations_terminalNodes <- function(object, newdata){

  terminalNodesMatrix <- predict_terminalNodesMatrix_terminalNodes(object, newdata)
  distObject          <- proxy::dist(terminalNodesMatrix
                                     , method = function(x, y) sqrt(1 - mean(x == y))
                                     )

  return( distObject )
}

#' @name predict_proximity_observations_terminalNodes
#' @title predict_proximity_observations_terminalNodes
#' @description predict_proximity_observations_terminalNodes
#' @param object object
#' @param newdata newdata
predict_proximity_observations_terminalNodes <- function(object, newdata){

  terminalNodesMatrix <- predict_terminalNodesMatrix_terminalNodes(object, newdata)
  similObject         <- proxy::simil(terminalNodesMatrix
                                      , method = function(x, y) mean(x == y)
                                      )

  return( similObject )
}

#' @name predict_proximity_trees_terminalNodes
#' @title predict_proximity_trees_terminalNodes
#' @description predict_proximity_trees_terminalNodes
#' @param object object
#' @param newdata newdata
predict_proximity_trees_terminalNodes <- function(object, newdata){

  terminalNodesMatrix <- predict_terminalNodesMatrix_terminalNodes(object, newdata)
  similObject         <-
    proxy::simil(terminalNodesMatrix
                , method = function(x, y) fossil::rand.index(x, y)
                , by_rows = FALSE
                )

  return( similObject )
}

#' @name predict_dissimilarity_trees_terminalNodes
#' @title predict_dissimilarity_trees_terminalNodes
#' @description predict_dissimilarity_trees_terminalNodes
#' @param object object
#' @param newdata newdata
predict_dissimilarity_trees_terminalNodes <- function(object, newdata){

  terminalNodesMatrix <- predict_terminalNodesMatrix_terminalNodes(object, newdata)
  distObject         <- proxy::dist(
    terminalNodesMatrix
    , method = function(x, y) 1 - fossil::rand.index(x, y)
    , by_rows = FALSE
    )

  return( distObject )
}

#' @name predict_outlyingness_observations_terminalNodes
#' @title predict_outlyingness_observations_terminalNodes
#' @description predict_outlyingness_observations_terminalNodes
#' @param object object
#' @param newdata newdata
#' @param classes (a factor) classes
predict_outlyingness_observations_terminalNodes <- function(object
                                                            , newdata
                                                            , classes
                                                            ){

  similObject <- predict_proximity_observations_terminalNodes(object, newdata)
  subset_dist <- utils::getFromNamespace("subset.dist", "proxy")

  classwiseOut <- function(aClass){
    classIndex <- which(classes == aClass)
    classSimil <- (as.matrix(subset_dist(similObject, classIndex)))^2
    return( data.table::data.table(index = classIndex
                                   , outlyingness = 1/colSums(classSimil)
                                   )
            )
  }

  outlyingness <- data.table::rbindlist(lapply(levels(classes), classwiseOut))
  data.table::setDT(outlyingness)
  index <- NULL
  outlyingness <- outlyingness[order(index), ]

  return(outlyingness[["outlyingness"]])
}
