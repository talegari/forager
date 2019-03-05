# terminalNodes workhorses

terminalNodes <- function(model, data){

  UseMethod("terminalNodes")

}

terminalNodes.ranger <- function(model, data){

  predmodel <- stats::predict(model    = model
                              , data    = data
                              , type    = "terminalNodes"
                              , verbose = FALSE
                              )

  return(predmodel[["predictions"]])
}

terminalNodes.randomForest <- function(model, data){

  predmodel <- stats::predict(model
                              , data = data
                              , type    = "response"
                              , nodes   = TRUE
                              )

  return(attr(predictedRandomForest, "nodes"))
}


dist_terminalNodes <- function(model, data){

  parallelDist::parDist(terminalNodes(model, data)
                        , method = "hamming"
                        )
}

proximity_terminalNodes <- function(model, data){

  parallelDist::parDist(terminalNodes(model, data)
                        , method = "hamming"
                        ) %>%
    proxy::as.simil()
}

knn_terminalNodes <- function(model, data, k){

  dbscan::kNN(terminalNodes(model, data)
              , k = k
              , )

}


predict_outlyingness_observations_terminalNodes <- function(model
                                                            , data
                                                            , classes
                                                            ){

  similmodel <- predict_proximity_observations_terminalNodes(model, data)
  subset_dist <- utils::getFromNamespace("subset.dist", "proxy")

  classwiseOut <- function(aClass){
    classIndex <- which(classes == aClass)
    classSimil <- (as.matrix(subset_dist(similmodel, classIndex)))^2
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
