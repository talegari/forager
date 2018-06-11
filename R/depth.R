#' depth_from_randomForest
depth_from_ranger <- function(treelike){

  data.table::setDT(treelike)
  dropThese <- setdiff(colnames(treelike)
                       , c("nodeID", "leftChild", "rightChild")
                       )
  treelike[, c(dropThese) := NULL]
  melted    <- data.table::melt(treelike
                                , id.vars      = "nodeID"
                                , measure.vars = c("leftChild", "rightChild")
                                )
  edgeMat     <- as.matrix(melted[!is.na(value), c("nodeID", "value")]) + 1L
  treegraph   <- igraph::graph_from_edgelist(edgeMat)
  depths      <- igraph::distances(treegraph, v = 1, mode = "out")
  dim(depths) <- NULL
  return(depths)
}

#' depth_from_randomForest
depth_from_randomForest <- function(treelike){

  data.table::setDT(treelike)
  dropThese <- setdiff(colnames(treelike)
                       , c("left daughter", "right daughter")
                       )
  treelike[, c(dropThese) := NULL]
  treelike[, c("nodeID")  := 1:nrow(treelike)]
  data.table::setcolorder(treelike, c("nodeID", "left daughter", "right daughter"))
  data.table::setnames(treelike
                       , c("nodeID", "left daughter", "right daughter")
                       , c("nodeID", "leftChild", "rightChild")
                       )
  treelike[treelike == 0] <- NA_integer_
  melted    <- data.table::melt(treelike
                                , id.vars      = "nodeID"
                                , measure.vars = c("leftChild", "rightChild")
                                )
  edgeMat     <- as.matrix(melted[!is.na(value), c("nodeID", "value")])
  treegraph   <- igraph::graph_from_edgelist(edgeMat)
  depths      <- igraph::distances(treegraph, v = 1, mode = "out")
  dim(depths) <- NULL
  return(depths)
}
