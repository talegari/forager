#' @name depth_from_ranger
#' @title depth_from_ranger
#' @description depth_from_ranger
#' @param treelike a treelike structure

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
  value       <- NULL
  edgeMat     <- as.matrix(melted[!is.na(value), c("nodeID", "value")]) + 1L
  treegraph   <- igraph::graph_from_edgelist(edgeMat)
  depths      <- igraph::distances(treegraph, v = 1, mode = "out")
  dim(depths) <- NULL
  return(depths)
}

#' @name depth_from_randomForest
#' @title depth_from_randomForest
#' @description depth_from_randomForest
#' @param treelike a treelike structure

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
  value       <- NULL
  edgeMat     <- as.matrix(melted[!is.na(value), c("nodeID", "value")])
  treegraph   <- igraph::graph_from_edgelist(edgeMat)
  depths      <- igraph::distances(treegraph, v = 1, mode = "out")
  dim(depths) <- NULL
  return(depths)
}

#' @name predict_depth.ranger
#' @title predict_depth.ranger
#' @description predict_depth.ranger
#' @param object A tree ensemble model
#' @param data data
#' @param nproc number of cores to use for parallel processing
predict_depth.ranger <- function(object
                                              , data
                                              , nproc = 1
                                              ){

  assertthat::assert_that(assertthat::is.count(nproc))
  tnm <- predict_terminalNodesMatrix_terminalNodes(object, data)

  nproc <- max(1, min(nproc, parallel::detectCores() - 1))
  if(.Platform$OS.type == "unix" && nproc > 1){
    outList     <- parallel::mclapply(
      1:ncol(tnm)
      , function(x) depth_from_ranger(ranger::treeInfo(object, x))[tnm[, x] + 1L]
      , mc.cores = nproc
      )
  } else {
    outList     <- lapply(
      1:ncol(tnm)
      , function(x) depth_from_ranger(ranger::treeInfo(object, x))[tnm[, x] + 1L]
      )
  }

  outMat      <- unlist(outList)
  dim(outMat) <- dim(tnm)
  return(outMat)
}

#' @name predict_depth.randomForest
#' @title predict_depth.randomForest
#' @description predict_depth.randomForest
#' @param object A tree ensemble model
#' @param data data
#' @param nproc number of cores to use for parallel processing
predict_depth.randomForest <- function(object
                                                    , data
                                                    , nproc = 1
                                                    ){

  assertthat::assert_that(assertthat::is.count(nproc))
  tnm <- predict_terminalNodesMatrix_terminalNodes(object, data)

  nproc <- max(1, min(nproc, parallel::detectCores() - 1))

  if(.Platform$OS.type == "unix" && nproc > 1){
    outList     <- parallel::mclapply(
      1:ncol(tnm)
      , function(x) depth_from_randomForest(randomForest::getTree(object, x, labelVar = TRUE))[tnm[, x]]
      , mc.cores = nproc
      )
  } else {
    outList     <- lapply(
      1:ncol(tnm)
      , function(x) depth_from_randomForest(randomForest::getTree(object, x, labelVar = TRUE))[tnm[, x]]
      )
  }

  outMat      <- unlist(outList)
  dim(outMat) <- dim(tnm)
  return(outMat)
}

#' @name predict_depth
#' @title predict_depth
#' @description predict_depth
#' @param object A tree ensemble model
#' @param data data
#' @param parallel (flag) Whether to use multicore parallel processing on
#'   unix-alike systems.
predict_depth <- function(object
                                       , data
                                       , parallel = TRUE
                                       ){
  UseMethod("predict_depth", object)
}