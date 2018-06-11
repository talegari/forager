# plan ----
# 1. unsupervised_synthetic does the synthetic data creation and growing a random forest.
# 2.


# # dist_ranger ----
# # randomly split iris data into two parts: smaller (50) and larger (100) rows
# set.seed(1)
# index <- sample(150)
# iris1 <- iris[index[1:50], ]
# iris2 <- iris[index[51:150], ]
#
# # create a randomforest model with smaller dataset
# set.seed(2)
# rfModel <- ranger::ranger(Species ~., data = iris1)
# rfModel # around 8% OOB error
#
# # compute distance matrix for larger dataset using the model
# distMat <- predictDist(rfModel, iris2)
# class(distMat)
#
# distMatHclust <- hclust(distMat)
# plot(distMatHclust) # no surprises about three main clusters
#
# # observe the clusters and adjusted rand index with the actual labels
# distMatClusters <- cutree(distMatHclust, k = 3)
# mclust::adjustedRandIndex(unclass(iris2$Species), distMatClusters) # > 0.9

# temp <- unsupervised(dataset = iris[,1:4])
# temp <- unsupervised(dataset = iris, min.node.size = 10)
# dis  <- predict(temp
#                 , newdata = iris
#                 , what = "dissimilarity"
#                 , method = "terminalNodes"
#                 )
# sim  <- predict(temp
#                 , newdata = iris
#                 , what = "proximity"
#                 , method = "terminalNodes"
#                 )
# ter  <- predict(temp
#                 , newdata = iris
#                 , what = "terminalNodesMatrix"
#                 , method = "terminalNodes"
#                 )
# out  <- predict(temp
#                 , newdata = iris
#                 , what = "outlyingness"
#                 , method = "terminalNodes"
#                 , classes = iris$Species
#                 )
#
# dtemp <- dist(iris[,1:4])
#
# temp <- dist(dataset = MASS::Boston)

# set.seed(1)
# iris_incomplete <- missRanger::generateNA(iris, 0.1)
#
# iris_complete           <- randomForest::na.roughfix(iris_incomplete)
# iris_incomplete_boolean <- as.data.frame(is.na(iris_incomplete))
#
# temp <- rf_impute(list(iris_complete, iris_incomplete_boolean))
# temp <- rf_impute(list(iris_complete, iris_incomplete_boolean)
#                   , object = randomForest::randomForest(Species ~ ., data = iris)
#                   )
# temp
