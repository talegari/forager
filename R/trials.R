# model_ranger <- synthetic_forest(iris[,1:4], implementation = "ranger")
# summary(model_ranger)
# model_ranger$prediction.error # OOB prediction error
#
# model_randomForest <- synthetic_forest(iris, implementation = "randomForest")
# summary(model_randomForest)
# mean(model_randomForest$err.rate[,1]) # OOB prediction error
#
# tn <- forage(model_ranger, newdata = iris, what = "terminalNodesMatrix")
# tn2 <- forage(model_randomForest, newdata = iris, what = "terminalNodesMatrix")
#
# di  <- forage(model_ranger, newdata = iris[,1:4], what = "dissimilarity")
# di2 <- forage(model_randomForest, newdata = iris, what = "dissimilarity")
#
# to <- Rtsne::Rtsne(di, is_distance = TRUE)
# to$Y %>% as.data.frame() %>%
#   ggplot2::ggplot(ggplot2::aes(V1, V2, color = iris$Species)) +
#   ggplot2::geom_point()
#
# hclust(di, method = "average") %>% cutree(k = 3) %>% table(unclass(iris$Species))
# hclust(di2, method = "average") %>% cutree(k = 3) %>% table(unclass(iris$Species))
#
# pr  <- forage(model_ranger, newdata = iris[,1:4], what = "proximity")
# pr2 <- forage(model_randomForest, newdata = iris, what = "proximity")
#
# iris_model <- ranger::ranger(Species ~., data = iris, num.trees = 10)
# iris_model2 <- randomForest::randomForest(Species ~., data = iris)
# forage(iris_model, newdata = iris[,1:4], what = "proximity")
#
# out <- forage(iris_model, newdata = iris[, 1:4], what = "outlyingness", classes = iris$Species)
# out2 <- forage(iris_model2, newdata = iris[, 1:4], what = "outlyingness", classes = iris$Species)
# boxplot.stats(out) # look for 71 which is 5.78
#
# lab <- rep(NA, 150)
# lab[which(out2 %in% boxplot.stats(out2)$out)] <- 1
#
# to$Y %>% as.data.frame() %>%
#   dplyr::mutate(label = lab) %>%
#   ggplot2::ggplot(ggplot2::aes(V1, V2, color = iris$Species)) +
#   ggplot2::geom_point() +
#   ggplot2::geom_label(ggplot2::aes(label = label))
#
# de <- forage(model_ranger, newdata = iris[, 1:4], what = "depth")
# de2 <- forage(iris_model2, newdata = iris[, 1:4], what = "depth")
#
# dt <- forage(iris_model, newdata = iris, what = "dissimilarity", context = "trees")
# dt %>% attr("Size")
# dt
#
# hclust(dt) %>% plot()
# to <- Rtsne::Rtsne(dt, is_distance = TRUE, perplexity = 3)
# to$Y %>% as.data.frame() %>%
#   ggplot2::ggplot(ggplot2::aes(V1, V2)) +
#   ggplot2::geom_point()
#
# iris_with_na <- missRanger::generateNA(iris, 0.2)
# iris_complete <- randomForest::na.roughfix(iris_with_na)
# iris_missing <- is.na(iris_with_na) %>% as.data.frame()
#
# imp1 <- forest_impute(list(iris_complete, iris_missing))
# imp1
#
# Map(metric_relative, iris_complete, iris, iris_missing) %>% unlist()
# Map(metric_relative, imp1$data, iris, iris_missing) %>% unlist()
#
# imp2 <- forest_impute(list(iris_complete, iris_missing), object = model_ranger)
# Map(metric_relative, imp2$data, iris, iris_missing) %>% unlist()
