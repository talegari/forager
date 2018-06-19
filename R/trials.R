# # example of unsupervised imputation
#
# # create 20% artificial missings values at random
# iris_with_na  <- missRanger::generateNA(iris, 0.4, seed = 1)
# # impute with mean/mode
# iris_complete <- randomForest::na.roughfix(iris_with_na)
# # dataframe of missing positions
# iris_missing  <- is.na(iris_with_na) %>% as.data.frame()
#
# system.time(
#   imp1        <- forest_impute(list(iris_complete, iris_missing)
#                                , implementation = "randomForest"
#                                )
#   )
# imp1$iter # number of iterations
# imp1$errors # errors of the last iteration
#
# compare_roughimpute_with_actual <- Map(metric_relative, iris_complete, iris, iris_missing) %>% unlist()
# compare_forest_impute_with_actual <- Map(metric_relative, imp1$data, iris, iris_missing) %>% unlist()
#
# perf <- data.frame(
#   colnames = names(compare_forest_impute_with_actual)
#   , rough  = round(compare_roughimpute_with_actual, 2)
#   , forest = round(compare_forest_impute_with_actual, 2)
#   )
# rownames(perf) <- NULL
# perf
#
# rfout <- randomForest::rfImpute(x = iris_with_na[,1:4], y = iris$Species, ntree = 1000)
# rfout <- data.frame(rfout[,2:5], Species = rfout[[1]])
# fout <- forest_impute(list(iris_complete, iris_missing)
#               , responseVarName = "Species"
#               , tol = 0
#               , nproc = 8
#               , implementation = "randomforest"
#               )
# Map(metric_relative, rfout, iris, iris_missing) %>% unlist()
# Map(metric_relative, fout$data, iris, iris_missing) %>% unlist()
#
# # example of supervised imputation
#
# # partiton iris data
# set.seed(1)
# index      <- sample.int(nrow(iris), floor(0.7 * nrow(iris)))
# iris_train <- iris[index, ]
# iris_test  <- iris[-index, ]
#
# # create 20% artificial missings values at random
# iris_test_with_na  <- missRanger::generateNA(iris_test, 0.2, seed = 1)
# # impute with mean/mode
# iris_test_complete <- randomForest::na.roughfix(iris_test_with_na)
# # dataframe of missing positions
# iris_test_missing  <- is.na(iris_test_with_na) %>% as.data.frame()
#
# model_ranger <- ranger::ranger(Species ~., data = iris_train)
# model_ranger$prediction.error # 5% error
# system.time(
#   imp2       <- forest_impute(list(iris_test_complete, iris_test_missing)
#                               , object = model_ranger
#                               )
#   )
# compare_forest_impute_sup_with_actual <-
#   Map(metric_relative, imp2$data, iris_test, iris_test_missing) %>% unlist()
#
# perf2 <- data.frame(
#   colnames     = names(compare_forest_impute_sup_with_actual)
#   , rough      = round(compare_roughimpute_with_actual, 2)
#   , forest_sup = round(compare_forest_impute_sup_with_actual, 2)
#   )
# rownames(perf2) <- NULL
# perf2
# cbind(perf, forest_sup = perf2[,3])
# # Note that some variables