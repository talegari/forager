#' @name forager
#' @title Compute auxiliary information (proximity, dissimilarity, outlyingness, depth) and imputation from tree ensembles on new data
#' @description
#'
#' \pkg{unsupervised} package provides unsupervised random forest methods to
#' compute and \strong{predict} (on new data):
#'
#' \itemize{
#'
#' \item Dissimilarity and proximity matrices (useful in clustering observations
#' and visualizing the dissimilarities)
#'
#' \item Detect outliers (by outlyingness)
#'
#' \item Impute missing data }
#'
#' The package implements various unsupervised random forest methods:
#'
#' \itemize{
#'
#' \item \strong{synthetic}:
#'
#' \itemize{
#'
#' \item Create synthetic data by sampling each covariate.
#'
#' \item Classify actual versus synthetic data using a random forest model.
#'
#' \item Obtain proximity between observations by counting the number of times a
#' pair of observations occur together in a terminal node of a tree.
#'
#' }
#'
#' For details, see Tao shi (2006).
#'
#' The following outputs might be obtained from this method:
#'
#' \itemize{
#'
#' \item \strong{Distance or proximity matrix}: This might be used to
#' \strong{cluster} the observations using Hierarchical clustering,  PAM
#' (partitioning around mediods), DBScan and other clustering methods that work
#' with distance or dissimilarity matrices. Low dimensional embedding methods
#' like MDS (Multi-dimensional scaling), TSNE allow \strong{visualizing} the
#' dissimilarities. Quoting from Andy Liaw et al (2002): "The idea is that real
#' data points that are similar to one another will frequently end up in the
#' same terminal node of a tree, exactly what is measured by the proximity
#' matrix".
#'
#' \item \strong{Variable Importance}: The synthetic data \emph{destroys} the
#' relationship among the covariates. The random forest classifier tries to
#' distinguish the classes: actual and synthetic based on the covariates.
#' \emph{High} OOB Error is an indication of lack of relationship or interaction
#' among the covariates. When the OOB error is \emph{low} enough, a variable
#' importance measure would indicate a set of covariates with \emph{high}
#' interactions among some subsets of themselves.
#'
#' \item \strong{Outlyingness}: This measure of outlyingness for the jth
#' observation is calculated as the reciprocal of the sum of squared proximities
#' between that observation and all other observations in the same class (from
#' Andy Liaw et al (2002))
#'
#' \item \strong{Impute missing data}: At the first step, the each column is
#' imputed by its median/mode value and a proximity matrix of the previous step
#' is used in the further steps to estimate the missing value where the
#' proximities are used in the weighted average. These iterations are continued
#' until values do not change beyond a threshold or until some maximum
#' iterations are reached.
#'
#' When the imputation is run in 'predict' mode, the random forest model built
#' during train is utilized to estimate proximities.
#'
#' }
#'
#' }
#'
#'
#' The \pkg{randomForest} package also provides proximity matrices by running
#' unsupervised mode. The \pkg{unsupervised} package differs from the
#' implementation in \pkg{randomForest} package in these ways:
#'
#' \itemize{
#'
#' \item Random forest is computed using 'ranger' package.
#'
#' \item Provides the \strong{predict} method. This may be used to compute the
#' pairwise distances between observations of a new dataset, understand
#' outliers, impute by learning from the training dataset. When predict method
#' is "terminalNodes", the observations of the new data traverse through the
#' trees built by the randomforest on training dataset and pairwise distance is
#' computed by counting the number of times the pair land in the same terminal
#' node.
#'
#' \item The \strong{predict} method can be run with any ranger(trained by
#' \code{\link[ranger]{ranger}}) or randomForest(trained by
#' \code{\link[randomForest]{randomForest}}) model
#'
#' }
#'
#' @references
#'
#' \itemize{
#'
#' \item \emph{Unsupervised Learning With Random Forest Predictors} by Tao Shi &
#' Steve Horvath
#' \href{https://doi.org/10.1198/106186006X94072}{<doi:10.1198/106186006X94072>}
#' (2016)
#'
#' \item \emph{Classification and Regression by randomForest
#' (\href{https://www.r-project.org/doc/Rnews/Rnews_2002-3.pdf}{R News, Vol.
#' 2/3, December 2002, page 18})} by Andy Liaw and Matthew Wiener
#'
#' }
#'
#' @importFrom data.table :=
#' @importFrom magrittr %>%
"_PACKAGE"
