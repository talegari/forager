#' @name generate_synthetic_data
#' @title Generate synthetic data for random forest
#' @description Unsupervised learning of randomforest as suggested by Brieman
#'   (https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#unsup)
#'   involves creating synthetic data by sampling randomly from unvariate
#'   distributions of each covariate(feature). This supports two methods: First,
#'   where proportions or distribition is taken into account when sampling at
#'   random, second where the data is sampled assuming uniform distribution. The
#'   former corresponds to "Addcl1" from Horvath's paper (Unsupervised Learning
#'   With Random Forest Predictors: Tao Shi & Steve Horvath) and latter
#'   corresponds to "addc2".
#' @param dataset A dataframe
#' @param prop Random sampling of covariates (when prop = TRUE) to generate
#'   synthetic data. Else, uniform sampling is used.
#' @param seed Seed for sampling.
#' @return A dataframe with synthetic data.

generate_synthetic_data <- function(dataset, prop, seed){

  set.seed(seed)
  seeds     <- sample.int(n = max(1e6, ncol(dataset)), size = ncol(dataset))
  synthetic <- Map(sampler, dataset, prop, seeds)
  data.table::setDT(synthetic)

  return(synthetic)
}
