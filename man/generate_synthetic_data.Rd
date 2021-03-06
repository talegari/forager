% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_synthetic_data.R
\name{generate_synthetic_data}
\alias{generate_synthetic_data}
\title{Generate synthetic data for random forest}
\usage{
generate_synthetic_data(dataset, prop, seed)
}
\arguments{
\item{dataset}{A dataframe}

\item{prop}{Random sampling of covariates (when prop = TRUE) to generate
synthetic data. Else, uniform sampling is used.}

\item{seed}{Seed for sampling.}
}
\value{
A dataframe with synthetic data.
}
\description{
Unsupervised learning of randomforest as suggested by Brieman
  (https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#unsup)
  involves creating synthetic data by sampling randomly from unvariate
  distributions of each covariate(feature). This supports two methods: First,
  where proportions or distribition is taken into account when sampling at
  random, second where the data is sampled assuming uniform distribution. The
  former corresponds to "Addcl1" from Horvath's paper (Unsupervised Learning
  With Random Forest Predictors: Tao Shi & Steve Horvath) and latter
  corresponds to "addc2".
}
