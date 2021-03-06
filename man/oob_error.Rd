% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{oob_error}
\alias{oob_error}
\title{Extract OOB error from a forest model}
\usage{
oob_error(model)
}
\arguments{
\item{model}{forest model of class: ranger, randomForest}
}
\value{
A numerical value with an attribute. See description.
}
\description{
Extract Out of box error from a forest model. Supports models
  of these classes: ranger, randomForestø. The result adds an attribute
  'measure' indicating what the result is measuring.
}
\examples{
temp <- ranger::ranger(Species ~., data = iris)
res <- oob_error(temp)
res
attr(res, "measure")

temp <- ranger::ranger(Sepal.Length ~., data = iris)
res <- oob_error(temp)
res
attr(res, "measure")

temp <- randomForest::randomForest(Species ~., data = iris)
res <- oob_error(temp)
res
attr(res, "measure")

temp <- randomForest::randomForest(Sepal.Length ~., data = iris)
res <- oob_error(temp)
res
attr(res, "measure")

}
