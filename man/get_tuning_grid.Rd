% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hyperparameters.R
\name{get_tuning_grid}
\alias{get_tuning_grid}
\title{Generate the tuning grid for tuning hyperparameters}
\usage{
get_tuning_grid(hyperparams_list, method)
}
\arguments{
\item{hyperparams_list}{Named list of lists of hyperparameters.}

\item{method}{ML method.
Options: \code{c("glmnet", "rf", "rpart2", "svmRadial", "xgbTree")}.
\itemize{
\item glmnet: linear, logistic, or multiclass regression
\item rf: random forest
\item rpart2: decision tree
\item svmRadial: support vector machine
\item xgbTree: xgboost
}}
}
\value{
The tuning grid.
}
\description{
Generate the tuning grid for tuning hyperparameters
}
\examples{
ml_method <- "glmnet"
hparams_list <- get_hyperparams_list(otu_small, ml_method)
get_tuning_grid(hparams_list, ml_method)
}
\author{
Begüm Topçuoğlu, \email{topcuoglu.begum@gmail.com}

Kelly Sovacool, \email{sovacool@umich.edu}
}
