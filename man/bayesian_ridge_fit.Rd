\name{bayesian_ridge_fit}
\alias{bayesian_ridge_fit}
\title{Bayesian ridge regression (Tipping 2001 / MacKay 1992)}
\description{Bayesian ridge regression (Tipping 2001 / MacKay 1992).}
\usage{
bayesian_ridge_fit(x,
  y,
  w,
  alpha_init = NULL,
  lambda_init = NULL,
  fit_alpha = TRUE,
  fit_lambda = TRUE,
  max_iter = 1000,
  tol = 1e-4,
  alpha_1 = 1e-6,
  alpha_2 = 1e-6,
  lambda_1 = 1e-6,
  lambda_2 = 1e-6,
  weighted_centering = FALSE)
}
\arguments{
  \item{x}{Numeric predictor matrix.}
  \item{y}{Numeric response vector.}
  \item{w}{Sample weights.}
  \item{alpha_init}{Initial (or fixed) noise precision; NULL uses 1 / var(y).}
  \item{lambda_init}{Initial (or fixed) weight precision; NULL uses 1.}
  \item{fit_alpha}{Whether the noise precision is re-estimated from data.}
  \item{fit_lambda}{Whether the weight precision is re-estimated from data.}
  \item{max_iter}{Maximum number of update iterations.}
  \item{tol}{Convergence tolerance on the coefficients.}
  \item{alpha_1}{Gamma-prior shape hyperparameter for alpha.}
  \item{alpha_2}{Gamma-prior rate hyperparameter for alpha.}
  \item{lambda_1}{Gamma-prior shape hyperparameter for lambda.}
  \item{lambda_2}{Gamma-prior rate hyperparameter for lambda.}
  \item{weighted_centering}{If TRUE, centre x/y with weighted means; if FALSE, use unweighted means.}
}
\value{A list with intercept, coef, sigma (posterior covariance), alpha and lambda.}
\keyword{models}
