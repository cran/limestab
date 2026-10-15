\name{bayesian_optimisation}
\alias{bayesian_optimisation}
\title{Bayesian optimisation of a scalar objective}
\description{Bayesian optimisation of a scalar objective.}
\usage{
bayesian_optimisation(n_iters,
  sample_loss,
  bounds,
  n_pre_samples = 5,
  random_state = 42,
  verbose = FALSE)
}
\arguments{
  \item{n_iters}{Number of Bayesian-optimisation iterations after the initial design.}
  \item{sample_loss}{Function of one scalar returning the value to maximise.}
  \item{bounds}{c(lower, upper) search bounds.}
  \item{n_pre_samples}{Number of random initial evaluations.}
  \item{random_state}{Seed for the whole procedure.}
  \item{verbose}{If TRUE, print progress.}
}
\value{A list with x_sample and y_sample, every point evaluated.}
\keyword{models}
