\name{generate_perturbations}
\alias{generate_perturbations}
\title{Generate a perturbed neighbourhood in standardised feature space}
\description{Generate a perturbed neighbourhood in standardised feature space.}
\usage{
generate_perturbations(instance_scaled,
  num_samples = 5000,
  sample_around_instance = TRUE)
}
\arguments{
  \item{instance_scaled}{Standardised instance (numeric vector).}
  \item{num_samples}{Number of perturbed samples to generate; row 1 is the instance itself.}
  \item{sample_around_instance}{If TRUE, sample around the instance; otherwise around the origin (the training-data mean).}
}
\value{A numeric matrix (num_samples x length(instance_scaled)) in standardised space.}
\keyword{models}
