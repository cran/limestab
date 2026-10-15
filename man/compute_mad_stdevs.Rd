\name{compute_mad_stdevs}
\alias{compute_mad_stdevs}
\title{Robust standard deviation from repeated coefficients}
\description{Robust standard deviation from repeated coefficients.}
\usage{
compute_mad_stdevs(beta_list)
}
\arguments{
  \item{beta_list}{Matrix of coefficients, one row per repeated explanation.}
}
\value{A numeric vector, 1.4826 * MAD for each column.}
\keyword{models}
