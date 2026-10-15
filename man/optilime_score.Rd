\name{optilime_score}
\alias{optilime_score}
\title{OptiLIME objective (reflected local R-squared)}
\description{OptiLIME objective (reflected local R-squared).}
\usage{
optilime_score(explanation,
  max_r2 = 0.9,
  epsilon = NULL)
}
\arguments{
  \item{explanation}{The list returned by lime_explain_instance().}
  \item{max_r2}{Target local R-squared; scores above it are penalised symmetrically.}
  \item{epsilon}{Optional tolerance used to flag and penalise degenerate explanations.}
}
\value{A single numeric value to be maximised by optilime().}
\keyword{models}
