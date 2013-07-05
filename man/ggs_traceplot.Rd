\name{ggs_traceplot}
\alias{ggs_traceplot}
\title{Traceplot of the chains}
\usage{
  ggs_traceplot(D, family = NA, original_burnin = TRUE,
    original_thin = TRUE)
}
\arguments{
  \item{D}{Data frame with the simulations.}

  \item{family}{Name of the family of parameters to plot,
  as given by a character vector or a regular expression. A
  family of parameters is considered to be any group of
  parameters with the same name but different numerical
  value between square brackets (as beta[1], beta[2],
  etc).}

  \item{original_burnin}{Logical. When TRUE (the default)
  start the Iteration counter in the x-axis at the end of
  the burnin period.}

  \item{original_thin}{Logical. When TRUE (the default)
  take into account the thinning interval in the x-axis.}
}
\value{
  A \code{ggplot} object.
}
\description{
  Traceplot with the time series of the chains.
}
\examples{
data(samples)
ggs_traceplot(ggs(S, parallel=FALSE))
}

