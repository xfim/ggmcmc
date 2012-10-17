\name{ac}
\alias{ac}
\title{Compute the autocorrelation of a single chain, for a specified amount of lags}
\usage{
  ac(x, nLags)
}
\arguments{
  \item{x}{vector with a chain of simulated values}

  \item{nLags}{maximum number of lags to take into account}
}
\value{
  a matrix with the autocorrelations of every chain
}
\description{
  Internal function used by
  \code{\link{ggs_autocorrelation}}.
}
\examples{
# Calculate the autocorrelation of a simple vector
ac(cumsum(rnorm(10))/10, nLags=4)
}

