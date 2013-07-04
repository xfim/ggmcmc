\name{ggs_autocorrelation}
\alias{ggs_autocorrelation}
\title{Plot an autocorrelation matrix}
\usage{
  ggs_autocorrelation(D, family = NA, nLags = 50)
}
\arguments{
  \item{D}{Data frame whith the simulations.}

  \item{family}{Name of the family of parameters to plot,
  as given by a character vector or a regular expression. A
  family of parameters is considered to be any group of
  parameters with the same name but different numerical
  value between square brackets (as beta[1], beta[2],
  etc).}

  \item{nLags}{Integer indicating the number of lags of the
  autocorrelation plot.}
}
\value{
  A \code{ggplot} object.
}
\description{
  Plot an autocorrelation matrix.
}
\examples{
data(samples)
ggs_autocorrelation(ggs(S))
}

