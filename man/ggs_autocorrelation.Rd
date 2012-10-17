\name{ggs_autocorrelation}
\alias{ggs_autocorrelation}
\title{Plot an autocorrelation matrix}
\usage{
  ggs_autocorrelation(D, nLags = 50)
}
\arguments{
  \item{D}{data frame whith the simulations}

  \item{nLags}{integer indicating the number of lags of the
  autocorrelation plot}
}
\value{
  a ggplot object
}
\description{
  Plot an autocorrelation matrix
}
\examples{
data(samples)
ggs_autocorrelation(ggs(S, parallel=FALSE))
}

