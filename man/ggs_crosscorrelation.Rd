\name{ggs_crosscorrelation}
\alias{ggs_crosscorrelation}
\title{Cross-correlation between-chain}
\usage{
  ggs_crosscorrelation(D)
}
\arguments{
  \item{D}{data frame whith the simulations}
}
\value{
  a ggplot object
}
\description{
  Cross-correlation between-chain
}
\examples{
data(samples)
ggs_crosscorrelation(ggs(S, parallel=FALSE))
}

