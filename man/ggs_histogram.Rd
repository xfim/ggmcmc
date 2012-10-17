\name{ggs_histogram}
\alias{ggs_histogram}
\title{Plot a histogram of each of the chains}
\usage{
  ggs_histogram(D)
}
\arguments{
  \item{D}{data frame whith the simulations}
}
\value{
  a ggplot object
}
\description{
  Histograms are plotted combining all chains for each
  parameter.
}
\examples{
data(samples)
ggs_histogram(ggs(S, parallel=FALSE))
}

