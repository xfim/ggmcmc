\name{ggs_histogram}
\alias{ggs_histogram}
\title{Histograms of the paramters.}
\usage{
  ggs_histogram(D, family = NA)
}
\arguments{
  \item{D}{Data frame whith the simulations.}

  \item{family}{Name of the family of parameters to plot,
  as given by a character vector or a regular expression. A
  family of parameters is considered to be any group of
  parameters with the same name but different numerical
  value between square brackets (as beta[1], beta[2],
  etc).}
}
\value{
  A \code{ggplot} object.
}
\description{
  Plot a histogram of each of the parameters. Histograms
  are plotted combining all chains for each parameter.
}
\examples{
data(samples)
ggs_histogram(ggs(S, parallel=FALSE))
}

