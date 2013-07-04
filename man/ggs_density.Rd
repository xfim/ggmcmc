\name{ggs_density}
\alias{ggs_density}
\title{Density plots of the chains}
\usage{
  ggs_density(D, family = NA)
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
  Density plots with the parameter distribution. For
  multiple chains, use colours to differentiate the
  distributions.
}
\examples{
data(samples)
ggs_density(ggs(S))
}

