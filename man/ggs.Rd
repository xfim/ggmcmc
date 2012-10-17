\name{ggs}
\alias{ggs}
\title{Manage the output from a coda object to be plotted by ggmcmc and convert it in an object that ggplot understands}
\usage{
  ggs(S, parallel = TRUE)
}
\arguments{
  \item{S}{mcmc object from samples}

  \item{parallel}{logical value for using parallel
  computing when managing the data frame in other
  functions}
}
\value{
  D data frame with the data arranged and ready to be used
  by the rest of the ggmcmc functions. The dataframe has
  four columns, namely: Iteration, Parameter, value and
  Chain, and four attributes: nChains, nParameters,
  nIterations, parallel.
}
\description{
  Manage the output from a coda object to be plotted by
  ggmcmc and convert it in an object that ggplot
  understands
}
\examples{
# Assign 'D' to be a data frame suitable for ggmcmc functions from
# a coda object called S
data(samples)
D <- ggs(S)        # S is a coda object
}

