\name{ggs_running}
\alias{ggs_running}
\title{Running means of the chains}
\usage{
  ggs_running(D, family = NA, original.burnin = TRUE,
    original.thin = TRUE)
}
\arguments{
  \item{D}{Data frame whith the simulations.}

  \item{family}{Name of the family of parameters to plot,
  as given by a character vector or a regular expression. A
  family of parameters is considered to be any group of
  parameters with the same name but different numerical
  value between square brackets (as beta[1], beta[2],
  etc).}

  \item{original.burnin}{Logical. When TRUE (the default),
  start the iteration counter in the x-axis at the end of
  the burnin period.}

  \item{original.thin}{Logical. When TRUE (the default),
  take into account the thinning interval in the x-axis.}
}
\value{
  A \code{ggplot} object.
}
\description{
  Running means of the chains.
}
\examples{
data(samples)
ggs_running(ggs(S, parallel=FALSE))
}

