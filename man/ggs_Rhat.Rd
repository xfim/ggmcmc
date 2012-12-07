\name{ggs_Rhat}
\alias{ggs_Rhat}
\title{Dotplot of Potential Scale Reduction Factor (Rhat)}
\usage{
  ggs_Rhat(D, family = NA)
}
\arguments{
  \item{D}{Data frame whith the simulations}

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
  Plot a dotplot of Potential Scale Reduction Factor
  (Rhat), proposed by Gelman and Rubin (1992). The version
  from the second edition of Bayesian Data Analysis
  (Gelman, Carlin, Stein and Rubin) is used.
}
\details{
  Notice that at least two chains are required.
}
\examples{
data(samples)
ggs_Rhat(ggs(S, parallel=FALSE))
}

