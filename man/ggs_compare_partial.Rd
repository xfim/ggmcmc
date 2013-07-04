\name{ggs_compare_partial}
\alias{ggs_compare_partial}
\title{Density plots comparing the distribution of the whole chain with only its last part.}
\usage{
  ggs_compare_partial(D, family = NA, partial = 0.1)
}
\arguments{
  \item{D}{Data frame whith the simulations}

  \item{family}{Name of the family of parameters to plot,
  as given by a character vector or a regular expression. A
  family of parameters is considered to be any group of
  parameters with the same name but different numerical
  value between square brackets (as beta[1], beta[2],
  etc).}

  \item{partial}{Percentage of the chain to compare to.
  Defaults to the last 10 percent.}
}
\value{
  A \code{ggplot} object.
}
\description{
  Density plots comparing the distribution of the whole
  chain with only its last part.
}
\examples{
data(samples)
ggs_compare_partial(ggs(S))
}

