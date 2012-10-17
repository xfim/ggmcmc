\name{ggs_compare_partial}
\alias{ggs_compare_partial}
\title{Compare the distribution of the whole chain with only the last part of it}
\usage{
  ggs_compare_partial(D, partial = 0.1)
}
\arguments{
  \item{D}{data frame whith the simulations}

  \item{partial}{percentage of the chain to compare to, by
  default the last 10 percent.}
}
\value{
  a ggplot object
}
\description{
  Compare the distribution of the whole chain with only the
  last part of it
}
\examples{
data(samples)
ggs_compare_partial(ggs(S, parallel=FALSE))
}

