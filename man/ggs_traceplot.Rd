\name{ggs_traceplot}
\alias{ggs_traceplot}
\title{Traceplot of the chains}
\usage{
  ggs_traceplot(D)
}
\arguments{
  \item{D}{data frame whith the simulations}
}
\value{
  a ggplot object
}
\description{
  Traceplot of the chains
}
\examples{
data(samples)
ggs_traceplot(ggs(S, parallel=FALSE))
}

