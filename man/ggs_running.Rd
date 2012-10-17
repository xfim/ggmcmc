\name{ggs_running}
\alias{ggs_running}
\title{Running means of the chains}
\usage{
  ggs_running(D)
}
\arguments{
  \item{D}{data frame whith the simulations}
}
\value{
  a ggplot object
}
\description{
  Running means of the chains
}
\examples{
data(samples)
ggs_running(ggs(S, parallel=FALSE))
}

