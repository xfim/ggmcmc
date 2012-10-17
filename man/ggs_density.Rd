\name{ggs_density}
\alias{ggs_density}
\title{Density plots}
\usage{
  ggs_density(D)
}
\arguments{
  \item{D}{data frame whith the simulations}
}
\value{
  a ggplot object
}
\description{
  For multiple chains, use colours to differentiate the
  distributions
}
\examples{
data(samples)
ggs_density(ggs(S, parallel=FALSE))
}

