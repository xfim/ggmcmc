\name{ggs_crosscorrelation}
\alias{ggs_crosscorrelation}
\title{Plot the Cross-correlation between-chains}
\usage{
  ggs_crosscorrelation(D, family = NA,
    absolute_scale = TRUE)
}
\arguments{
  \item{D}{Data frame whith the simulations.}

  \item{family}{Name of the family of parameters to plot,
  as given by a character vector or a regular expression. A
  family of parameters is considered to be any group of
  parameters with the same name but different numerical
  value between square brackets (as beta[1], beta[2],
  etc).}

  \item{absolute_scale}{Logical. When TRUE (the default),
  the scale of the colour diverges between perfect inverse
  correlation (-1) to perfect correlation (1), whereas when
  FALSE, the scale is relative to the minimum and maximum
  cross-correlations observed.}
}
\value{
  a \code{ggplot} object.
}
\description{
  Plot the Cross-correlation between-chains.
}
\examples{
data(samples)
ggs_crosscorrelation(ggs(S))
}

