\name{ggs_geweke}
\alias{ggs_geweke}
\title{Dotplot of the Geweke diagnostic, the standard Z-score}
\usage{
  ggs_geweke(D, family = NA, frac1 = 0.1, frac2 = 0.5)
}
\arguments{
  \item{D}{data frame whith the simulations.}

  \item{family}{Name of the family of parameters to plot,
  as given by a character vector or a regular expression. A
  family of parameters is considered to be any group of
  parameters with the same name but different numerical
  value between square brackets (as beta[1], beta[2],
  etc).}

  \item{frac1}{Numeric, proportion of the first part of the
  chains selected. Defaults to 0.1.}

  \item{frac2}{Numeric, proportion of the last part of the
  chains selected. Defaults to 0.5.}
}
\value{
  A \code{ggplot} object.
}
\description{
  Dotplot of Geweke diagnostic.
}
\examples{
data(samples)
ggs_geweke(ggs(S))
}

