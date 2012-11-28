\name{ggs_caterpillar}
\alias{ggs_caterpillar}
\title{Caterpillar plot with thick and thin CI}
\usage{
  ggs_caterpillar(D, family = NA, X = NA,
    thick.ci = c(0.05, 0.95), thin.ci = c(0.025, 0.975),
    line = NA, horizontal = FALSE)
}
\arguments{
  \item{D}{data frame whith the simulations}

  \item{X}{data frame with two columns, Parameter and the
  value for the x location. Parameter must be a character
  vector with the same names that the parameters in the D
  object.}

  \item{family}{Name of the family of parameters to plot,
  as given by a character vector or a regular expression. A
  family of parameters is considered to be any group of
  parameters with the same name but different numerical
  value between square brackets (as beta[1], beta[2],
  etc).}

  \item{thick.ci}{vector of length 2 with the quantiles of
  the thick band for the credible interval}

  \item{thin.ci}{vector of length 2 with the quantiles of
  the thin band for the credible interval}

  \item{line}{plot a line indicating a concrete position,
  usually used to mark where zero is. By default do not
  plot any line.}

  \item{horizontal}{logical value for the plot having
  horizontal or vertical lines. Vertical is the default.}
}
\value{
  a ggplot object
}
\description{
  Caterpillar plots are plotted combining all chains for
  each parameter.
}
\examples{
data(samples)
ggs_caterpillar(ggs(S, parallel=FALSE))
}

