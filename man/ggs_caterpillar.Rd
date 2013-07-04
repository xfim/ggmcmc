\name{ggs_caterpillar}
\alias{ggs_caterpillar}
\title{Caterpillar plot with thick and thin CI}
\usage{
  ggs_caterpillar(D, family = NA, X = NA,
    thick.ci = c(0.05, 0.95), thin.ci = c(0.025, 0.975),
    line = NA, horizontal = TRUE, labels = NULL)
}
\arguments{
  \item{D}{Data frame whith the simulations or list of data
  frame with simulations. If a list of data frames with
  simulations is passed, the names of the models are the
  names of the objects in the list.}

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

  \item{thick.ci}{Vector of length 2 with the quantiles of
  the thick band for the credible interval}

  \item{thin.ci}{Vector of length 2 with the quantiles of
  the thin band for the credible interval}

  \item{line}{Numerical value indicating a concrete
  position, usually used to mark where zero is. By default
  do not plot any line.}

  \item{horizontal}{Logical. When TRUE (the default), the
  plot has horizontal lines. When FALSE, the plot is
  reversed to show vertical lines. Horizontal lines are
  more appropriate for categorical caterpillar plots,
  because the x-axis is the only dimension that matters.
  But for caterpillar plots against another variable, the
  vertical position is more appropriate.}

  \item{labels}{Vector of strings that matches the number
  of models in the list. It is only used in case of
  multiple models and when the list of ggs objects given at
  \code{D} is not named. Otherwise, the names in the list
  are used.}
}
\value{
  A \code{ggplot} object.
}
\description{
  Caterpillar plots are plotted combining all chains for
  each parameter.
}
\examples{
data(samples)
ggs_caterpillar(ggs(S))
ggs_caterpillar(list(A=ggs(S), B=ggs(S))) # silly example duplicating the same model
}

