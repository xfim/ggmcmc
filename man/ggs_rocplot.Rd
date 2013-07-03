\name{ggs_rocplot}
\alias{ggs_rocplot}
\title{Receiver-Operator Characteristic (ROC) plot for models with binary dependent variables}
\usage{
  ggs_rocplot(D, outcome, fully.bayesian = FALSE)
}
\arguments{
  \item{D}{Data frame whith the simulations. Notice that
  only the posterior outcomes are needed, and so either the
  ggs() call limits the parameters to the outcomes or the
  user provides a family of parameters to limit it.}

  \item{outcome}{vector (or matrix or array) containing the
  observed outcome variable. Currently only a vector is
  supported.}

  \item{family}{Name of the family of parameters to plot,
  as given by a character vector or a regular expression. A
  family of parameters is considered to be any group of
  parameters with the same name but different numerical
  value between square brackets (as beta[1], beta[2],
  etc).}

  \item{fully.bayesian}{logical, false by default. When not
  fully Bayesian, it uses the median of the predictions for
  each observation by iteration. When TRUE the function
  plots as many ROC curves as iterations. It uses a a lot
  of CPU and needs more memory. Use it with caution.}
}
\value{
  A \code{ggplot} object
}
\description{
  Receiver-Operator Characteristic (ROC) plot for models
  with binary dependent variables
}
\examples{
\dontrun{
ggs_rocplot(S, outcome=y)
}
}

