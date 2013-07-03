\name{ggs_separation}
\alias{ggs_separation}
\title{Separation plot for models with binary response variables}
\usage{
  ggs_separation(D, outcome, fully.bayesian = FALSE,
    minimalist = FALSE)
}
\arguments{
  \item{D}{Data frame whith the simulations. Notice that
  only the posterior outcomes are needed, and so either the
  previous call to ggs() should have limited the family of
  parameters to pass to the predicted outcomes.}

  \item{outcome}{vector (or matrix or array) containing the
  observed outcome variable. Currently only a vector is
  supported.}

  \item{fully.bayesian}{logical, FALSE by default.
  Currently not implemented}

  \item{minimalist}{logical, FALSE by default. It returns a
  minimalistic version of the figure with the bare minimum
  elements, suitable for being used inline as suggested by
  Greenhill, Ward and Sacks citing Tufte.}
}
\value{
  A \code{ggplot} object
}
\description{
  Separation plot for models with binary response variables
}
\examples{
\dontrun{
ggs_separation(S, outcome=y)
}
}
\references{
  Greenhill, Ward and Sacks (2011): The separation plot: a
  new visual method for evaluating the fit of binary
  models. American Journal of Political Science, vol 55,
  number 4, pg 991-1002.
}

