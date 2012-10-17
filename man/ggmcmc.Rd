\docType{package}
\name{ggmcmc}
\alias{ggmcmc}
\alias{ggmcmc-package}
\title{Wrapper for creating a single pdf file with all plots that ggmcmc can produce.}
\usage{
  ggmcmc(D, file = "ggmcmc-output.pdf", param.page = 5,
    width = 7, height = 10, ...)
}
\arguments{
  \item{D}{data frame whith the simulations, previously
  arranged using \code{\link{ggs}}}

  \item{file}{name of the file to create with the plots. By
  default, use "ggmcmc-output.pdf"}

  \item{param.page}{numerical, number of parameters to plot
  for each page. Defaults to 5.}

  \item{width}{width of the pdf display, in inches.
  Defaults to 7.}

  \item{height}{of the pdf display, in inches. Defaults to
  10.}

  \item{...}{other options for the pdf device}
}
\description{
  Notice that caterpillar plots are only created when there
  are multiple parameters within the same family. A family
  of parameters is considered to be all parameters that
  have the same name (usually the same greek letter) but
  different number within square brackets (such as
  alpha[1], alpha[2], ...).

  ggmcmc is a tool for assessing and diagnosing convergence
  of Markov Chain Monte Carlo simulations, as well as for
  graphically display results from full MCMC analysis. The
  package also facilitates the graphical interpretation of
  models by providing flexible functions to plot the
  results against observed variables.
}
\examples{
data(samples)
ggmcmc(ggs(S, parallel=FALSE))  # Directly from a coda object
}
\references{
  \url{http://xavier-fim.net/packages/ggmcmc}.
}

