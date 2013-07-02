\name{ggs_separation}
\alias{ggs_separation}
\title{Separation plot for models with binary response variables}
\usage{
  ggs_separation(ppd, data, xlab = "", ylab = "",
    title = "")
}
\arguments{
  \item{ppd}{object of type mcmc or mcmc.list, which
  contains posterior predictions. The number of columns of
  each element must be the length of the response variable
  vector and the number of rows of each element of the
  object should be the number of MCMC iterations run}

  \item{data}{a vector which contains the binary response
  variable}

  \item{xlab}{label for the x-axis}

  \item{ylab}{label for the y-axis}

  \item{title}{title for the entire plot}
}
\value{
  A \code{ggplot} object
}
\description{
  Separation plot for models with binary response variables
}
\examples{
ggs_separation(ppd, df$response)
}

