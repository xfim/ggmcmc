\name{ggs_rocplot}
\alias{ggs_rocplot}
\title{Receiver-Operator Characteristic (ROC) plot for models with binary dependent variables}
\usage{
  ggs_rocplot(ppd, data, xlab = "false positive rate",
    ylab = "true positive rate", title = "", labels)
}
\arguments{
  \item{ppd}{object of type mcmc or mcmc.list which
  contains posterior predictions. May also be a list of
  objects of type mcmc or mcmc.list. If a single ppd is
  passed, then the number of columns of each element must
  be the length of the response variable vector and the
  number of rows of each element of the object should be
  the number of MCMC iterations run. If multiple models are
  passed as a list of mcmc or mcmc.list objects, the same
  must be true for each ppd within the list of ppds}

  \item{data}{a vector which contains the binary response
  variable}

  \item{xlab}{label for the x-axis}

  \item{ylab}{label for the y-axis}

  \item{title}{title for the entire plot}

  \item{labels}{labels for separate models}
}
\value{
  A \code{ggplot} object
}
\description{
  Receiver-Operator Characteristic (ROC) plot for models
  with binary dependent variables
}
\examples{
ggs_rocplot(ppd, df$response)
}

