\name{ggs_separation}
\alias{ggs_separation}
\title{Separation plot}
\usage{
   ggs_separation(ppd, data, xlab = "", ylab = "", title = "", labels = NULL)
}
\arguments{
  \item{ppd}{Posterior predictive distribution of type mcmc, mcmc.list,
    or list of objects of either type. If a list of mcmc or mcmc.list
    objects is passed, the names of the models default to the names of
    the objects in the list unless the labels option is specified.}

  \item{data}{Binary response variable. Must be of the same length as
    the number of rows in the posterior predictive distribution. If a
    list of posterior predictions was passed, they must be predictions
    on the same response variable.}

  \item{xlab}{x-axis label for the plot.}
  \item{ylab}{y-axis label for the plot.}
  \item{title}{title for the plot.}
  \item{labels}{Vector of character labels. Only applicable when ppd is
  a list of mcmc or mcmc.list objects and the user prefers to replace
  the default names.}
\value{A \code{ggplot} object.}
\description{Separation plots show the predictive performance of a model
  with a binary dependent variable by sorting observations according the
  the mean prediction, producing a graphical separation of observations
  by outcome that increases with model performce.}
}
\examples{
#examples are broken for now
ggs_separation(model.ppd, data$response)
ggs_separation(list(model1.ppd, model2.ppd), data$response, labels =
  c("model 1", "model 2"))
}