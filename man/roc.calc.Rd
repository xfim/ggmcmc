\name{roc.calc}
\alias{roc.calc}
\title{Calculate the ROC curve for a set of observed outcomes and predicted probabilities}
\usage{
  roc.calc(R)
}
\arguments{
  \item{R}{data frame with the 'value' (predicted
  probability) and the observed 'Outcome'.}
}
\value{
  A data frame with the Sensitivity and the Specificity.
}
\description{
  Internal function used by
  \code{\link{ggs_autocorrelation}}.
}

