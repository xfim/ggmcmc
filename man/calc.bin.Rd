\name{calc.bin}
\alias{calc.bin}
\title{Calculate binwidths by parameter, based on the total number of bins}
\usage{
  calc.bin(x, bins = bins)
}
\arguments{
  \item{x}{any vector or variable}

  \item{bins}{the number of requested bins}
}
\value{
  A data frame with the x location, the width of the bars
  and the number of observations at each x location.
}
\description{
  Compute the minimal elements to recreate a histogram
  manually by defining the total number of bins
}

