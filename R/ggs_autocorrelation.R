#' Calculate the autocorrelation of a single chain, for a specified amount of lags
#'
#' Calculate the autocorrelation of a single chain, for a specified amount of lags.
#'
#' Internal function used by \code{\link{ggs_autocorrelation}}.
#'
#' @param x Vector with a chain of simulated values.
#' @param nLags Numerical value with the maximum number of lags to take into account.
#' @return A matrix with the autocorrelations of every chain.
#' @export
#' @examples
#' # Calculate the autocorrelation of a simple vector
#' ac(cumsum(rnorm(10))/10, nLags=4)
ac <- function(x, nLags) {
  X <- matrix(NA, ncol=nLags, nrow=length(x))
  X[,1] <- x
  for (i in 2:nLags) {
    X[,i] <- c(rep(NA, i-1), x[1:(length(x)-i+1)])
  }
  X <- (cor(X, use="pairwise.complete.obs")[,1])
  return(X)
}

#' Plot an autocorrelation matrix
#'
#' Plot an autocorrelation matrix.
#'
#' @param D Data frame whith the simulations.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @param nLags Integer indicating the number of lags of the autocorrelation plot.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' data(samples)
#' ggs_autocorrelation(ggs(S))
ggs_autocorrelation <- function(D, family=NA, nLags=50) {
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  
  nIter <- attr(D, 'nIteration')
  if (nIter < nLags) {
    warning(sprintf('nLags=%d is larger than number of iterations, computing until max possible lag %d', nLags, nIter))
    nLags <- nIter
  }

  wc.ac <- suppressWarnings(ddply(D, c("Parameter", "Chain"), here(summarise), .inform=TRUE,
    Autocorrelation=ac(value, nLags), 
    Lag=1:nLags,
    .parallel=attributes(D)$parallel))

  # Manage multiple chains
  if (attributes(D)$nChains <= 1) {
    f <- ggplot(wc.ac, aes(x=Lag, y=Autocorrelation)) + 
      geom_bar(stat="identity", position="identity") +
      facet_wrap(~ Parameter) + ylim(-1, 1)
  } else {
    f <- ggplot(wc.ac, aes(x=Lag, y=Autocorrelation, colour=as.factor(Chain), fill=as.factor(Chain))) + 
      geom_bar(stat="identity", position="identity") +
      facet_grid(Parameter ~ Chain) + ylim(-1, 1) +
      scale_fill_discrete(name="Chain") + scale_colour_discrete(name="Chain")
  }

 return(f)
}
