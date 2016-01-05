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
  X <- data.frame(Lag=1:nLags, Autocorrelation=cor(X, use="pairwise.complete.obs")[,1])
  return(X)
}

#' Plot an autocorrelation matrix
#'
#' Plot an autocorrelation matrix.
#'
#' @param D Data frame whith the simulations.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @param nLags Integer indicating the number of lags of the autocorrelation plot.
#' @param greek Logical value indicating whether parameter labels have to be parsed to get Greek letters. Defaults to false.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' data(linear)
#' ggs_autocorrelation(ggs(s))
ggs_autocorrelation <- function(D, family=NA, nLags=50, greek=FALSE) {
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  
  nIter <- attr(D, 'nIteration')
  if (nIter < nLags) {
    warning(sprintf('nLags=%d is larger than number of iterations, computing until max possible lag %d', nLags, nIter))
    nLags <- nIter
  }

  # No way to make the following use summarize(), as of dplyr 0.3
  # https://github.com/hadley/dplyr/issues/154
  # Temporary workaround using dplyr 0.2 and do()
  wc.ac <- D %>%
    dplyr::group_by(Parameter, Chain) %>%
    dplyr::do(ac(.$value, nLags))

  # Manage multiple chains
  if (attributes(D)$nChains <= 1) {
    f <- ggplot(wc.ac, aes(x=Lag, y=Autocorrelation)) + 
      geom_bar(stat="identity", position="identity") + ylim(-1, 1)
    if (!greek) {
      f <- f + facet_wrap(~ Parameter)
    } else {
      f <- f + facet_wrap(~ Parameter, labeller = label_parsed)
    }
  } else {
    f <- ggplot(wc.ac, aes(x=Lag, y=Autocorrelation, colour=as.factor(Chain), fill=as.factor(Chain))) + 
      geom_bar(stat="identity", position="identity") + ylim(-1, 1) +
      scale_fill_discrete(name="Chain") + scale_colour_discrete(name="Chain")
    if (!greek ) {
      f <- f + facet_grid(Parameter ~ Chain)
    } else {
      f <- f + facet_grid(Parameter ~ Chain, labeller = label_parsed)
    }
  }

 return(f)
}
