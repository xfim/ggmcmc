#' Posterior predictive plot comparing the outcome mean vs the distribution of the predicted posterior means.
#'
#' Histogram with the distribution of the predicted posterior means, compared with the mean of the observed outcome.
#'
#' @references Fernández-i-Marín, Xavier (2016) ggmcmc: Analysis of MCMC Samples and Bayesian Inference. Journal of Statistical Software, 70(9), 1-20. doi:10.18637/jss.v070.i09
#' @param D Data frame whith the simulations. Notice that only the posterior outcomes are needed, and so either the ggs() call limits the parameters to the outcomes or the user provides a family of parameters to limit it.
#' @param outcome vector (or matrix or array) containing the observed outcome variable. Currently only a vector is supported.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @param bins integer indicating the total number of bins in which to divide the histogram. Defaults to 30, which is the same as geom_histogram()
#' 
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' data(linear)
#' ggs_ppmean(ggs(s.y.rep), outcome=y)
ggs_ppmean <- function(D, outcome, family=NA, bins=30) {
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  # Check that the lengths of the outcome and the predicted posterior are equal
  lo <- length(outcome)
  lpp <- attributes(D)$nParameters
  if (lo != lpp) {
    stop("The length of the outcome must be equal to the number of Parameters of the ggs object.")
  }
  # Calculate the posterior predictive means at each iteration
  ppM <- D %>%
    dplyr::group_by(Iteration) %>%
    dplyr::summarize(m=mean(value))
  m <- mean(outcome, na.rm=TRUE)
  # Calculate binwidths
  ppMbw <- calc_bin(ppM$m, bins=bins)
  names(ppMbw)[names(ppMbw)=="x"] <- "Posterior predictive mean"
  # Plot
  f <- ggplot(ppMbw, aes(x=`Posterior predictive mean`, y=count, width=width)) +
    geom_bar(stat="identity", position="identity") +
    geom_vline(xintercept=m)
  return(f)
}
