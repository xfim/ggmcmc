#' Posterior predictive plot comparing the outcome vs the posterior standard deviations.
#'
#' Histogram with the distribution of the predicted posterior standard deviations, compared with the standard deviationsof the observed outcome.
#'
#' @param D Data frame whith the simulations. Notice that only the posterior outcomes are needed, and so either the ggs() call limits the parameters to the outcomes or the user provides a family of parameters to limit it.
#' @param outcome vector (or matrix or array) containing the observed outcome variable. Currently only a vector is supported.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @param bins integer indicating the total number of bins in which to divide the histogram. Defaults to 30, which is the same as geom_histogram()
#' 
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' \dontrun{
#' data(samples)
#' ggs_ppsd(ggs(S, family="y"), outcome="y")
#' }
ggs_ppsd <- function(D, outcome, family=NA, bins=30) {
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
  ppSD <- D %>%
    group_by(Iteration) %>%
    summarize(sd=sd(value))
  sd <- sd(outcome, na.rm=TRUE)
  # Calculate binwidths
  ppSDbw <- calc.bin(ppSD$sd, bins=bins)
  names(ppSDbw)[names(ppSDbw)=="x"] <- "Posterior predictive standard deviation"
  # Plot
  f <- ggplot(ppSDbw, aes(x=`Posterior predictive standard deviation`, y=count, width=width)) +
    geom_bar(stat="identity", position="identity") +
    geom_vline(xintercept=sd)
  return(f)
}
