#' Traceplot of the chains
#'
#' Traceplot with the time series of the chains.
#'
#' @references Fernández-i-Marín, Xavier (2016) ggmcmc: Analysis of MCMC Samples and Bayesian Inference. Journal of Statistical Software, 70(9), 1-20. doi:10.18637/jss.v070.i09
#' @param D Data frame with the simulations.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @param original_burnin Logical. When TRUE (the default) start the Iteration counter in the x-axis at the end of the burnin period.
#' @param original_thin Logical. When TRUE (the default) take into account the thinning interval in the x-axis.
#' @param simplify Numerical. A percentage of iterations to keep in the time series. It is an option intended only for the purpose of saving time and resources when doing traceplots. It is not a thin operation, because it is not regular. It must be used with care.
#' @param greek Logical value indicating whether parameter labels have to be parsed to get Greek letters. Defaults to false.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' data(linear)
#' ggs_traceplot(ggs(s))
ggs_traceplot <- function(D, family=NA, original_burnin=TRUE, original_thin=TRUE, simplify=NULL, greek=FALSE) {
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  if (!is.null(simplify)) {
    if (simplify > 0 & simplify < 1) {
      aD <- attributes(D)
      D <- dplyr::sample_frac(D, simplify)
      # Unfortunately, the attributes are not inherited, so they have to be manually passed again
      attr(D, "nChains") <- aD$nChains
      attr(D, "nParameters") <- aD$nParameters
      attr(D, "nIterations") <- aD$nIterations
      attr(D, "nBurnin") <- aD$nBurnin
      attr(D, "nThin") <- aD$nThin
      attr(D, "description") <- aD$description
    } else {
      print("It is not possible to guess the simplification percentage to apply.")
    }
  }
  # Plot
  if (attributes(D)$nChains <= 1) {
    f <- ggplot(D, aes(x=Iteration, y=value)) 
  } else {
    f <- ggplot(D, aes(x=Iteration, y=value, colour=as.factor(Chain))) 
  }
  f <- f + geom_line(alpha=0.7) + 
    scale_colour_discrete(name="Chain")
  if (!greek) {
    f <- f + facet_wrap(~ Parameter, ncol=1, scales="free")
  } else {
    f <- f + facet_wrap(~ Parameter, ncol=1, scales="free", labeller = label_parsed)
  }
  # Manage changing the scales using different sets of burnin and thinning
  # Duplicated code chunk in ggs_running()
  t_format <- function(x) {
    return( ((x-1) * attributes(D)$nThin) + attributes(D)$nThin)
  }
  b_format <- function(x) {
    return(x + attributes(D)$nBurnin)
  }
  bt_format <- function(x) {
    return( attributes(D)$nBurnin + (((x-1) * attributes(D)$nThin) + attributes(D)$nThin))
  }
  if (original_burnin & !original_thin) {
    f <- f + scale_x_continuous(labels=b_format)
  } else if (!original_burnin & original_thin) {
    f <- f + scale_x_continuous(labels=t_format)
  } else if (original_burnin & original_thin) {
    f <- f + scale_x_continuous(labels=bt_format)
  } else {
    f <- f
  }
  return(f)
}
