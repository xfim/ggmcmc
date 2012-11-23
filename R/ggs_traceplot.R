#' Traceplot of the chains
#'
#' @param D data frame with the simulations
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @param original.burnin, logical. When true (default) start the Iteration counter in the x-axis at the end of the burnin period.
#' @param original.thin, logical. When true (default) take into account the thinning interval in the x-axis.
#' @return a ggplot object
#' @export
#' @examples
#' data(samples)
#' ggs_traceplot(ggs(S, parallel=FALSE))
ggs_traceplot <- function(D, family=NA, original.burnin=TRUE, original.thin=TRUE) {
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  # Plot
  if (attributes(D)$nChains <= 1) {
    f <- ggplot(D, aes(x=Iteration, y=value)) 
  } else {
    f <- ggplot(D, aes(x=Iteration, y=value, colour=as.factor(Chain))) 
  }
  f <- f + geom_line(alpha=0.7) + 
    facet_wrap(~ Parameter, ncol=1, scales="free") + 
    scale_colour_discrete(name="Chain")
  # Manage changing the scales using different sets of burnin and thinning
  # Duplicated code chunk in ggs_running()
  if (original.burnin & !original.thin) {
    f <- f + scale_x_continuous(labels=ggmcmc::b_format())
  } else if (!original.burnin & original.thin) {
    f <- f + scale_x_continuous(labels=ggmcmc::t_format())
  } else if (original.burnin & original.thin) {
    f <- f + scale_x_continuous(labels=ggmcmc::bt_format())
  } else {
    f <- f
  }
  return(f)
}
