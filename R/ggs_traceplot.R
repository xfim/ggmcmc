#' Traceplot of the chains
#'
#' @param D data frame with the simulations
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @return a ggplot object
#' @export
#' @examples
#' data(samples)
#' ggs_traceplot(ggs(S, parallel=FALSE))
ggs_traceplot <- function(D, family=NA) {
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
  return(f)
}
