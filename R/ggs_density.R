#' Density plots
#'
#' Density plots with the parameter distribution. For multiple chains, use colours to differentiate the distributions.
#'
#' @param D data frame whith the simulations
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @return a ggplot object
#' @export
#' @examples
#' data(samples)
#' ggs_density(ggs(S, parallel=FALSE))
ggs_density <- function(D, family=NA) {
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  # Plot
  if (attributes(D)$nChains <= 1) {
    f <- ggplot(D, aes(x=value))
  } else {
    f <- ggplot(D, aes(x=value, colour=as.factor(Chain), fill=as.factor(Chain)))
  }
  f <- f + geom_density(alpha=0.3) + 
      facet_wrap(~ Parameter, ncol=1, scales="free") + geom_rug(alpha=0.1) +
      scale_fill_discrete(name="Chain") + scale_colour_discrete(name="Chain")
  return(f)
}
