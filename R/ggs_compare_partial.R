#' Density plots comparing the distribution of the whole chain with only its last part.
#'
#' Density plots comparing the distribution of the whole chain with only its last part.
#'
#' @param D Data frame whith the simulations
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @param partial Percentage of the chain to compare to. Defaults to the last 10 percent.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' data(samples)
#' ggs_compare_partial(ggs(S))
ggs_compare_partial <- function(D, family=NA, partial=0.1) {
  # Check that partial is a percentage
  if (partial <= 0 | partial >=1 | !is.numeric(partial)) {
    stop("partial must be a numerical argument and a value greater than zero or less than one")
  }
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  # Get the maximum number of samples
  n.samples <- max(D$Iteration)
  # Add more rows to the original dataframe according to the desired size of the
  # comparison
  D.comp <- rbind(
    cbind(part_chain="Complete", D), 
    cbind(part_chain="Partial", 
      subset(D, Iteration>(n.samples-trunc(n.samples * partial)))))

  # Generate the plot
  f <- ggplot(D.comp, aes(x=value, fill=part_chain, colour=part_chain), group=Chain) + 
    geom_density(alpha=0.4)
  # Manage multiple chains
  if (attributes(D)$nChains <= 1) {
    f <- f + facet_wrap(~ Parameter, ncol=1, scales="free")
  } else {
    f <- f + facet_wrap(Parameter ~ Chain, ncol=attributes(D)$nChains, scales=c("free"))
  }
  f <- f + geom_rug(alpha=0.1) +
    scale_colour_manual(name="Chain length", values=c("black", "aquamarine3")) +
    scale_fill_manual(name="Chain length", values=c("black", "aquamarine3"))
  return(f)
}
