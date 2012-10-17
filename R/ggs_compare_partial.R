#' Compare the distribution of the whole chain with only the last part of it
#'
#'
#' @param D data frame whith the simulations
#' @param partial percentage of the chain to compare to, by default the last 10 percent.
#' @return a ggplot object
#' @export
#' @examples
#' data(samples)
#' ggs_compare_partial(ggs(S, parallel=FALSE))
ggs_compare_partial <- function(D, partial=0.1) {
  # Check that partial is a percentage
  if (partial <= 0 | partial >=1 | !is.numeric(partial)) {
    stop("partial must be a numerical argument and a value greater than zero or less than one")
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
