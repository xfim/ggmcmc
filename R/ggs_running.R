#' Running means of the chains
#'
#' @param D data frame whith the simulations
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @return a ggplot object
#' @export
#' @examples
#' data(samples)
#' ggs_running(ggs(S, parallel=FALSE))
ggs_running <- function(D, family=NA) {
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  # Calculate the mean of the chain
  dm.m <- ddply(D, .(Parameter, Chain), summarize, m=mean(value), 
    .parallel=attributes(D)$parallel)
  # Calculate the running mean
  # Force the object to be sorted by Parameter, and hence avoid 'rm' calculation
  # to be wrong
  D.sorted <- D[order(D$Parameter, D$Iteration),]
  dm.rm <- ddply(D.sorted, .(Parameter, Chain), transform, rm=cumsum(value)/Iteration, 
    .parallel=attributes(D)$parallel)
  # Plot
  f <- ggplot(dm.rm, aes(x=Iteration, y=rm, colour=as.factor(Chain))) + 
    geom_line() + 
    geom_hline(aes(yintercept=m), dm.m, colour="black", alpha=0.5) +
    ylab("Running Mean")
  if (attributes(D)$nChains <= 1) {
    f <- f + facet_wrap(~ Parameter, ncol=1, scales="free") 
  } else {
    f <- f + facet_grid(Parameter ~ Chain, scales="free") 
  }
  f <- f + scale_colour_discrete(name="Chain")
  return(f)
}
