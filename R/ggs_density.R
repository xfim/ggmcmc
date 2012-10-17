#' Density plots 
#'
#' For multiple chains, use colours to differentiate the distributions
#'
#' @param D data frame whith the simulations
#' @return a ggplot object
#' @export
#' @examples
#' data(samples)
#' ggs_density(ggs(S, parallel=FALSE))
ggs_density <- function(D) {
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
