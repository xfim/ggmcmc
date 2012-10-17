#' Traceplot of the chains
#'
#' @param D data frame whith the simulations
#' @return a ggplot object
#' @export
#' @examples
#' data(samples)
#' ggs_traceplot(ggs(S, parallel=FALSE))
ggs_traceplot <- function(D) {
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
