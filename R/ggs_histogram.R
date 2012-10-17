#' Plot a histogram of each of the chains
#'
#' Histograms are plotted combining all chains for each parameter.
#
#' @param D data frame whith the simulations
#' @return a ggplot object
#' @export
#' @examples
#' data(samples)
#' ggs_histogram(ggs(S, parallel=FALSE))
ggs_histogram <- function(D) {
  f <- ggplot(D, aes(x=value)) + geom_histogram() + 
    facet_wrap(~ Parameter, ncol=1, scales="free")
  return(f)
}
