#' Histograms of the paramters.
#'
#' Plot a histogram of each of the parameters. Histograms are plotted combining all chains for each parameter.
#
#' @param D Data frame whith the simulations.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' data(samples)
#' ggs_histogram(ggs(S, parallel=FALSE))
ggs_histogram <- function(D, family=NA) {
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  # Plot
  f <- ggplot(D, aes(x=value)) + geom_histogram() + 
    facet_wrap(~ Parameter, ncol=1, scales="free")
  return(f)
}
