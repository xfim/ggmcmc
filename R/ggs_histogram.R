#' Histograms of the paramters.
#'
#' Plot a histogram of each of the parameters. Histograms are plotted combining all chains for each parameter.
#
#' @param D Data frame whith the simulations.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @param bins integer indicating the total number of bins in which to divide the histogram. Defaults to 30, which is the same as geom_histogram()
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' data(samples)
#' ggs_histogram(ggs(S))
ggs_histogram <- function(D, family=NA, bins=30) {
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  # Manually generate the histogram by parameter, based on the total number of
  # bins
  l <- unlist(dlply(D, .(Parameter), here(summarize), calc.bin(value, bins)), recursive=FALSE)
  ds <- ldply(l, data.frame)
  dl <- as.numeric(table(ds$`.id`))
  # There may be cases of parameters with slightly different numbers of bins,
  # and therefore a Parameter-by-Parameter approach is needed
  ds <- cbind(Parameter=gl.unq(attributes(D)$nParameters, dl, labels=levels(D$Parameter)), ds)
  # Plot
  f <- ggplot(ds, aes(x=x, y=count, width=width)) + geom_bar(stat="identity", position="identity") +
    facet_wrap(~ Parameter, ncol=1, scales="free") + xlab("value")
  return(f)
}
