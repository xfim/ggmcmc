#' Plot the Cross-correlation between-chains
#'
#' Plot the Cross-correlation between-chains.
#'
#' @param D Data frame whith the simulations.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @param absolute_scale Logical. When TRUE (the default), the scale of the colour diverges between perfect inverse correlation (-1) to perfect correlation (1), whereas when FALSE, the scale is relative to the minimum and maximum cross-correlations observed.
#' @return a \code{ggplot} object.
#' @export
#' @examples
#' data(linear)
#' ggs_crosscorrelation(ggs(s))
ggs_crosscorrelation <- function(D, family=NA, absolute_scale=TRUE) {
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  if (attributes(D)$nParameters <= 1) {
    stop("Can't calculate crosscorrelations with a single chain")
  } 
 X <- spread(D[, c( "Iteration" ,"Chain" ,"Parameter","value"), drop=FALSE], Parameter, value)
  # Chain management is not easy
  bc.cc <- as.data.frame.table(
    cor(as.matrix(X[,-c(1, 2), drop=FALSE])),
    responseName="value")
  # Diagonals are avoided
  bc.cc$value[bc.cc$Var1==bc.cc$Var2] <- NA
  # Plot
  f <- ggplot(bc.cc, aes(x=Var1, y=Var2)) +
    geom_tile(aes(fill=value)) +
    xlab("") + ylab("") +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
    if (absolute_scale) {
      f <- f + scale_fill_gradient2(limits=c(-1, 1)) 
    } else {
      f <- f + scale_fill_gradient2() 
    }
  return(f)
}
