#' Cross-correlation between-chain
#'
#' @param D data frame whith the simulations
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @return a ggplot object
#' @export
#' @examples
#' data(samples)
#' ggs_crosscorrelation(ggs(S, parallel=FALSE))
ggs_crosscorrelation <- function(D, family=NA) {
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  if (attributes(D)$nParameters <= 1) {
    stop("Can't calculate crosscorrelations with a single chain")
  } 
  X <- cast(D, Iteration + Chain ~ Parameter)
  # Chain management is not easy
  bc.cc <- melt(cor(as.matrix(X[,-c(1, 2)])))
  # Need to revert parameter names
  bc.cc$X1 <- factor(bc.cc$X1, labels=levels(D$Parameter))
  bc.cc$X2 <- factor(bc.cc$X2, labels=levels(D$Parameter))
  bc.cc$value[bc.cc$X1==bc.cc$X2] <- NA
  # Plot
  f <- ggplot(bc.cc, aes(x=X1, y=X2)) + 
    geom_tile(aes(fill=value)) +
    xlab("") + ylab("") + theme(axis.text.x=element_text(angle=90)) +
    # todo, scales fixed between -1 and 1
    scale_fill_gradient2() 
  return(f)
}
