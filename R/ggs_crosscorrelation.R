#' Cross-correlation between-chain
#'
#' @param D data frame whith the simulations
#' @return a ggplot object
#' @export
#' @examples
#' data(samples)
#' ggs_crosscorrelation(ggs(S, parallel=FALSE))
ggs_crosscorrelation <- function(D) {
  X <- cast(D, Iteration + Chain ~ Parameter)
  # Chain management is not easy
  bc.cc <- melt(cor(as.matrix(X[,-c(1, 2)])))
  # Need to revert parameter names
  bc.cc$X1 <- factor(bc.cc$X1, labels=levels(D$Parameter))
  bc.cc$X2 <- factor(bc.cc$X2, labels=levels(D$Parameter))
  bc.cc$value[bc.cc$X1==bc.cc$X2] <- NA
  f <- ggplot(bc.cc, aes(x=X1, y=X2)) + 
    geom_tile(aes(fill=value)) + 
    scale_fill_gradient2() + xlab("") + ylab("") + 
    theme(axis.text.x=element_text(angle=90))
  return(f)
}
