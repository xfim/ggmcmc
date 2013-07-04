#' Dotplot of Potential Scale Reduction Factor (Rhat)
#'
#' Plot a dotplot of Potential Scale Reduction Factor (Rhat), proposed by Gelman and Rubin (1992). The version from the second edition of Bayesian Data Analysis (Gelman, Carlin, Stein and Rubin) is used.
#'
#' Notice that at least two chains are required.
#
#' @param D Data frame whith the simulations
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' data(samples)
#' ggs_Rhat(ggs(S))
ggs_Rhat <- function(D, family=NA) {
  if (attributes(D)$nChains<2) {
    stop("At least two chains are required")
  }
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  # The computations follow BDA, pg 296-297, and the notation tries to be
  # consistent with it
  # Compute between-sequence variance using psi.. and psi.j
  psi.dot <- suppressWarnings(ddply(D, .(Parameter, Chain), summarize, psi.dot=mean(value), 
    .parallel=attributes(D)$parallel))
  psi.j <- suppressWarnings(ddply(D, .(Parameter), summarize, psi.j=mean(value), 
    .parallel=attributes(D)$parallel))
  b.df <- merge(psi.dot, psi.j)
  # Apparently I can't pass the D's attributes as an argument to b.df
  attr(b.df, "nIterations") <- attributes(D)$nIterations
  # todo, try to make it pass properly
  b.df <- cbind(b.df, nIterations=attributes(D)$nIterations)
  B <- suppressWarnings(ddply(b.df, .(Parameter), summarize, 
    B=var(psi.j-psi.dot)*nIterations,
    #B=var(psi.j-psi.dot)*(attributes(D)$nIterations),
    .parallel=attributes(D)$parallel))
  # Compute within-sequence variance using s2j
  s2j <- suppressWarnings(ddply(D, .(Parameter, Chain), summarize, s2j=var(value),
    .parallel=attributes(D)$parallel))
  W <- suppressWarnings(ddply(s2j, .(Parameter), summarize, W=mean(s2j),
    .parallel=attributes(D)$parallel))
  # Merge BW and compute the weighted average (wa, var.hat+) and the Rhat
  # todo, again try to pass it properly
  BW <- merge(B, W)
  BW <- cbind(BW, nIterations=attributes(D)$nIterations)
  BW <- suppressWarnings(ddply(BW, .(Parameter), transform, 
    wa=( 
      #(((attributes(D)$nIterations-1)/attributes(D)$nIterations )* W) + 
      #((1/ attributes(D)$nIterations)*B) ),
      (((nIterations-1)/nIterations )* W) + 
      ((1/ nIterations)*B) ),
    .parallel=attributes(D)$parallel))
  BW <- suppressWarnings(ddply(BW, .(Parameter), transform, Rhat=sqrt(wa/W),
    .parallel=attributes(D)$parallel))
  # Plot
  f <- ggplot(BW, aes(x=Rhat, y=Parameter)) + geom_point() +
    ggtitle("Potential Scale Reduction Factor")
  return(f)
}
