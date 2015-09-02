#' Dotplot of Potential Scale Reduction Factor (Rhat)
#'
#' Plot a dotplot of Potential Scale Reduction Factor (Rhat), proposed by Gelman and Rubin (1992). The version from the second edition of Bayesian Data Analysis (Gelman, Carlin, Stein and Rubin) is used.
#'
#' Notice that at least two chains are required.
#
#' @param D Data frame whith the simulations
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc).
#' @param scaling Value of the upper limit for the x-axis. By default, it is 1.5, to help contextualization of the convergence. When 0 or NA, the axis are not scaled.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' data(linear)
#' ggs_Rhat(ggs(s))
ggs_Rhat <- function(D, family=NA, scaling=1.5) {
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
  psi.dot <- D tidyr::%>%
    group_by(Parameter, Chain) tidyr::%>%
    dplyr::summarize(psi.dot=mean(value))
  psi.j <- D tidyr::%>%
    group_by(Parameter) tidyr::%>%
    dplyr::summarize(psi.j=mean(value))
  b.df <- dplyr::inner_join(psi.dot, psi.j, by="Parameter")
  B <- b.df tidyr::%>%
    group_by(Parameter) tidyr::%>%
    dplyr::summarize(B=var(psi.j-psi.dot)*attributes(D)$nIterations)
  B <- unique(B)
  # Compute within-sequence variance using s2j
  s2j <- D tidyr::%>%
    group_by(Parameter, Chain) tidyr::%>%
    dplyr::summarize(s2j=var(value))
  W <- s2j tidyr::%>%
    group_by(Parameter) tidyr::%>%
    dplyr::summarize(W=mean(s2j))
  # Merge BW and compute the weighted average (wa, var.hat+) and the Rhat
  BW <- dplyr::inner_join(B, W, by="Parameter") tidyr::%>%
    dplyr::mutate(
      wa= (((attributes(D)$nIterations-1)/attributes(D)$nIterations )* W) +
        ((1/ attributes(D)$nIterations)*B),
      Rhat=sqrt(wa/W))
  # For parameters that do not vary, Rhat is Nan. Move it to NA
  BW$Rhat[is.nan(BW$Rhat)] <- NA
  # Plot
  f <- ggplot(BW, aes(x=Rhat, y=Parameter)) + geom_point() +
    xlab(expression(hat("R"))) +
    ggtitle("Potential Scale Reduction Factor")
  # If scaling, add the scale
  if (!is.na(scaling)) {
    # Use the maximum of Rhat if it is larger than the prespecified value
    scaling <- ifelse(scaling > max(BW$Rhat, na.rm=TRUE), scaling, max(BW$Rhat, na.rm=TRUE))
    f <- f + xlim(min(BW$Rhat, na.rm=TRUE), scaling)
  }
  return(f)
}
