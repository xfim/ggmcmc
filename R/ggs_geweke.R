#' Dotplot of the Geweke diagnostic, the standard Z-score
#'
#' Dotplot of Geweke diagnostic.
#
#' @param D data frame whith the simulations.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc).
#' @param frac1 Numeric, proportion of the first part of the chains selected. Defaults to 0.1.
#' @param frac2 Numeric, proportion of the last part of the chains selected. Defaults to 0.5.
#' @param shadow_limit, logical. When TRUE (the default), a shadowed area between -2 and +2 is drawn.
#' @param greek Logical value indicating whether parameter labels have to be parsed to get Greek letters. Defaults to false.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' data(linear)
#' ggs_geweke(ggs(s))
ggs_geweke <- function(D, family=NA, frac1=0.1, frac2=0.5, shadow_limit=TRUE, greek=TRUE) {
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  # Check fractions, similar to geweke.diag function in coda
  if (!is.numeric(frac1) | !is.numeric(frac2)) {
    stop("Fractions must be numerical values.")
  }
  if (frac1 < 0 | frac1 > 1 | frac2 < 0 | frac2 > 1 | (frac1+frac2)>1) {
    stop("Fractions are not valid.")
  }
  # Take subsequences of the chains
  window.1 <- 1:trunc(attributes(D)$nIterations * frac1)
  window.2 <- trunc(attributes(D)$nIterations * frac2):attributes(D)$nIterations
  D.geweke.first <- dplyr::mutate(dplyr::filter(D, Iteration <= max(window.1)), part="first")
  D.geweke.last <- dplyr::mutate(dplyr::filter(D, Iteration <= max(window.2)), part="last")
  D.geweke <- dplyr::bind_rows(D.geweke.first, D.geweke.last)
  # Compute means, spectral densities and N's
  D.geweke <- D.geweke %>%
    dplyr::group_by (Parameter, Chain, part) %>%
    dplyr::summarize(m=mean(value), sde0f=sde0f(value), n=n())
  # Cast the dataframe in pieces to have the data arranged by parameter, chain
  # and first and last
  M <- D.geweke %>%
    dplyr::select(-sde0f, -n) %>%
    dplyr::ungroup() %>%
    tidyr::spread(part, m)
  N <- D.geweke %>%
    dplyr::select(-sde0f, -m) %>%
    dplyr::ungroup() %>%
    tidyr::spread(part, n)
  SDE0F <- D.geweke %>%
    dplyr::select(-m, -n) %>%
    dplyr::ungroup() %>%
    tidyr::spread(part, sde0f)
  # Reorganize the z scores
  Z <- data.frame(Parameter=M$Parameter, Chain=M$Chain,
    z= (M$first - M$last) /
      sqrt( (SDE0F$first/N$first) + (SDE0F$last/N$last) ) )
  # Check that there are no Inf values, otherwise raise a message and continue
  # having converted it into NA
  Zinf <- which(is.infinite(Z$z))
  if (length(Zinf) > 0) {
    for (nas in 1:length(Zinf)) {
      warning(paste(
        "Infinite value in the z score for Parameter ", Z$Parameter[Zinf[nas]],
        ", Chain ", Z$Chain[Zinf[nas]], ".", sep=""))
    }
    Z$z[Zinf] <- NA
  }
  # Calculate ranges of z-scores for plotting
  rz <- range(Z$z, na.rm=TRUE)
  rz[1] <- ifelse(rz[1] < -2.5, rz[1], -2.5)
  rz[2] <- ifelse(rz[2] > 2.5, rz[2], 2.5)
  # Plot
  f <- ggplot(Z, aes(x=z, y=Parameter, colour=as.factor(Chain))) +
    geom_point() + xlim(rz) + ggtitle("Geweke Diagnostics")
  # Apply shadow of extreme values
  if (!is.na(shadow_limit)) {
    gw <- data.frame(x=c(-2, 2, 2, -2), y=c(rep(0, 2), rep(attributes(D)$nParameters+1, 2)), g='A')
    f <- f + geom_polygon(data=gw, aes(x=x, y=y, group=g, colour="black", linetype=NA),
      alpha=0.2, show.legend=FALSE)
  }
  # Correct legend
  f <- f +
    scale_colour_discrete(name="Chain",
      breaks=as.factor(unique(Z$Chain)), labels=as.factor(unique(Z$Chain)))
  if (greek) {
    f <- f + scale_y_discrete(labels = parse(text = levels(Z$Parameter)))
  }
  return(f)
}
