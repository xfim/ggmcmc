#' Dotplot of the effective number of independent draws
#'
#' Plot a dotplot of the effective number of independent draws. The version from the third edition of Bayesian Data Analysis (Gelman, Carlin, Stein, Dunson, Vehtari and Rubin) is used.
#'
#' Notice that at least two chains are required.
#'
#' @references Fernández-i-Marín, Xavier (2016) ggmcmc: Analysis of MCMC Samples and Bayesian Inference. Journal of Statistical Software, 70(9), 1-20. doi:10.18637/jss.v070.i09
#' @references Gelman, Carlin, Stern, Dunson, Vehtari and Rubin (2014) Bayesian Data Analysis. 3rd edition. Chapman & Hall/CRC, Boca Raton.
#' @param D Data frame whith the simulations
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc).
#' @param greek Logical value indicating whether parameter labels have to be parsed to get Greek letters. Defaults to false.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' data(linear)
#' ggs_effective(ggs(s))
ggs_effective <- function(D, family = NA, greek = FALSE) {
  if (attributes(D)$nChains<2) {
    stop("At least two chains are required")
  }
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  # The computations follow BDA, pg 298-299, and the notation tries to be
  # consistent with it
  # Compute between-sequence variance using psi.. and psi.j
  psi.dot <- D %>%
    dplyr::group_by(Parameter, Chain) %>%
    dplyr::summarize(psi.dot=mean(value))
  psi.j <- D %>%
    dplyr::group_by(Parameter) %>%
    dplyr::summarize(psi.j=mean(value))
  b.df <- dplyr::inner_join(psi.dot, psi.j, by="Parameter")
  B <- b.df %>%
    dplyr::group_by(Parameter) %>%
    dplyr::summarize(B=var(psi.j-psi.dot)*attributes(D)$nIterations)
  B <- unique(B)
  # Compute within-sequence variance using s2j
  s2j <- D %>%
    dplyr::group_by(Parameter, Chain) %>%
    dplyr::summarize(s2j=var(value))
  W <- s2j %>%
    dplyr::group_by(Parameter) %>%
    dplyr::summarize(W=mean(s2j))
  BW <- dplyr::inner_join(B, W, by="Parameter")
  # Merge BW and compute the weighted average (wa, var.hat+) and the nEffective
  BW <- BW %>%
    dplyr::mutate(
      wa= ((W * (attributes(D)$nIterations - 1)) / attributes(D)$nIterations ) +
        (B / attributes(D)$nIterations))
  # Calculate the variogram
  VG <- D %>%
    dplyr::group_by(Parameter, Chain) %>%
    dplyr::arrange(Parameter, Chain, Iteration) %>%
    dplyr::mutate(diff.sq = (value - dplyr::lag(value))^2) %>%
    dplyr::filter(Iteration != 1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Parameter) %>%
    dplyr::mutate(sum.diff.sq = sum(diff.sq)) %>%
    dplyr::mutate(variogram = (1 / (attributes(D)$nChains * (attributes(D)$nIterations - Iteration)) * sum.diff.sq))
  # Correlations
  Phat <- VG %>%
    dplyr::inner_join(dplyr::select(BW, Parameter, wa)) %>%
    dplyr::mutate(phat = (1 - (variogram / (2 * wa)))) %>%
    dplyr::ungroup()
  # Restrict phat to first lag where ac is negative
  sum.Phat <- Phat %>%
    dplyr::group_by(Parameter) %>%
    dplyr::arrange(Parameter, Iteration) %>%
    dplyr::mutate(psum = sum(phat, dplyr::lead(phat))) %>%
    dplyr::mutate(psum = ifelse(is.na(psum), phat, psum)) %>%
    dplyr::mutate(psum.negative = ifelse(psum < 0, 1, 0)) %>%
    dplyr::mutate(now.negative = psum.negative + dplyr::lag(psum.negative)) %>%
    dplyr::mutate(now.negative = ifelse(is.na(now.negative), 0, now.negative)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(now.negative == 0) %>%
    dplyr::group_by(Parameter) %>%
    dplyr::summarize(sum.phat = sum(phat))
  NE <- sum.Phat %>%
    dplyr::group_by(Parameter) %>%
    dplyr::summarize(nEffective = attributes(D)$nIterations * attributes(D)$nChains / (1 + 2 * sum.phat))

  # For parameters that do not vary, nEffective is Nan. Move it to NA
  NE$nEffective[is.nan(NE$nEffective)] <- NA
  # Plot
  f <- ggplot(NE, aes(x=nEffective, y=reorder(Parameter, nEffective))) + geom_point() +
    ylab("Parameter") +
    ggtitle("Number of effective independent draws")
  if (greek) {
    f <- f + scale_y_discrete(labels = parse(text = as.character(NE$Parameter)))
  }
  return(f)
}
