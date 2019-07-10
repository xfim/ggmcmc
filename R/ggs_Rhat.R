#' Dotplot of Potential Scale Reduction Factor (Rhat)
#'
#' Plot a dotplot of Potential Scale Reduction Factor (Rhat), proposed by Gelman and Rubin (1992). The version from the second edition of Bayesian Data Analysis (Gelman, Carlin, Stern and Rubin) is used, but the version used in the package "coda" can also be used (Brooks & Gelman 1998).
#'
#' Notice that at least two chains are required.
#'
#' @references Fernández-i-Marín, Xavier (2016) ggmcmc: Analysis of MCMC Samples and Bayesian Inference. Journal of Statistical Software, 70(9), 1-20. doi:10.18637/jss.v070.i09
#' @references Gelman, Carlin, Stern and Rubin (2003) Bayesian Data Analysis. 2nd edition. Chapman & Hall/CRC, Boca Raton.
#' @references Gelman, A and Rubin, DB (1992) Inference from iterative simulation using multiple sequences, _Statistical Science_, *7*, 457-511.
#' @references Brooks, S. P., and Gelman, A. (1998). General methods for monitoring convergence of iterative simulations. _Journal of computational and graphical statistics_, 7(4), 434-455.
#' @param D Data frame whith the simulations
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc).
#' @param scaling Value of the upper limit for the x-axis. By default, it is 1.5, to help contextualization of the convergence. When 0 or NA, the axis are not scaled.
#' @param greek Logical value indicating whether parameter labels have to be parsed to get Greek letters. Defaults to false.
#' @param version_rhat Character variable with the name of the version of the potential scale reduction factor to use. Defaults to "BDA2", which refers to the second version of "bayesian Data Analysis (Gelman, Carlin, Stern and Rubin). The other available version is "BG98", which refers to Brooks & Gelman (1998) and is the one used in the "coda" package.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' data(linear)
#' ggs_Rhat(ggs(s))
ggs_Rhat <- function(D, family = NA, scaling = 1.5, greek = FALSE, version_rhat = "BDA2") {
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
  # Choose which version of the estimator to produce
  if (version_rhat == "BDA2") {
    # Merge BW and compute the weighted average (wa, var.hat+) and the Rhat
    BW <- BW %>%
      dplyr::mutate(
        wa= ((W * (attributes(D)$nIterations - 1)) / attributes(D)$nIterations ) +
          (B / attributes(D)$nIterations)) %>%
      dplyr::mutate(Rhat = sqrt(wa / W))
  } else if (version_rhat == "BG98") {
    # Follow coda's version
    # Not very elegant, as it uses matrix-like structures in some steps
    Rhat.fixed <- (attributes(D)$nIterations - 1) / attributes(D)$nIterations
    Xbar <- D %>%
      dplyr::group_by(Chain, Parameter) %>%
      dplyr::summarize(Xbar = mean(value))
    muhat <- Xbar %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Parameter) %>%
      dplyr::summarize(muhat = mean(Xbar))
    muhat <- muhat$muhat
    var.B <- B %>%
      dplyr::mutate(var.B = (2 * B^2) / (attributes(D)$nChains - 1)) %>%
      dplyr::select(-B)
    var.W <- s2j %>%
      dplyr::group_by(Parameter) %>%
      dplyr::summarize(var.W = var(s2j) / attributes(D)$nChains)
    var11 <- s2j %>%
      tidyr::spread(Chain, s2j)
    var12 <- Xbar %>%
      dplyr::mutate(Xbar.sq = Xbar^2) %>%
      dplyr::select(-Xbar) %>%
      tidyr::spread(Chain, Xbar.sq)
    var1 <- var(t(var11[,-1]), t(var12[,-1]))
    var21 <- var11
    var22 <- Xbar %>%
      tidyr::spread(Chain, Xbar)
    var2 <- var(t(var21[,-1]), t(var22[,-1]))
    cov.WB <- (attributes(D)$nIterations / attributes(D)$nChains) * diag(var1 - 2 * muhat * var2)
    cov.WB <- data.frame(Parameter = var11$Parameter, cov.WB = cov.WB)
    V <- dplyr::inner_join(W, B, by = "Parameter") %>%
      dplyr::mutate(V = ((attributes(D)$nIterations - 1) * W / attributes(D)$nIterations +
      (1 + 1 / attributes(D)$nChains) * B / attributes(D)$nIterations)) %>%
      dplyr::select(Parameter, V)
    R <- dplyr::inner_join(cov.WB, W, by = "Parameter") %>%
      dplyr::inner_join(B, by = "Parameter") %>%
      dplyr::inner_join(var.B, by = "Parameter") %>%
      dplyr::inner_join(var.W, by = "Parameter") %>%
      dplyr::mutate(var.V = ((attributes(D)$nIterations - 1)^2 *
              var.W + (1 + 1/attributes(D)$nChains)^2 *
              var.B + 2 * (attributes(D)$nIterations - 1) *
              (1 + 1/attributes(D)$nChains) * cov.WB) / attributes(D)$nIterations^2)
    DF <- dplyr::inner_join(V, R, by = "Parameter") %>%
      dplyr::mutate(df.V = (2 * V^2) / var.V) %>%
      dplyr::mutate(df.adj = (df.V + 3) / (df.V + 1)) %>%
      dplyr::select(Parameter, df.adj)
    BW <- dplyr::inner_join(BW, DF, by = "Parameter") %>%
      dplyr::mutate(Rhat.random = (1 + (1 / attributes(D)$nChains)) *
                      (1 / attributes(D)$nIterations) * (B / W)) %>%
      dplyr::mutate(Rhat.fixed = (attributes(D)$nIterations - 1) / attributes(D)$nIterations) %>%
      dplyr::mutate(Rhat2.estimate = Rhat.fixed + Rhat.random) %>%
      dplyr::mutate(Rhat = sqrt(df.adj * Rhat2.estimate)) %>%
      dplyr::select(Parameter, Rhat)
  } else {
    stop("The version of the Rhat to use is not known.")
  }

  # For parameters that do not vary, Rhat is Nan. Move it to NA
  BW$Rhat[is.nan(BW$Rhat)] <- NA
  # Plot
  f <- ggplot(BW, aes(x=Rhat, y=Parameter)) + geom_point() +
    xlab(expression(hat("R"))) + ggtitle("Potential Scale Reduction Factors")
  if (greek) {
    f <- f + scale_y_discrete(labels = parse(text = as.character(BW$Parameter)))
  }
  # If scaling, add the scale
  if (!is.na(scaling)) {
    # Use the maximum of Rhat if it is larger than the prespecified value
    scaling <- ifelse(scaling > max(BW$Rhat, na.rm=TRUE), scaling, max(BW$Rhat, na.rm=TRUE))
    f <- f + xlim(min(BW$Rhat, na.rm=TRUE), scaling)
  }
  return(f)
}
