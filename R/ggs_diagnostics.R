#' Formal diagnostics of convergence and sampling quality
#'
#' Get in a single tidy dataframe the results of the formal (non-visual) convergence analysis. Namely, the Geweke diagnostic (z, from ggs_geweke()), the Potential Scale Reduction Factor Rhat (Rhat, from ggs_Rhat()) and the number of effective independent draws (Effective, from ggs_effective()).
#'
#' Notice that at least two chains are required. Otherwise, only the Geweke diagnostic makes sense, and can be returned with its own function.
#'
#' @references Fernández-i-Marín, Xavier (2016) ggmcmc: Analysis of MCMC Samples and Bayesian Inference. Journal of Statistical Software, 70(9), 1-20. doi:10.18637/jss.v070.i09
#' @references Geweke, J. Evaluating the accuracy of sampling-based approaches to calculating posterior moments. In _Bayesian Statistics 4_ (ed JM Bernardo, JO Berger, AP Dawid and AFM Smith). Clarendon Press, Oxford, UK.
#' @references Gelman, Carlin, Stern and Rubin (2003) Bayesian Data Analysis. 2nd edition. Chapman & Hall/CRC, Boca Raton.
#' @references Gelman, A and Rubin, DB (1992) Inference from iterative simulation using multiple sequences, _Statistical Science_, *7*, 457-511.
#' @references Brooks, S. P., and Gelman, A. (1998). General methods for monitoring convergence of iterative simulations. _Journal of computational and graphical statistics_, 7(4), 434-455.
#' @references Gelman, Carlin, Stern, Dunson, Vehtari and Rubin (2014) Bayesian Data Analysis. 3rd edition. Chapman & Hall/CRC, Boca Raton.
#'
#' @param D Data frame whith the simulations
#' @param family Name of the family of parameters to return, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc).
#' @param version_rhat Character variable with the name of the version of the potential scale reduction factor to use. Defaults to "BDA2", which refers to the second version of _Bayesian Data Analysis_ (Gelman, Carlin, Stern and Rubin). The other available version is "BG98", which refers to Brooks & Gelman (1998) and is the one used in the "coda" package.
#' @param version_effective Character variable with the name of the version of the calculation to use. Defaults to "spectral", which refers to the simple version estimating the spectral density at frequency zero used in the "coda" package. An alternative version "BDA3" is provided, which refers to the third edition of Bayesian Data Analysis (Gelman, Carlin, Stern, Dunson, Vehtari and Rubin).
#' @param proportion Logical value whether to return the proportion of effective independent draws over the total (the default) or the number.
#' @include ggs_geweke.R
#' @include ggs_Rhat.R
#' @include ggs_effective.R
#' @seealso \code{\link{ggs_geweke}}, \code{\link{ggs_Rhat}} and \code{\link{ggs_effective}} for their respective options.
#' @return A \code{tidy} dataframe. 
#' @export
#' @examples
#' data(linear)
#' ggs_diagnostics(ggs(s))
ggs_diagnostics <- function(D, family = NA, version_rhat = "BDA2", version_effective = "spectral", proportion = TRUE) {
  if (attributes(D)$nChains<2) {
    stop("At least two chains are required")
  }
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  R <- ggmcmc::ggs_geweke(D, plot = FALSE) %>%
    tidyr::gather(Diagnostic, value, -Parameter, -Chain) %>%
    dplyr::bind_rows(
      ggmcmc::ggs_Rhat(D, version_rhat = version_rhat, plot = FALSE) %>%
        tidyr::gather(Diagnostic, value, -Parameter)) %>%
    dplyr::bind_rows(
      ggmcmc::ggs_effective(D, version_effective = version_effective, proportion = proportion, plot = FALSE) %>%
        tidyr::gather(Diagnostic, value, -Parameter)) %>%
    dplyr::mutate(Diagnostic = as.factor(Diagnostic))
  return(R)
}
