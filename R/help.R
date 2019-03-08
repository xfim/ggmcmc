#' Graphical tools for analyzing Markov Chain Monte Carlo simulations from Bayesian inference.
#'
#' ggmcmc is a tool for assessing and diagnosing convergence of
#' Markov Chain Monte Carlo simulations, as well as for graphically display
#' results from full MCMC analysis. The package also facilitates the graphical
#' interpretation of models by providing flexible functions to plot the
#' results against observed variables.
#'
#' @references \url{http://xavier-fim.net/packages/ggmcmc}.
#' @import ggplot2
#' @importFrom tidyr spread gather
#' @importFrom dplyr tbl_df data_frame select filter mutate arrange group_by ungroup summarize bind_rows left_join inner_join do sample_frac lag lead %>%
#' @docType package
#' @name ggmcmc
#' @aliases ggmcmc ggmcmc-package
NULL
