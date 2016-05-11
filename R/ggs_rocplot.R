#' Calculate the ROC curve for a set of observed outcomes and predicted probabilities
#'
#' Internal function used by \code{\link{ggs_autocorrelation}}.
#'
#' @references Fernández-i-Marín, Xavier (2016) ggmcmc: Analysis of MCMC Samples and Bayesian Inference. Journal of Statistical Software, 70(9), 1-20. doi:10.18637/jss.v070.i09
#' @param R data frame with the 'value' (predicted probability) and the observed 'Outcome'.
#' @return A data frame with the Sensitivity and the Specificity.
#' @export
roc_calc <- function(R) {
  value <- R$value
  Observed <- R$Observed
  point <- seq(0, 1, length.out=dim(R)[1])
  tp <- sapply(point, function(x) nrow(R[value > x & Observed==1,]))   # True positives
  tn <- sapply(point, function(x) nrow(R[value < x & Observed==0,]))   # True negatives
  fp <- sapply(point, function(x) nrow(R[value > x & Observed==0,]))   # False positives
  fn <- sapply(point, function(x) nrow(R[value < x & Observed==1,]))   # False negatives
  return(data.frame(Sensitivity=tp/(tp+fn), Specificity=tn/(tn+fp)))
}

#' Receiver-Operator Characteristic (ROC) plot for models with binary outcomes
#'
#' @param D Data frame whith the simulations. Notice that only the posterior outcomes are needed, and so either the previous call to ggs() should have limited the family of parameters to pass to the predicted outcomes.
#' @param outcome vector (or matrix or array) containing the observed outcome variable. Currently only a vector is supported.
#' @param fully_bayesian logical, false by default. When not fully Bayesian, it uses the median of the predictions for each observation by iteration. When TRUE the function plots as many ROC curves as iterations. It uses a a lot of CPU and needs more memory. Use it with caution.
#'
#' @return A \code{ggplot} object
#' @export
#' @examples
#' data(binary)
#' ggs_rocplot(ggs(s.binary, family="mu"), outcome=y.binary)
ggs_rocplot <- function(D, outcome, fully_bayesian=FALSE) {
  # Create a single object that stores the predicted 'value' and the observed 'Outcome'
  D.observed <- dplyr::tbl_df(data.frame(Observed=outcome, Parameter=unique(D$Parameter)))
  # Work with the full posterior or with the expected medians, to economize memory
  if (fully_bayesian) {
    D.predicted <- D
  } else {
    D.predicted <- D %>%
      group_by(Parameter, Chain) %>%
      dplyr::summarize(value=quantile(value, 0.5)) %>%
      dplyr::ungroup()
  }
  roc.df <- dplyr::left_join(D.predicted, D.observed, by="Parameter")
  # Compute the roc curve using the roc_calc function
  roc.df <- dplyr::bind_cols(roc.df, roc_calc(dplyr::select(roc.df, value, Observed)))
  # Sort it to be sure that the figure is plotted nicely
  roc.df <- dplyr::tbl_df(roc.df) %>%
    dplyr::filter(Sensitivity, Specificity)
  # Plot differently if it's a fully Bayesian figure or not
  if (fully_bayesian) {
    # Start plotting
    f <- ggplot(roc.df, aes(x=1-Specificity, y=Sensitivity, group=as.factor(Iteration), color=as.factor(Chain))) +
      geom_step(alpha=0.4, size=0.4) + geom_rug(alpha=0.1, size=0.1, sides="b")
  } else {
    # Start plotting
    f <- ggplot(roc.df, aes(x=1-Specificity, y=Sensitivity, group=as.factor(Chain), color=as.factor(Chain))) +
      geom_step(alpha=0.4, size=1.2) + geom_rug(alpha=0.5, sides="b")
  }
  # Polish the details
  f <- f + geom_abline(intercept=0, slope=1, color="grey") + scale_colour_discrete(name="Chain")
  return(f)
}
