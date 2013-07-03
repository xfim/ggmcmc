#' Calculate the ROC curve for a set of observed outcomes and predicted probabilities
#'
#' Internal function used by \code{\link{ggs_autocorrelation}}.
#'
#' @param R data frame with the 'value' (predicted probability) and the observed 'Outcome'.
#' @return A data frame with the Sensitivity and the Specificity.
#' @export
roc.calc <- function(R) {
  value <- R$value
  Observed <- R$Observed
  point <- seq(0, 1, length.out=dim(R)[1])
  tp <- sapply(point, function(x) nrow(R[value > x & Observed==1,]))   # True positives
  tn <- sapply(point, function(x) nrow(R[value < x & Observed==0,]))   # True negatives
  fp <- sapply(point, function(x) nrow(R[value > x & Observed==0,]))   # False positives
  fn <- sapply(point, function(x) nrow(R[value < x & Observed==1,]))   # False negatives
  return(data.frame(Sensitivity=tp/(tp+fn), Specificity=tn/(tn+fp)))
}

#' Receiver-Operator Characteristic (ROC) plot for models with binary dependent variables
#'
#' @param D Data frame whith the simulations. Notice that only the posterior outcomes are needed, and so either the ggs() call limits the parameters to the outcomes or the user provides a family of parameters to limit it.
#' @param outcome vector (or matrix or array) containing the observed outcome variable. Currently only a vector is supported.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc).
#' @param fully.bayesian logical, false by default. When not fully Bayesian, it uses the median of the predictions for each observation by iteration. When TRUE the function plots as many ROC curves as iterations. It uses a a lot of CPU and needs more memory. Use it with caution.
#'
#' @return A \code{ggplot} object
#' @examples
#' \dontrun{
#' ggs_rocplot(S, outcome=y)
#' }
ggs_rocplot <- function(D, outcome, fully.bayesian=FALSE) {
  # Create a single object that stores the predicted 'value' and the observed 'Outcome'
  D.observed <- data.frame(Observed=outcome, Parameter=unique(D$Parameter))
  # Work with the full posterior or with the expected medians, to economize memory
  if (fully.bayesian) {
    D.predicted <- D
  } else {
    D.predicted <- ddply(D, .(Parameter, Chain), summarize, value=quantile(value, 0.5),
      .parallel=attributes(D)$parallel)
  }
  roc.df <- merge(D.predicted, D.observed)
  # Compute the roc curve using the roc.calc function
  roc.df <- cbind(roc.df, roc.calc(roc.df[,c("value", "Observed")]))
  # Sort it to be sure that the figure is plotted nicely
  roc.df <- roc.df[order(roc.df$Sensitivity, roc.df$Specificity, decreasing=FALSE),]
  # Plot differently if it's a fully Bayesian figure or not
  if (fully.bayesian) {
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
