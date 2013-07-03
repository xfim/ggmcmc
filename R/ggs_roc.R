#' Receiver-Operator Characteristic (ROC) plot for models with binary dependent variables
#'
#' @param D Data frame whith the simulations. Notice that only the posterior outcomes are needed, and so either the ggs() call limits the parameters to the outcomes or the user provides a family of parameters to limit it.
#' @param outcome vector (or matrix or array) containing the observed outcome variable. Currently only a vector is supported.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @param points integer giving the number of the cut-off points. Defaults to 21, which corresponds to a sequence between 0, 0.05, 0.10, 0.15, ..., 1. An increased precision results in more memory used, and it may easily consume it.
#' @param fully.bayesian logical, false by default. When TRUE the function plots as many ROC curves as iterations. It uses a huge amount of memory. Use it with caution. By default it estimates the median of the prediction by each observation.
#'
#' @param ppd object of type mcmc or mcmc.list which contains posterior predictions. May also be a list of objects of type mcmc or mcmc.list. If a single ppd is passed, then the number of columns of each element must be the length of the response variable vector and the number of rows of each element of the object should be the number of MCMC iterations run. If multiple models are passed as a list of mcmc or mcmc.list objects, the same must be true for each ppd within the list of ppds
#' @param data a vector which contains the binary response variable
#' @param xlab label for the x-axis
#' @param ylab label for the y-axis
#' @param labels labels for separate models
#' @return A \code{ggplot} object
#' @examples
#' \dontrun{
#' ggs_rocplot(S, outcome=y)
#' }
#ggs_rocplot <- function(ppd, data, xlab = "false positive rate",
#                        ylab = "true positive rate", title = "", labels) {
ggs_rocplot <- function(D, outcome, points=21, fully.bayesian=FALSE,
  xlab="False positive rate", ylab="True positive rate", labels) {

  # Create a single object that stores the predicted 'value', the observed
  # 'Outcome' and the cut-off point.
  D.observed <- data.frame(Observed=outcome, Parameter=unique(D$Parameter))
  # Work with the full posterior or with the expected medians, to economize memory
  if (fully.bayesian) {
    D.predicted <- D
  } else {
    D.predicted <- ddply(D, .(Parameter, Chain), summarize, value=quantile(value, 0.5),
      .parallel=attributes(D)$parallel)
  }
  roc.df <- merge(D.predicted, D.observed)
  roc.df <- merge(roc.df, data.frame(point=seq(0, 1, length.out=points)))
  roc.df <- cbind(roc.df, tp=roc.df$value > roc.df$point & roc.df$Observed==1)
  # Again, do the calculations for the true positives (tp) true negatives (tn),
  # ... separately, depending on whether iterations matter or not.
  # It replicates code, but it is the way to either use Iterations or not.
  if (fully.bayesian) {
    roc.df <- ddply(roc.df, .(Parameter, Iteration, Chain), transform,
      tp=value > point & Observed==1,       # True positives
      tn=value < point & Observed==0,       # True negatives
      fp=value > point & Observed==0,       # False positives
      fn=value < point & Observed==1,       # False negatives
      .parallel=attributes(D)$parallel)
    roc.summary <- ddply(roc.df, .(Chain, Iteration, point), summarize,
      Sensitivity=length(which(tp)) / (length(which(tp)) + length(which(fn))),
      Specificity=length(which(tn)) / (length(which(tn)) + length(which(fp))),
      .parallel=attributes(D)$parallel)
    roc.summary <- roc.summary[order(roc.summary$Sensitivity, roc.summary$Specificity, decreasing=FALSE),]
    # Start plotting
    f <- ggplot(roc.summary, aes(x=1-Specificity, y=Sensitivity, group=as.factor(Iteration), color=as.factor(Chain))) +
      geom_step(alpha=0.1)
  } else {
    roc.df <- ddply(roc.df, .(Parameter, Chain), transform,
      tp=value > point & Observed==1,
      tn=value < point & Observed==0,
      fp=value > point & Observed==0,
      fn=value < point & Observed==1,
      .parallel=attributes(D)$parallel)
    roc.summary <- ddply(roc.df, .(Chain, point), summarize,
      Sensitivity=length(which(tp)) / (length(which(tp)) + length(which(fn))),
      Specificity=length(which(tn)) / (length(which(tn)) + length(which(fp))),
      .parallel=attributes(D)$parallel)
    roc.summary <- roc.summary[order(roc.summary$Sensitivity, roc.summary$Specificity, decreasing=FALSE),]
    # Start plotting
    f <- ggplot(roc.summary, aes(x=1-Specificity, y=Sensitivity, group=as.factor(Chain), color=as.factor(Chain))) +
      geom_step(alpha=0.6, size=1.2)
  }
  # Polish the details
  f <- f + geom_rug(alpha=0.5, sides="b") + geom_abline(intercept=0, slope=1, color="grey") +
    scale_colour_discrete(name="Chain")
  return(f)
}
