#' Separation plot for models with binary response variables
#'
#' @param D Data frame whith the simulations. Notice that only the posterior outcomes are needed, and so either the previous call to ggs() should have limited the family of parameters to pass to the predicted outcomes.
#' @param outcome vector (or matrix or array) containing the observed outcome variable. Currently only a vector is supported.
#' @param fully.bayesian logical, false by default. Currently not implemented
#'
#' @return A \code{ggplot} object
#' @references Greenhill, Ward and Sacks (2011): The separation plot: a new visual method for evaluating the fit of binary models. American Journal of Political Science, vol 55, number 4, pg 991-1002.
#' @examples
#' \dontrun{
#' ggs_separation(S, outcome=y)
#' }
#' @export

ggs_separation <- function(D, outcome, fully.bayesian=FALSE, minimalist=FALSE) {
  if (fully.bayesian) {
    stop("The fully Bayesian version has not been implemented yet.")
  }
  # Calculate the prediction bands
  if (fully.bayesian) {
  } else {
    S <- ddply(D, .(Parameter), summarize,
      low=quantile(value, 0.025),
      median=quantile(value, 0.5),
      high=quantile(value, 0.975),
      .parallel=attributes(D)$parallel)
  }
  S <- merge(S, data.frame(Observed=outcome, Parameter=unique(D$Parameter)))
  # Sort the observations by predicted value
  S <- S[order(S$median),]
  S <- cbind(S, id=1:dim(S)[1])
  # Calculate expected number of events
  if (fully.bayesian) {
  } else {
    N <- length(outcome)
    perc.obs.events <- length(which(outcome==1)) / N            # percent observed events
    ene <- length(which(S$median > perc.obs.events))
  }
  # Plot
  f <- ggplot(S, aes(x=id, y=median)) +
    geom_vline(xintercept=which(S$Observed==1), colour = "red", alpha=0.5, size=0.4)
  f <- f + geom_line() + geom_ribbon(aes(y=median, ymin =low, ymax=high), alpha = 0.25)
  f <- f + xlab("") + scale_x_discrete(breaks=NULL)
  f <- f + theme(legend.position = "none", axis.text.x=element_blank())
  f <- f + geom_point(data=data.frame(ene=ene, y=0), aes(x=ene, y=y), size=3, shape=17)
  if (minimalist) {
    # Plot a minimalist version of the figure to be used inline as suggested by
    # the authors citing Tufte.
    f <- f + scale_y_discrete(breaks=NULL) + theme(axis.text.y=element_blank()) + ylab("")
  } else {
    # Add more elements to the figure
    f <- f + ylab("Predicted")
  }
  return(f)
}
