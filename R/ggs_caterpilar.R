#' Caterpillar plot with thick and thin CI
#'
#' Caterpillar plots are plotted combining all chains for each parameter.
#'
#' @param D data frame whith the simulations
#' @param parameter.family Name of the family of parameters to plot. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). Not implemented.
#' @param thick.ci vector of length 2 with the quantiles of the thick band for the credible interval
#' @param thin.ci vector of length 2 with the quantiles of the thin band for the credible interval
#' @param line plot a line indicating a concrete position, usually used to mark where zero is. By default do not plot any line.
#' @param horizontal logical value for the plot being horizontal, which is the default
#' @return a ggplot object
#' @export
#' @examples
#' data(samples)
#' ggs_caterpillar(ggs(S, parallel=FALSE))
ggs_caterpillar <- function(D, parameter.family, thick.ci=c(0.05, 0.95), thin.ci=c(0.025, 0.975), line=NA, horizontal=TRUE) {

  # Passing the "probs" argument to quantile inside ddply inside a function
  # turns out to be really hard.
  # Apparently there is a bug in ddply that even Hadley does not know how to
  # solve
  # http://stackoverflow.com/questions/6955128/object-not-found-error-with-ddply-inside-a-function
  # One of the solutions, not elegant, is to assign qs globally (as well as
  # locally  for further commands in this function
  qs  <- qs <<- c(thin.low=thin.ci[1], thick.low=thick.ci[1], 
    median=0.5, 
    thick.high=thick.ci[2], thin.high=thin.ci[2])
  dc <- ddply(D, .(Parameter), summarize, 
    q=quantile(value, probs=qs), 
    qs=qs,
    .parallel=attributes(D)$parallel)
  dc$qs <- factor(dc$qs, labels=names(qs))
  dcm <- cast(dc, Parameter ~ qs, value=.(q))
  f <- ggplot(dcm, aes(x=median, y=reorder(Parameter, median))) + geom_point() +
    geom_segment(aes(x=thick.low, xend=thick.high, yend=reorder(Parameter, median)), size=0.7) +
    geom_segment(aes(x=thin.low, xend=thin.high, yend=reorder(Parameter, median)), size=0.5) +
    xlab("HPD") + ylab("Parameter") +
    theme(legend.position="none", axis.text.x=element_text(size=7, hjust=1))
  if (horizontal == FALSE) {
    f <- coord_flip()
  }
  # Add a line to remark a specific point
  if (!is.na(line)) {
    f <- f + geom_vline(xintercept=line, linetype="dashed")
  }
  return(f)
}
