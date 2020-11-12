#' Separation plot for models with binary response variables
#'
#' Plot a separation plot with the results of the model against a binary response variable.
#'
#' @references Fernández-i-Marín, Xavier (2016) ggmcmc: Analysis of MCMC Samples and Bayesian Inference. Journal of Statistical Software, 70(9), 1-20. doi:10.18637/jss.v070.i09
#' @references Greenhill B, Ward MD and Sacks A (2011). The separation plot: A New Visual Method for Evaluating the Fit of Binary Models. _American Journal of Political Science_, 55(4), 991-1002, doi:10.1111/j.1540-5907.2011.00525.x.
#' @param D Data frame whith the simulations. Notice that only the fitted / expected posterior outcomes are needed, and so either the previous call to ggs() should have limited the family of parameters to only pass the fitted / expected values. See the example below.
#' @param outcome vector (or matrix or array) containing the observed outcome variable. Currently only a vector is supported.
#' @param minimalist logical, FALSE by default. It returns a minimalistic version of the figure with the bare minimum elements, suitable for being used inline as suggested by Greenhill, Ward and Sacks citing Tufte.
#' @param show_labels logical, FALSE by default. If TRUE it adds the Parameter as the label of the case in the x-axis.
#' @param uncertainty_band logical, TRUE by default. If FALSE it removes the uncertainty band on the predicted values.
#'
#' @return A \code{ggplot} object
#' @references Greenhill, Ward and Sacks (2011): The separation plot: a new visual method for evaluating the fit of binary models. American Journal of Political Science, vol 55, number 4, pg 991-1002.
#' @export
#' @examples
#' data(binary)
#' ggs_separation(ggs(s.binary, family="mu"), outcome=y.binary)

ggs_separation <- function(D, outcome, minimalist = FALSE, show_labels = FALSE, uncertainty_band = TRUE) {
  # Calculate the prediction bands
  q <- data.frame(
    qs=c("low", "median", "high"),
    q=c(0.025, 0.5, 0.975))
  S <- D %>%
    dplyr::group_by(Parameter) %>%
    dplyr::do(data.frame(qs=q$qs, q=quantile(.$value, prob=q$q))) %>%
    dplyr::ungroup() %>%
    tidyr::spread(qs, q)
  # Sort the observations by predicted value
  S <- dplyr::inner_join(S, dplyr::tibble(Observed=outcome, Parameter=unique(D$Parameter)), by="Parameter") %>%
    dplyr::arrange(median) %>%
    dplyr::mutate(id=1:dim(S)[1])
  # Calculate expected number of events
  ene <- max(S$id) - sum(S$median)
  # Calculate the polygon for the bars with observed values
  bars_observed <- dplyr::filter(S, Observed == 1) %>%
    dplyr::mutate(y1 = 0, y2 = 1)
  # Plot
  f <- ggplot(S, aes(x=id, y=median)) +
    geom_rect(data = bars_observed,
      aes(xmin = id-0.5, xmax = id+0.5, ymin = 0, ymax = 1, group = id),
      fill = "red", alpha = 0.5)
  f <- f + geom_line()
  if (uncertainty_band) {
    f <- f + geom_ribbon(aes(ymin =low, ymax=high), alpha = 0.25)
  }
  f <- f + xlab("")
  f <- f + theme(legend.position = "none", axis.text.x=element_blank())
  f <- f + geom_point(data=data.frame(ene=ene, y=0), aes(x=ene, y=y), size=3, shape=17)
  if (show_labels) {
    f <- f + scale_x_continuous(breaks = S$id, labels = S$Parameter) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  } else {
    f <- f + scale_x_discrete(breaks = NULL)
  }
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
