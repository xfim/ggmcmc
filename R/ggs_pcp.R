#' Plot for model fit of binary response variables: percent correctly predicted
#'
#' Plot a histogram with the distribution of correctly predicted cases in a model against a binary response variable.
#'
#' @param D Data frame whith the simulations. Notice that only the fitted / expected posterior outcomes are needed, and so either the previous call to ggs() should have limited the family of parameters to only pass the fitted / expected values. See the example below.
#' @param outcome vector (or matrix or array) containing the observed outcome variable. Currently only a vector is supported.
#' @param threshold numerical bounded between 0 and 1 or "observed", the default. If "observed", the threshold of expected values to be considered a realization of the event (1, succes) is computed using the observed value in the data. Otherwise, a numerical value showing which threshold to use (typically, 0.5) can be given.
#' @param bins integer indicating the total number of bins in which to divide the histogram. Defaults to 30, which is the same as geom_histogram()
#'
#' @return A \code{ggplot} object
#' @export
#' @examples
#' data(binary)
#' ggs_pcp(ggs(s.binary, family="mu"), outcome=y.binary)

ggs_pcp <- function(D, outcome, threshold = "observed", bins = 30) {
  # Clarify the threshold
  if (threshold == "observed") {
    threshold <- length(which(outcome == 1)) / length(outcome)
  } else {
    if (!is.numeric(threshold) | (is.numeric(threshold) & (threshold < 0 | threshold > 1))) {
      stop("Error: threshold must be either 'observed' or a number between 0 and 1")
    }
    threshold <- threshold
  }
  # Calculate the percent correctly predicted
  S <- dplyr::inner_join(D, dplyr::data_frame(Observed=outcome, Parameter=unique(D$Parameter)), by="Parameter") %>%
    dplyr::mutate(Correct = ifelse( (value < threshold & Observed == 0) | # 0 predicted and 0 observed
                             (value > threshold & Observed == 1), # 1 predicted and 1 observed
                              TRUE, FALSE)) %>%
    dplyr::group_by(Iteration, Chain) %>%
    dplyr::summarize(PCP = length(which(Correct)) / dplyr::n())
  # Adjust binwidth
  pcp.bw <- calc_bin(S$PCP, bins = bins)
  names(pcp.bw)[names(pcp.bw)=="x"] <- "Percent correctly predicted"
  # Plot
  f <- ggplot(pcp.bw, aes(x=`Percent correctly predicted`, y = count, width = width)) +
    geom_bar(stat = "identity", position = "identity") +
    expand_limits(x = c(0, 1))
  return(f)
}
