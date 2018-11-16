#' Subset a ggs object to get only the parameters with a given regular expression.
#'
#' Internal function used by the graphical functions to get only some of the parameters that follow a given regular expression.
#'
#' @param D Data frame with the data arranged and ready to be used by the rest of the ggmcmc functions. The dataframe has four columns, namely: Iteration, Parameter, value and Chain, and six attributes: nChains, nParameters, nIterations, nBurnin, nThin and description.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc).
#' @return D Data frame that is a subset of the given D dataset.
get_family <- function(D, family=NA) {
  if (!is.character(family) | length(family)!=1) {
    stop("family must be a character vector with a single element")
  }
  # Select only the family paramaters
  family.id.parameters <- grep(family, D$Parameter)
  D.sub <- D[family.id.parameters,]
  D.sub$Parameter <- droplevels(D.sub$Parameter)
  # Copy the same attributes to the new object, except the number of
  # parameters
  # Probably there's a cleaner way to do it
  attr(D.sub, "nChains") <- attributes(D)$nChains
  attr(D.sub, "nParameters") <- length(unique(D.sub$Parameter))
  attr(D.sub, "nIterations") <- attributes(D)$nIterations
  attr(D.sub, "nBurnin") <- attributes(D)$nBurnin
  attr(D.sub, "nThin") <- attributes(D)$nThin
  attr(D.sub, "description") <- attributes(D)$description
  return(D=D.sub)
}

#' Spectral Density Estimate at Zero Frequency.
#'
#' Compute the Spectral Density Estimate at Zero Frequency for a given chain.
#'
#' Internal function to compute the Spectral Density Estimate at Zero Frequency for a given chain used by \code{\link{ggs_geweke}}.
#'
#' @param x A time series
#' @return A vector with the spectral density estimate at zero frequency
#' @export
sde0f <- function(x) {
  # In case of series not varying, set v0 to 0
  if (length(unique(x))>1) {
    m.ar <- ar(x)
    v0 <- m.ar$var.pred / (1-sum(m.ar$ar))^2
  } else {
    v0 <- 0
  }
  return(v0)
}

#' Calculate binwidths by parameter, based on the total number of bins.
#'
#' Compute the minimal elements to recreate a histogram manually by defining the total number of bins.
#'
#' Internal function to compute the minimal elements to recreate a histogram manually by defining the total number of bins, used by \code{\link{ggs_histogram}} \code{\link{ggs_ppmean}} and \code{\link{ggs_ppsd}}.
#'
#' @param x any vector or variable
#' @param bins the number of requested bins
#' @return A data frame with the x location, the width of the bars and the number of observations at each x location.
#' @export
calc_bin <- function(x, bins=bins) {
  mn <- min(x)
  mx <- max(x)
  bw <- (mx-mn)/bins
  z <- seq(mn, mx, by=bw)
  if (length(z)==1) { # no variation on the parameter, manual table
    return(data.frame(x=c(mn-1, mn, mn+1), width=rep(1, 3), count=c(0, length(x), 0)))
  } else {
    count <- as.vector(table(cut(x, breaks=z)))
    return(data.frame(x=z[-length(z)], width=bw, count=count))
  }
}

#' Generate a factor with unequal number of repetitions.
#'
#' Generate a factor with levels of unequal length.
#'
#' Internal function to generate a factor with levels of unequal length, used by \code{\link{ggs_histogram}}.
#'
#' @param n number of levels
#' @param k number of repetitions
#' @param labels optional vector of labels
#' @return A factor
#' @export
gl_unq <- function (n, k, labels=1:n) {
  x <- NULL
  for (i in 1:n) {
    x <- append(x, rep(i, length.out=k[i]))
  }
  x <- factor(x, levels=1:n, labels=labels)
  return(x)
}

#' Calculate Credible Intervals (wide and narrow).
#'
#' Generate a data frame with the limits of two credible intervals. Function used by \code{\link{ggs_caterpillar}}. "low" and "high" refer to the wide interval, whereas "Low" and "High" refer to the narrow interval. "median" is self-explanatory and is used to draw a dot in caterpillar plots. The data frame generated is of wide format, suitable for ggplot2::geom_segment().
#'
#' @param D Data frame whith the simulations.
#' @param thick_ci Vector of length 2 with the quantiles of the thick band for the credible interval
#' @param thin_ci Vector of length 2 with the quantiles of the thin band for the credible interval
#' @return A data frame tbl with the Parameter names and 5 variables with the limits of the credibal intervals (thin and thick), ready to be used to produce caterpillar plots.
#' @export
#' @examples
#' data(linear)
#' ci(ggs(s))
ci <- function (D, thick_ci=c(0.05, 0.95), thin_ci=c(0.025, 0.975)) {
  # See ggs_autocorrelation.R
  # No way to make the following use summarize(), as of dplyr 0.3
  # https://github.com/hadley/dplyr/issues/154
  # Temporary workaround using dplyr 0.2 and do()
  q <- data.frame(
    qs=c("low", "Low", "median", "High", "high"),
    q=c(thin_ci[1], thick_ci[1], 0.5, thick_ci[2], thin_ci[2]))
  X <- D %>%
    dplyr::group_by(Parameter) %>%
    dplyr::do(data.frame(qs=q$qs, q=quantile(.$value, prob=q$q))) %>%
    dplyr::ungroup() %>%
    tidyr::spread(qs, q) %>%
    dplyr::select(Parameter, low, Low, median, High, high)
  # Recover the rest of the variables that can come with the par_labels
  if (dim(D)[2] > 4) {
    X <- suppressWarnings(dplyr::left_join(X, dplyr::distinct(dplyr::select(D, -Iteration, -Chain, -value)), by="Parameter"))
  }
  return(X)
}
