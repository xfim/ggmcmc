#' Subset a ggs object to get only the parameters with a given regular expression.
#'
#' Internal function used by the graphical functions to get only some of the parameters that follow a given regular expression.
#'
#' @param D Data frame with the data arranged and ready to be used by the rest of the ggmcmc functions. The dataframe has four columns, namely: Iteration, Parameter, value and Chain, and seven attributes: nChains, nParameters, nIterations, nBurnin, nThin, description and parallel.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @return D Data frame that is a subset of the given D dataset.
get_family <- function(D, family=NA) {
  if (!is.character(family) | length(family)!=1) {
    stop("family must be a character vector with a single element")
  }
  # Select only the family paramaters
  family.id.parameters <- grep(family, D$Parameter)
  D.sub <- D[family.id.parameters,]
  D.sub$Parameter <- factor(as.character(D.sub$Parameter))
  # Copy the same attributes to the new object, except the number of
  # parameters
  # Probably there's a cleaner way to do it
  attr(D.sub, "nChains") <- attributes(D)$nChains
  attr(D.sub, "nParameters") <- length(unique(D.sub$Parameter))
  attr(D.sub, "nIterations") <- attributes(D)$nIterations
  attr(D.sub, "nBurnin") <- attributes(D)$nBurnin
  attr(D.sub, "nThin") <- attributes(D)$nThin
  attr(D.sub, "description") <- attributes(D)$description
  attr(D.sub, "parallel") <- attributes(D)$parallel
  return(D=D.sub)
}

#' Spectral Density Estimate at Zero Frequency
#'
#' Compute the Spectral Density Estimate at Zero Frequency for a given chain.
#'
#' @param x A time series
#' @return A vector with the spectral density estimate at zero frequency
#' @export
sde0f <- function(x) {
  m.ar <- ar(x)
  v0 <- m.ar$var.pred / (1-sum(m.ar$ar))^2
  return(v0)
}
