#' Subset a ggs object to get only the parameters with a given name. 
#'
#' @param D data frame with the data arranged and ready to be used by the rest of the ggmcmc functions. The dataframe has four columns, namely: Iteration, Parameter, value and Chain, and four attributes: nChains, nParameters, nIterations, parallel.
#' @param x character vector with the partial name to subset
#' @return D subset of the original D dataset given.
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
  attr(D.sub, "parallel") <- attributes(D)$parallel
  return(D=D.sub)
}

#' Spectral Density Estimate at Zero Frequency
#'
#' @param x a time series
#' @return
sde0f <- function(x) {
  m.ar <- ar(x)
  v0 <- m.ar$var.pred / (1-sum(m.ar$ar))^2
  return(v0)
}
