#' Manage the output from a coda object to be plotted by ggmcmc and convert it in an object that ggplot understands
#'
#' @param S mcmc object from samples
#' @param parallel logical value for using parallel computing when managing the data frame in other functions
#' @export
#' @return D data frame with the data arranged and ready to be used by the rest of the ggmcmc functions. The dataframe has four columns, namely: Iteration, Parameter, value and Chain, and four attributes: nChains, nParameters, nIterations, parallel.
#' @examples
#' # Assign 'D' to be a data frame suitable for ggmcmc functions from 
#' # a coda object called S
#' data(samples)
#' D <- ggs(S)        # S is a coda object
ggs <- function(S, parallel=TRUE) {
  lS <- length(S)
  D <- NULL
  if (lS == 1) {
    # Process a single chain]
    s <- S[1][[1]]
    D <- cbind(ggs_chain(s), Chain=1)
  } else {
    # Process multiple chains
    for (l in 1:lS) {
      s <- S[l][[1]]
      D <- rbind(D, cbind(ggs_chain(s), Chain=l))
    }
  }
  # Set several attributes to the object, to avoid computations afterwards
  attr(D, "nChains") <- lS
  attr(D, "nParameters") <- length(unique(D$Parameter))
  attr(D, "nIterations") <- max(D$Iteration)
  attr(D, "parallel") <- parallel
  return(D)
}

#' Manage each chain
#'
#' @param s a single chain to convert into a data frame
#' @return D data frame with the chain arranged
ggs_chain <- function(s) {
  # Get the names of the chains, the number of samaples and the vector of
  # iterations
  name.chains <- dimnames(s)[[2]]
  n.samples <- dim(s)[1]
  iter <- 1:n.samples

  # Prepare the dataframe
  d <- data.frame(Iteration=iter, as.matrix(unclass(s)))
  D <- melt(d, id.vars=c("Iteration"), variable_name="Parameter")

  # Revert the name of the parameters to their original names
  levels(D$Parameter) <- name.chains

  # Return the modified data frame
  return(D)
}
