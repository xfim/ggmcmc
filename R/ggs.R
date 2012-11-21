#' Manage the output from a coda object to be plotted by ggmcmc and convert it in an object that ggplot understands
#'
#' @param S either mcmc.list object with samples from JAGS, mcmc object with samples from MCMCpack. ggmcmc guesses what is the original object and tries to import it accordingly.
#' @param parallel logical value for using parallel computing when managing the data frame in other functions
#' @export
#' @return D data frame with the data arranged and ready to be used by the rest of the ggmcmc functions. The dataframe has four columns, namely: Iteration, Parameter, value and Chain, and four attributes: nChains, nParameters, nIterations, parallel.
#' @examples
#' # Assign 'D' to be a data frame suitable for ggmcmc functions from 
#' # a coda object called S
#' data(samples)
#' D <- ggs(S)        # S is a coda object
ggs <- function(S, parallel=TRUE) {
  if (class(S)=="mcmc.list" | class(S)=="mcmc") {  # JAGS typical output or MCMCpack
    lS <- length(S)
    D <- NULL
    if (lS == 1 | class(S)=="mcmc") { # Single chain or MCMCpack
      if (lS == 1 & class(S)=="mcmc.list") { # single chain
        s <- S[[1]]
      } else { # MCMCpack
        s <- S
      }
      # Process a single chain
      D <- cbind(ggs_chain(s), Chain=1)
      # Get information from mcpar (burnin period, thinning)
      nBurnin <- (attributes(s)$mcpar[1])-1
      nThin <- attributes(s)$mcpar[3]
    } else {
      # Process multiple chains
      for (l in 1:lS) {
        s <- S[l][[1]]
        D <- rbind(D, cbind(ggs_chain(s), Chain=l))
      }
      # Get information from mcpar (burnin period, thinning). Taking the last
      # chain is fine. All chains are assumed to have the same structure.
      nBurnin <- attributes(s)$mcpar[1]-1
      nThin <- attributes(s)$mcpar[3]
    }
    # Set several attributes to the object, to avoid computations afterwards
    # Number of chains
    attr(D, "nChains") <- lS
    # Number of parameters
    attr(D, "nParameters") <- length(unique(D$Parameter))
    # Number of Iterations really present in the sample
    attr(D, "nIterations") <- max(D$Iteration)
    # Number of burning periods previously
    attr(D, "nBurnin") <- nBurnin
    # Thinning interval
    attr(D, "nThin") <- nThin
    # Whether parallel computing is desired
    attr(D, "parallel") <- parallel
    return(D)
  } else {
    stop("ggs is not able to transform the input object into a ggs object suitable for ggmcmc.")
  }
}

#' Manage each chain
#'
#' @param s a single chain to convert into a data frame
#' @return D data frame with the chain arranged
ggs_chain <- function(s) {
  # Get the names of the chains, the number of samples and the vector of
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
