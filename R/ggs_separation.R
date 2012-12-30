#' Separation plot for models with binary response variables
#' @param ppd object of type mcmc or mcmc.list, which contains posterior predictions. The number of columns of each element must be the length of the response variable vector and the number of rows of each element of the object should be the number of MCMC iterations run
#' @param data a vector which contains the binary response variable
#' @param xlab label for the x-axis
#' @param ylab label for the y-axis
#' @param title title for the entire plot
#' @return A \code{ggplot} object
#' @export
#' @examples
#' ggs_separation(ppd, df$response)

ggs_separation <- function(ppd, data, xlab = "", ylab = "", title = "")
  {
    if(is.vector(data) == FALSE)
      stop("The response variable argument is not a vector.")
    if(!(max(data) == 1 & min(data) == 0))
      stop("This plot is designed for binary response variables.")
    if(class(ppd) != "mcmc.list")
      stop("The ppd argument is not an mcmc.list.")

    ppd <- as.data.frame(t(apply(ldply(ppd), 2, quantile, c(.5, .025, .975))))
    names(ppd) <- c("mean", "lo", "hi")
    ppd$response <- data

    ppd <- ppd[order(ppd$mean), ]
    ppd$id <- seq_along(data)
    row.names(ppd) <- NULL

    p <- ggplot(ppd, aes(x = id)) +
      geom_line(aes(x = id, y = mean)) +
      geom_ribbon(aes(y = mean, ymin = lo, ymax = hi), alpha = 0.25) +
      geom_vline(xintercept = which(ppd$response == 1), colour = "red") +
      scale_x_discrete(breaks = NULL) + 
      xlab(xlab) + ylab(ylab) + ggtitle(title) + 
      theme(legend.position = "none", axis.text.x = element_blank())
    
    return(p)
  }
