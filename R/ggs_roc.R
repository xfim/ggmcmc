#' Receiver-Operator Characteristic (ROC) plot for models with binary dependent variables
#' @param ppd object of type mcmc or mcmc.list which contains posterior predictions. May also be a list of objects of type mcmc or mcmc.list. If a single ppd is passed, then the number of columns of each element must be the length of the response variable vector and the number of rows of each element of the object should be the number of MCMC iterations run. If multiple models are passed as a list of mcmc or mcmc.list objects, the same must be true for each ppd within the list of ppds
#' @param data a vector which contains the binary response variable
#' @param xlab label for the x-axis
#' @param ylab label for the y-axis
#' @param title title for the entire plot
#' @param labels labels for separate models
#' @return A \code{ggplot} object
#' @examples
#' ggs_rocplot(ppd, df$response)
ggs_rocplot <- function(ppd, data, xlab = "false positive rate",
                        ylab = "true positive rate", title = "", labels) {

  roc.calc <- function(roc.df) { #expects df with 2 col, response, prob
    th <- seq(0, 1, length = length(roc.df[,1]))
    tn <- sapply(th, function(x) nrow(roc.df[roc.df[,2] < x & roc.df[,1] == 0,]))
    fn <- sapply(th, function(x) nrow(roc.df[roc.df[,2] < x & roc.df[,1] == 1,]))
    fp <- sapply(th, function(x) nrow(roc.df[roc.df[,2] > x & roc.df[,1] == 0,]))
    tp <- sapply(th, function(x) nrow(roc.df[roc.df[,2] > x & roc.df[,1] == 1,]))
    roc.plot <- data.frame(sen = tp / (tp + fn), spe = tn / (tn + fp))
    return(roc.plot)
  }

  if(grepl("mcmc", class(ppd))) { #if a single ppd is passed
    m.ppd <- colMeans(ldply(ppd))
    roc.df <- data.frame(response, prob = m.ppd)
    roc.plot <- roc.calc(roc.df)
    p <- ggplot(data = roc.plot, aes(x = 1 - spe, y = sen))
    p <- p + geom_line()
    p <- p + labs(x = xlab, y = ylab, title = title)
  }

  else if(class(ppd) == "list") { #if multiple ppds are passed
    if(!exists("labels", mode = "character"))
      labels <- paste("model", 1:length(ppd), sep = " ")

    m.ppd.list <- lapply(ppd, function(x) data.frame(response, prob = colMeans(ldply(x))))
    roc.plot.list <- lapply(m.ppd.list, roc.calc)
    for(i in 1:length(roc.plot.list))
      roc.plot.list[[i]]$label <- labels[i]
    roc.plot <- ldply(roc.plot.list)

    p <- ggplot(data = roc.plot, aes(x = 1 - spe, y = sen, group = label))
    p <- p + geom_line(aes(linetype = label))
    p <- p + labs(x = xlab, y = ylab, title = title)
    p <- p + theme(legend.title = element_blank())
  }

  else
    stop("unsupported ppd type")

  return(p)
}
