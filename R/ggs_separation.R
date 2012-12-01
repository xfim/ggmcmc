ggs_separation <- function(ppd, data, xlab = "", ylab = "", title = "")
  {
    if(is.mcmc(ppd) | is.mcmc.list(ppd)) #check inputs
      ppd <- as.matrix(ppd)
    if(is.matrix(ppd) == FALSE)
      stop("The posterior predictive distribution argument is not a matrix.")
    if(is.vector(data) == FALSE)
      stop("The response variable argument is not a vector.")
    if(!(max(data) == 1 & min(data) == 0))
      stop("This plot is designed for binary response variables.")
    if(length(data) != length(ppd[2,]))
      stop("The length of the posterior predictive distribution matrix does not match
            the length of the response variable vector.")
    
    ppd.df <- as.data.frame(t(ppd)) #transpose and cast ppd as a df
    ppd.mean <- rowMeans(ppd.df) #find mean prediction for each obs.
    ppd.quantiles <- apply(ppd.df, 1, quantile, probs = c(0.05, 0.90)) #find quantiles
    ppd.quantiles <- as.data.frame(t(as.matrix(ppd.quantiles)))
    ppd.df <- cbind(ppd.mean, ppd.quantiles)
    names(ppd.df) <- c("mean", "lo", "hi")
    ppd.df$mean <- as.numeric(ppd.df$mean)
    ppd.df$hi <- as.numeric(ppd.df$hi)
    ppd.df$lo <- as.numeric(ppd.df$lo)
    
    #create an index for the y variable and binds together id, y, and ppd
    df <- data.frame(response = as.integer(data), ppd.df)
    df <- df[order(df$mean), ] #order df by y values
    df$id = seq_along(data)
    p <- ggplot(df, aes(x = id)) +
         geom_line(aes(x = id, y = response, color = "#E77471")) +
         geom_line(aes(x = id, y = mean)) +
         xlab(xlab) + ylab(ylab) + ggtitle(title) + theme_bw() +
         scale_x_discrete(breaks = NULL) +
         theme(legend.position = "none", axis.text.x = element_blank())

    return(p)
  }
