ggs_separation <- function(ppd, data)
  {
    if(is.mcmc(ppd) | is.mcmc.list(ppd)) #check inputs
      ppd <- as.matrix(ppd)
    if(is.matrix(ppd) == FALSE)
      stop("The posterior predictive distribution argument is not a matrix.")
    if(is.vector(data) == FALSE)
      stop("The response variable argument is not a vector.")
    if(length(data) != length(ppd[2,]))
      stop("The length of the posterior predictive distribution matrix does not match
            the length of the response variable vector.")
    ppd.df <- as.data.frame(t(ppd)) #transpose and cast ppd as a df
    ppd.mean <- rowMeans(ppd.df) #find mean prediction for each obs.
    ppd.quantiles <- apply(ppd.df, 1, quantile, probs = c(0.05, 0.90)) #find quantiles
    ppd.quantiles <- as.data.frame(t(as.matrix(ppd.quantiles)))
    ppd.df <- cbind(ppd.mean, ppd.quantiles)
    names(ppd.df) <- c("mean", "lo", "hi")
    
    #create an index for the y variable and binds together id, y, and ppd
    df <- data.frame(response = data, id = seq_along(data), ppd.df)
    df <- df[order(df$response), ] #order df by y values
    p <- ggplot(df, aes(id)) +
         geom_line(aes(x = id, y = response, color = "#E77471")) +
         geom_line(aes(x = id, y = mean)) +
         geom_line(aes(x = id, y = lo, linetype = "dashed")) +
         geom_line(aes(x = id, y = hi, linetype = "dashed")) +
         xlab("") + ylab("")
         
    return(p)
  }
