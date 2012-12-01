ggs_separation <- function(ppd, data, xlab = "", ylab = "", title = "", labels = NULL)
  {
    if(is.vector(data) == FALSE)
      stop("The response variable argument is not a vector.")
    if(!(max(data) == 1 & min(data) == 0))
      stop("This plot is designed for binary response variables.")
    
    if(is.list(ppd) & !is.mcmc.list(ppd)) { #if user pases a list of ppds
      
#      if(length(labels) != length(ppd))
#        stop("The number of labels passed does not match the number of ppds.")

      ppd <- lapply(ppd, function(x) as.data.frame(t(as.matrix(x))))
#      check <- unique(unlist(lapply(ppd, length))) #find the length of each list element
#      if(length(check) != 1) #check to make sure all elements have the same length
#        stop("Posterior predictive distribution length mismatch.")
#      else if(length(check) != length(data)) #check lengths against response variable
#        stop("The length of one of the posterior prediction matrices does not match
#              the length of the response variable vector.")
      ppd <- lapply(ppd, rowMeans) #find ppd means
      ppd <- lapply(ppd, as.numeric)
      multi = TRUE

      for(i in 1:length(ppd)) {
        ppd[[i]] <- as.data.frame(ppd[[i]], row.names = NULL)
        names(ppd[[i]]) <- "ppd"
        ppd[[i]]$label = labels[i] #add label to each list element
        ppd[[i]]$data <- data
        ppd[[i]] <- ppd[[i]][order(ppd[[i]]$ppd, ppd[[i]]$label), ] #order ppd within labels
        ppd[[i]]$id = seq_along(ppd[[i]]$data) #add index for each list element
      }
      #bind and replicate data for each list element
      df <- do.call(rbind, ppd)
      names(df) <- c("ppd", "label", "response", "id")
    }
    
    else if(is.mcmc(ppd) | is.mcmc.list(ppd)) { #if user passes a single ppd
      multi = FALSE
      ppd <- as.matrix(ppd)
      if(length(data) != length(ppd[2,]))
        stop("The length of the posterior predictive distribution matrix does not match
              the length of the response variable vector.")
      ppd <- as.data.frame(t(ppd)) #transpose and cast ppd as a df
      ppd <- rowMeans(ppd) #find ppd mean
      ppd <- as.numeric(ppd)

      #create an index for the y variable and binds together id, y, and ppd
      df <- data.frame(response = as.integer(data), ppd)
      df <- df[order(df$ppd), ] #order df by y values
      df$id = seq_along(data) #create index
    }

    #create plot
    p <- ggplot(df, aes(x = id)) +
         geom_line(aes(x = id, y = response, color = "#E77471")) +
         geom_line(aes(x = id, y = ppd)) +
         xlab(xlab) + ylab(ylab) + ggtitle(title) + theme_bw() +
         scale_x_discrete(breaks = NULL) +
         scale_y_continuous() +
         theme(legend.position = "none", axis.text.x = element_blank())

    #add faceting if applicable
    if(multi == TRUE)
      p <- p + facet_grid(label ~ ., scale = "free", space = "free")
    
    return(p)
  }


test.out <- lapply(list(mod.ppd, base.ppd, null.ppd), function(x) as.data.frame(t(as.matrix(x))))
labels = c("m1", "m2", "m3")
test.out <- lapply(test.out, rowMeans)
test.out <- lapply(test.out, as.numeric)

for(i in 1:length(test.out)) {
  test.out[[i]] <- as.data.frame(test.out[[i]], row.names = NULL)
  test.out[[i]]$label <- labels[i]
  test.out[[i]]$data <- data$onset
  test.out[[i]]$id <- seq_along(data$onset)
}
test <- do.call(rbind, test.out)
names(test) <- c("ppd", "label", "response", "id")
