#' Caterpillar plot with thick and thin CI
#'
#' Caterpillar plots are plotted combining all chains for each parameter.
#'
#' @param D data frame whith the simulations or list of data frame with simulations
#' @param X data frame with two columns, Parameter and the value for the x location. Parameter must be a character vector with the same names that the parameters in the D object. 
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @param thick.ci vector of length 2 with the quantiles of the thick band for the credible interval
#' @param thin.ci vector of length 2 with the quantiles of the thin band for the credible interval
#' @param line plot a line indicating a concrete position, usually used to mark where zero is. By default do not plot any line.
#' @param horizontal logical value for the plot having horizontal or vertical lines. Vertical is the default.
#' @param labels vector of strings that matches the number of models in the list (if applicable), used for facet labels
#' @return a ggplot object
#' @export
#' @examples
#' data(samples)
#' ggs_caterpillar(ggs(S, parallel=FALSE))
ggs_caterpillar <- function(D, family=NA, X=NA, thick.ci=c(0.05, 0.95), thin.ci=c(0.025, 0.975),
                            line=NA, horizontal=FALSE, labels = NULL) {
  
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  # Manage adding a X dataframe, and check potential errors in X
  x.present <- FALSE
  if (class(X)=="data.frame") {
    # check number of observations
    if (dim(X)[1] != attributes(D)$nParameters) {
      stop("X has a different number of observations than the number of parameters to plot.")
    }
    # get the name of the numerical variable
    x.name <- names(X)[which(names(X)!="Parameter")]
    # check that the x is numerical
    if (!is.numeric(X[,which(names(X)==x.name)])) {
      stop("X has a non numeric variable to plot against. The variable different than 'Parameter' must be numeric.")
    }
    # Set x present to distinguish between plots against x or without
    x.present <- TRUE
  }
  # Passing the "probs" argument to quantile inside ddply inside a function
  # turns out to be really hard.
  # Apparently there is a bug in ddply that even Hadley does not know how to
  # solve
  # http://stackoverflow.com/questions/6955128/object-not-found-error-with-ddply-inside-a-function
  # One of the solutions, not elegant, is to assign qs globally (as well as
  # locally  for further commands in this function
  qs  <- qs <<- c(thin.low=thin.ci[1], thick.low=thick.ci[1], 
                  median=0.5, thick.high=thick.ci[2], thin.high=thin.ci[2])
  
  if(is.list(D)) { #check if obj 'D' is a list
    multi = TRUE #used later in plot call
    for(i in 1:length(D)) { #iterate over list elements
      dc <- ddply(D[[i]], .(Parameter), summarize,
                  q = quantile(value, probs = qs), qs = qs)
      dc$qs <- factor(dc$qs, labels = names(qs))
      dcm <- cast(dc, Parameter ~ qs, value = .(q))
      D[[i]] <- dcm #replace list element with transformed list element
    }

    if(length(labels) != length(D)) {
      stop("Label length does not match the model list length.")
    }
    dcm <- do.call(rbind,lapply(1:length(D), function(i)
                                if (length(D[[i]]) > 1)
                                cbind(D[[i]], label = labels[i])))
  }
  else if (!is.list(D)) { #if obj 'D' is not a list
    multi = FALSE
    dc <- ddply(D, .(Parameter), summarize, 
                q=quantile(value, probs=qs), qs=qs,
                .parallel=attributes(D)$parallel)
    dc$qs <- factor(dc$qs, labels=names(qs))
    dcm <- as.data.frame(cast(dc, Parameter ~ qs, value=.(q)))
  }
  # Plot, depending on continuous or categorical x
  if (!x.present) {
    f <- ggplot(dcm, aes(x=median, y=reorder(Parameter, median))) + geom_point() +
      geom_segment(aes(x=thick.low, xend=thick.high, yend=reorder(Parameter, median)), size=0.7) +
      geom_segment(aes(x=thin.low, xend=thin.high, yend=reorder(Parameter, median)), size=0.5) +
      xlab("HPD") + ylab("Parameter")
  }
  else {
    dcm <- merge(dcm, X)
    f <- ggplot(dcm, aes_string(x="median", y=x.name)) + geom_point() +
      geom_segment(aes_string(x="thick.low", xend="thick.high", yend=x.name), size=0.7) +
      geom_segment(aes_string(x="thin.low", xend="thin.high", yend=x.name), size=0.5) +
      xlab("HPD") + ylab(x.name)
  }
  if (horizontal == FALSE) {
    f <- f + coord_flip() 
  }
  if (multi == TRUE & horizontal == TRUE) 
    f <- f + facet_grid(label ~ ., scale = "free", space = "free")
  if (multi == TRUE & horizontal == FALSE)
    f <- f + facet_grid(. ~ label, scale = "free", space = "free")

  if (!x.present & horizontal==FALSE) {
    f <- f + theme(legend.position="none", axis.text.x=element_text(size=7, hjust=1, angle=90))
  } else {
    f <- f + theme(legend.position="none", axis.text.x=element_text(size=7, hjust=1))
  } 
  # Add a line to remark a specific point
  if (!is.na(line)) {
    f <- f + geom_vline(xintercept=line, linetype="dashed")
  }
  return(f)
}
