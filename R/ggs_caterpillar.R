#' Caterpillar plot with thick and thin CI
#'
#' Caterpillar plots are plotted combining all chains for each parameter.
#'
#' @references Fernández-i-Marín, Xavier (2016) ggmcmc: Analysis of MCMC Samples and Bayesian Inference. Journal of Statistical Software, 70(9), 1-20. doi:10.18637/jss.v070.i09
#' @param D Data frame whith the simulations or list of data frame with simulations. If a list of data frames with simulations is passed, the names of the models are the names of the objects in the list.
#' @param X data frame with two columns, Parameter and the value for the x location. Parameter must be a character vector with the same names that the parameters in the D object.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc).
#' @param thick_ci Vector of length 2 with the quantiles of the thick band for the credible interval
#' @param thin_ci Vector of length 2 with the quantiles of the thin band for the credible interval
#' @param line Numerical value indicating a concrete position, usually used to mark where zero is. By default do not plot any line.
#' @param horizontal Logical. When TRUE (the default), the plot has horizontal lines. When FALSE, the plot is reversed to show vertical lines. Horizontal lines are more appropriate for categorical caterpillar plots, because the x-axis is the only dimension that matters. But for caterpillar plots against another variable, the vertical position is more appropriate.
#' @param model_labels Vector of strings that matches the number of models in the list. It is only used in case of multiple models and when the list of ggs objects given at \code{D} is not named. Otherwise, the names in the list are used.
#' @param label Character value with the name of the variable that contains the labels displayed in the plot. Defaults to NULL, which corresponds to using the Parameter name or the Label in case par_labels is used in the ggs() object.
#' @param comparison Character value with the name of the variable that contains the focus of the comparison. Defaults to NULL, which corresponds to no comparison. It is not expected to be used together with X.
#' @param comparison_separation Numerical value with the separation between the dodged parameters. Defaults to 0.2.
#' @param greek Logical value indicating whether parameter labels have to be parsed to get Greek letters. Defaults to false.
#' @param sort Logical value indicating whether, in a horizontal display, y-axis labels must be sorted (the default) or not.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' data(linear)
#' ggs_caterpillar(ggs(s))
#' ggs_caterpillar(list(A=ggs(s), B=ggs(s))) # silly example duplicating the same model
ggs_caterpillar <- function(D, family=NA, X=NA,
  thick_ci=c(0.05, 0.95), thin_ci=c(0.025, 0.975),
  line=NA, horizontal=TRUE, model_labels=NULL, label = NULL, comparison = NULL, comparison_separation = 0.2, greek=FALSE, sort=TRUE) {

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

  # Multiple models or a single model
  if (!is.data.frame(D)) { # D is a list, and so multiple models are passed
    multi <- TRUE # used later in plot call
    dcm <- NULL
    for (i in 1:length(D)) { # iterate over list elements
      # Get model labels, by default the description attribute of the ggs object
      model.label <- attributes(D[[i]])$description
      # But prevalence is for the names of the named list, not for labels or for the description
      if (length(model_labels)==length(D)) model.label <- model_labels[i]  # get model labels from labels
      if (length(names(D)!=0)) model.label <- names(D)[i]                   # get model labels from named list

      # Transform list elements into wide dfs with thick and thin limits
      dcm <- dplyr::bind_rows(dcm, ci(D[[i]], thick_ci = thick_ci, thin_ci = thin_ci) %>%
        dplyr::mutate(Model=model.label))
    }

  } else if (is.data.frame(D)) { # D is a data frame, and so a single model is passed
    multi <-  FALSE
    dcm <- ci(D, thick_ci = thick_ci, thin_ci = thin_ci)
  }

  #
  # Plot, depending on continuous or categorical x
  #
  if (!x.present) {
    # Change the Labels used if needed
    if (!is.null(label)) {
      dcm$Parameter <- data.frame(dcm)[,label]
    }
    if (is.null(comparison)) {
      # It is easier to build the graph from scratch in every scenario
      # than try to recycle elements
      if (sort) {
        f <- ggplot(dcm, aes(x=median, y=reorder(Parameter, median), yend=reorder(Parameter, median))) + geom_point(size=3) +
          geom_segment(aes(x=Low, xend=High), size=1.5) +
          geom_segment(aes(x=low, xend=high), size=0.5)
      } else {
        f <- ggplot(dcm, aes(x=median, y=Parameter, yend=Parameter)) + geom_point(size=3) +
          geom_segment(aes(x=Low, xend=High), size=1.5) +
          geom_segment(aes(x=low, xend=high), size=0.5)
      }
      f <- f + xlab("HPD") + ylab("Parameter")
      if (greek) {
        f <- f + scale_y_discrete(labels = parse(text = as.character(dcm$Parameter[order(dcm$median)])))
      }
    } else {
      # This is a very specific set-up, where combinations with the rest of the function
      # arguments is not viable
      if (sort) {
        f <- ggplot(dcm, aes(x = reorder(Parameter, median), y = median,
                             group = get(comparison))) +
          geom_point(size=3, position = position_dodge(width = comparison_separation)) +
          geom_linerange(aes(ymin = Low, ymax = High), size=1.5, position = position_dodge(width = comparison_separation)) +
          geom_linerange(aes(ymin = low, ymax = high), size=0.5, position = position_dodge(width = comparison_separation))
      } else {
        f <- ggplot(dcm, aes(x = Parameter, y = median,
                             group = get(comparison))) +
          geom_point(size=3, position = position_dodge(width = comparison_separation)) +
          geom_linerange(aes(ymin = Low, ymax = High), size=1.5, position = position_dodge(width = comparison_separation)) +
          geom_linerange(aes(ymin = low, ymax = high), size=0.5, position = position_dodge(width = comparison_separation))
      }
      f <- f + ylab("HPD") + xlab("Parameter")
      if (horizontal == TRUE) {
        f <- f + coord_flip()
      }
      if (greek) {
        f <- f + scale_x_discrete(labels = parse(text = as.character(dcm$Parameter[order(dcm$median)])))
      }
    }
  } else {
    dcm <- merge(dcm, X)
    f <- ggplot(dcm, aes_string(x="median", y=x.name)) + geom_point(size=3) +
      geom_segment(aes_string(x="Low", xend="High", yend=x.name), size=1.5) +
      geom_segment(aes_string(x="low", xend="high", yend=x.name), size=0.5) +
      xlab("HPD") + ylab(x.name)
  }

  # Manage horizontal or vertical plot
  if (horizontal == FALSE && is.null(comparison)) {
    f <- f + coord_flip()
  }

  # Manage multiple models
  if (multi==TRUE & horizontal==TRUE)
    f <- f + facet_grid(Model ~ ., scales="free", space="free")
  if (multi==TRUE & horizontal==FALSE)
    f <- f + facet_grid(. ~ Model, scales="free", space="free")

  # Manage axis labels
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
