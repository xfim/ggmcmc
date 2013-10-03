#' Wrapper function that creates a single pdf file with all plots that ggmcmc can produce.
#'
#' \code{ggmcmc()} is simply a wrapper function that generates a pdf file with all the potential plots that the package can produce.
#'
#' Notice that caterpillar plots are only created when there are multiple parameters within the same family. A family of parameters is considered to be all parameters that have the same name (usually the same greek letter) but different number within square brackets (such as alpha[1], alpha[2], ...).
#'
#' @param D Data frame whith the simulations, previously arranged using \code{\link{ggs}}
#' @param file Character vector with the name of the file to create. Defaults to "ggmcmc-output.pdf". When NULL, no pdf device is opened or closed. This allows the user to work with an opened pdf (or other) device.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @param plot character vector containing the names of the desired plots. By default (NULL), \code{ggmcmc()} plots \code{ggs_histogram()}, \code{ggs_density()}, \code{ggs_traceplot()}, \code{ggs_running()}, \code{ggs_compare_partial()}, \code{ggs_autocorrelation()}, \code{ggs_crosscorrelation()}, \code{ggs_Rhat()}, \code{ggs_geweke()} and \code{ggs_caterpillar()}.
#' @param param_page Numerical, number of parameters to plot for each page. Defaults to 5.
#' @param width Width of the pdf display, in inches. Defaults to 7.
#' @param height Height of the pdf display, in inches. Defaults to 10.
#' @param ... Other options passed to the pdf device.
#' @export
#' @examples
#' data(samples)
#' ggmcmc(ggs(S))  # Directly from a coda object
ggmcmc <- function(D, file="ggmcmc-output.pdf", family=NA, plot=NULL,
  param_page=5, width=7, height=10, ...) {
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }

  # Get the number of parameters
  n.param <- length(unique(D$Parameter))

  ## Open the pdf device
  if (!is.null(file)) {
    pdf(file, width=width, height=height)
  }

  # When there are fewer parameters to plot than the number of requested
  # parameters per page, simply print them. Otherwise, the device needs to be
  # arranged
  if (attributes(D)$nParameters <= param_page) {
    # Simply print each plot separately
    if (is.null(plot) | length(grep("histogram", plot)) > 0) {
      cat("Plotting histograms\n")
      print(ggs_histogram(D))
    }

    if (is.null(plot) | length(grep("density", plot)) > 0) {
      cat("Plotting density plots\n")
      print(ggs_density(D))
    }

    if (is.null(plot) | length(grep("traceplot", plot)) > 0) {
      cat("Plotting traceplots\n")
      print(ggs_traceplot(D))
    }

    if (is.null(plot) | length(grep("running", plot)) > 0) {
      cat("Plotting running means\n")
      print(ggs_running(D))
    }

    if (is.null(plot) | length(grep("compare_partial", plot)) > 0) {
      cat("Plotting comparison of partial and full chain\n")
      print(ggs_compare_partial(D))
    }

    if (is.null(plot) | length(grep("autocorrelation", plot)) > 0) {
      cat("Plotting autocorrelation plots\n")
      print(ggs_autocorrelation(D))
    }

  } else {
    # Arrange manually the plots to fit in the pages
    # Create a new variable that sets each parameter in a specified page number
    # Preserve the attributes of the original object
    old.atrib <- attributes(D)
    n.pages <- ceiling(n.param / param_page)
    parameters <- unique(D$Parameter)
    D.parameters <- data.frame(Parameter=parameters, 
      page=as.numeric(as.character(gl(n.pages, param_page, length=length(parameters)))))
    new.atrib <- old.atrib
    D <- merge(D, D.parameters)
    # In case merge has changed the order of the columns of the dataframe,
    # arrange it
    D <- D[,c(old.atrib$names, "page")]
    new.atrib$names <- c(old.atrib$names, "page")
    attributes(D) <- new.atrib

    # Loop for every page and print only the parameters in that page
    # The following lines would be ideal, but they don't work yet
    #ddply(D, .variables="page", .fun=function(x) {
    #  print(ggs_density(x))
    #}, .parallel=TRUE)
    #ddply(D, .(page), .fun=ggs_histogram(D))

    # So just do it manually
    if (is.null(plot) | length(grep("histogram", plot)) > 0) {
      cat("Plotting histograms\n")
      for (p in 1:n.pages) {
        Dsub <- D[D$page==p,]
        Dsub$Parameter <- as.factor(as.character(Dsub$Parameter))
        attr(Dsub, "nParameters") <- length(unique(Dsub$Parameter))
        print(ggs_histogram(Dsub))
      }
    }

    if (is.null(plot) | length(grep("density", plot)) > 0) {
      cat("Plotting density plots\n")
      for (p in 1:n.pages) print(ggs_density(D[D$page==p,]))
    }

    if (is.null(plot) | length(grep("traceplot", plot)) > 0) {
      cat("Plotting traceplots\n")
      for (p in 1:n.pages) print(ggs_traceplot(D[D$page==p,]))
    }

    if (is.null(plot) | length(grep("running", plot)) > 0) {
      cat("Plotting running means\n")
      for (p in 1:n.pages) print(ggs_running(D[D$page==p,]))
    }

    if (is.null(plot) | length(grep("compare_partial", plot)) > 0) {
      cat("Plotting comparison of partial and full chain\n")
      for (p in 1:n.pages) print(ggs_compare_partial(D[D$page==p,]))
    }

    if (is.null(plot) | length(grep("autocorrelation", plot)) > 0) {
      cat("Plotting autocorrelation plots\n")
      for (p in 1:n.pages) print(ggs_autocorrelation(D[D$page==p,]))
    }

  }

  ##
  ## Print also the figures that can fit in a single page
  ##
  if (attributes(D)$nParameters > 1) {                    # only in case of more than one parameter
    if (is.null(plot) | length(grep("crosscorrelation", plot)) > 0) {
      cat("Plotting crosscorrelation plot\n")
      print(ggs_crosscorrelation(D))
    }
  }

  if (attributes(D)$nChain > 1) {                         # only in case of multiple chains
    if (is.null(plot) | length(grep("Rhat", plot)) > 0) {
      cat("Plotting Potential Scale Reduction Factors\n")
      print(ggs_Rhat(D))
    }
  }

  if (is.null(plot) | length(grep("geweke", plot)) > 0) {
    cat("Plotting Geweke Diagnostic\n")
    print(ggs_geweke(D))
  }

  if (is.null(plot) | length(grep("caterpillar", plot)) > 0) {
    # Caterpillar plots are only plotted for each of the repeated parameters
    cat("Plotting caterpillar plot\n")

    # get the names of the parameters without their number (parameter family)
    Parameter.family <- gsub("\\[.+\\]", "", D$Parameter)

    # Count how many members each family of parameters has, and only plot where
    # there is more than one parameter
    n.family.members <- apply(ifelse(table(D$Parameter, Parameter.family) > 0, 1, 0), 2, sum)
    for (f in unique(Parameter.family)) {
      if (n.family.members[f] > 1) {
        print(ggs_caterpillar(D, family=f, horizontal=TRUE) + labs(title=f))
      }
    }
  }

  # Close the pdf device
  if (!is.null(file)) {
    dev.off()
  }
}
