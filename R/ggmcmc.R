#' Wrapper for creating a single pdf file with all plots that ggmcmc can produce.
#'
#' Notice that caterpillar plots are only created when there are multiple parameters within the same family. A family of parameters is considered to be all parameters that have the same name (usually the same greek letter) but different number within square brackets (such as alpha[1], alpha[2], ...).
#'
#' @param D data frame whith the simulations, previously arranged using \code{\link{ggs}}
#' @param file name of the file to create with the plots. By default, use "ggmcmc-output.pdf"
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc). 
#' @param param.page numerical, number of parameters to plot for each page. Defaults to 5.
#' @param width width of the pdf display, in inches. Defaults to 7.
#' @param height of the pdf display, in inches. Defaults to 10.
#' @param ... other options for the pdf device
#' @export
#' @examples
#' data(samples)
#' ggmcmc(ggs(S, parallel=FALSE))  # Directly from a coda object
ggmcmc <- function(D, file="ggmcmc-output.pdf", family=NA, param.page=5, width=7, height=10, ...) {
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }

  # Get the number of parameters
  n.param <- length(unique(D$Parameter))

  ## Open the pdf device
  pdf(file, width=width, height=height)

  # When there are fewer parameters to plot than the number of requested
  # parameters per page, simply print them. Otherwise, the device needs to be
  # arranged
  if (attributes(D)$nParameters <= param.page) {
    # Simply print each plot separately
    cat("Plotting histograms\n")
    print(ggs_histogram(D))

    cat("Plotting density plots\n")
    print(ggs_density(D))

    cat("Plotting traceplots\n")
    print(ggs_traceplot(D))

    cat("Plotting running means\n")
    print(ggs_running(D))

    cat("Plotting comparison of partial and full chain\n")
    print(ggs_compare_partial(D))

    cat("Plotting autocorrelation plots\n")
    print(ggs_autocorrelation(D))

  } else {
    # Arrange manually the plots to fit in the pages
    # Create a new variable that sets each parameter in a specified page number
    # Preserve the attributes of the original object
    old.atrib <- attributes(D)
    n.pages <- ceiling(n.param / param.page)
    parameters <- unique(D$Parameter)
    D.parameters <- data.frame(Parameter=parameters, 
      page=as.numeric(as.character(gl(n.pages, param.page, length=length(parameters)))))
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
    cat("Plotting histograms\n")
    for (p in 1:n.pages) print(ggs_histogram(D[D$page==p,]))

    cat("Plotting density plots\n")
    for (p in 1:n.pages) print(ggs_density(D[D$page==p,]))

    cat("Plotting traceplots\n")
    for (p in 1:n.pages) print(ggs_traceplot(D[D$page==p,]))

    cat("Plotting running means\n")
    for (p in 1:n.pages) print(ggs_running(D[D$page==p,]))

    cat("Plotting comparison of partial and full chain\n")
    for (p in 1:n.pages) print(ggs_compare_partial(D[D$page==p,]))

    cat("Plotting autocorrelation plots\n")
    for (p in 1:n.pages) print(ggs_autocorrelation(D[D$page==p,]))

  }

  ##
  ## Print also the figures that can fit in a single page
  ##
  cat("Plotting crosscorrelation plot\n")
  print(ggs_crosscorrelation(D))

  cat("Plotting Potential Scale Reduction Factors\n")
  print(ggs_Rhat(D))

  cat("Plotting Geweke Diagnostic\n")
  print(ggs_geweke(D))

  ##
  ## Print caterpillar plots for each of the repeated parameters
  ##
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

  # Close the pdf device
  dev.off()
}
