#' Create a plot matrix of posterior simulations
#'
#' Pairs style plots to evaluate posterior correlations among parameters.
#'
#' @param D Data frame with the simulations.
#' @param family Name of the family of parameters to plot, as given by a character vector or a regular expression. A family of parameters is considered to be any group of parameters with the same name but different numerical value between square brackets (as beta[1], beta[2], etc).
#' @param ... Arguments to be passed to \code{ggpairs}, including geom's \code{aes} (see examples)
#' @return A \code{ggpairs} object that creates a plot matrix consisting of univariate density plots on the diagonal, correlation estimates in upper triangular elements, and scatterplots in lower triangular elements.
#' @export
#' @importFrom GGally ggpairs
#' @examples
#' library(GGally)
#' data(linear)
#'
#' # default ggpairs plot
#' ggs_pairs(ggs(s))
#'
#' # change alpha transparency of points
#' ggs_pairs(ggs(s), lower=list(continuous = wrap("points", alpha = 0.2)))
#'
#' # with too many points, try contours instead
#' ggs_pairs(ggs(s), lower=list(continuous="density"))
#'
#' # histograms instead of univariate densities on diagonal
#' ggs_pairs(ggs(s), diag=list(continuous="barDiag"))
#'
#' # coloring results according to chains
#' ggs_pairs(ggs(s), mapping = aes(color = Chain))
#'
#' # custom points on lower panels, black contours on upper panels
#' ggs_pairs(ggs(s),
#'   upper=list(continuous = wrap("density", color = "black")),
#'   lower=list(continuous = wrap("points", alpha = 0.2, shape = 1)))
ggs_pairs <- function(D, family=NA, ...) {
  # Manage subsetting a family of parameters
  if (!is.na(family)) {
    D <- get_family(D, family=family)
  }
  if (attributes(D)$nParameters <= 1) {
    stop("Two or more parameters are required to make a pairs plot")
  }
  # Format data for GGally::ggpairs
  D_wide <- D %>%
    tidyr::spread(Parameter, value)
  bracket_names <- names(D_wide)
  names(D_wide) <- gsub("\\[|]|,", ".", names(D_wide))
  D_wide$Chain <- factor(D_wide$Chain)
  par_cols <- !(bracket_names %in% c("Iteration", "Chain"))
  # Plot
  f <- ggpairs(D_wide,
    columnLabels = bracket_names[par_cols],
    columns = which(par_cols), ...)
  return(f)
}
