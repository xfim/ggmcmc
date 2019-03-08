# Hack to avoid NOTES in R CMD check
# Hadley does not seem to like it: http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
if (base::getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "Parameter", "Chain", "value", "Iteration", "ParameterOriginal", "Label",  # from ggs object
    "nIterations", # for ggs object attributes
    "wa", "Rhat", # from ggs_Rhat
    "Lag", "Autocorrelation", # from ggs_autocorrelation
    "low", "Low", "median", "High", "high", # for ci() in ggs_caterpillar
    "part_chain", # for ggs_compare_partial
    "Var1", "Var2", # for ggs_crosscorrelation
    "n", "z", "part", "g", # for ggs_geweke
    "k", # for gl_unq (with 'n')
    "sd", # for ggs_ppsd
    "x", "width", # for ggs_histogram
    "Posterior predictive mean", "Posterior predictive standard deviation", # for ggs_ppmean and ggs_ppsd
    "Specificity", "Sensitivity", "Observed", # for ggs_rocplot()
    "low", "high", "y", # for ggs_separation()
    "m", # for ggs_geweke, ggs_running, ggs_ppmean
    "qs", # for ggs_caterpillar(), ggs_separation()
    ".", # for several do() calls in ggs_histogram, ggs_autocorrelation, ggs_caterpillar, ggs_separation
    "ar", "cor", "quantile", "reorder", "var", # from stats
    "pdf", "dev.off", # from grDevices, for ggmcmc()
    "count", # for ggs_histogram(), for ggs_ppmean(), for ggs_ppsd()
    "id", # for ggs_separation()
    "read.table", # from utils, for ggs()
    "df.adj", "df.V", "first.m", "first.v", "last.m", "last.v",
    "Percent correctly predicted", "Rhat2.estimate", "Rhat.random",
    "statistic", "st.part", "unite", "var22", "var.V", "Xbar.sq", # for ggs_geweke and ggs_Rhat
    "Correct", "Percent", "correctly", "predicted",
    "diff.sq", "nEffective", "now.negative", "phat", "psum", "psum.negative", "sum.diff.sq",
    "sum.phat", "variogram", # from ggs_effective()
    "browseURL" # from utils, for ggmcmc()
  ))
}
