# Hack to avoid NOTES in R CMD check
# Hadley does not seem to like it: http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
if (base::getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "Parameter", "Chain", "value", "Iteration",   # from ggs object
    "nIterations", # for ggs object attributes
    "wa", "Rhat", # from ggs_Rhat
    "Lag", "Autocorrelation", # from ggs_autocorrelation
    "thick.low", "thick.high", "thin.low", "thin.high", # from ggs_caterpillar
    "part_chain", # for ggs_compare_partial
    "Var1", "Var2", # for ggs_crosscorrelation
    "n", "z", "part", # for ggs_geweke
    "m" # for ggs_geweke and ggs_running
  ))
}
