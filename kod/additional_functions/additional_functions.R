#Additional functions for caret library

#---------------------------------------------------------------------------------------------------------------
#fiveStats function - designed for no-cost modeling (possible to generate class probalilities and calculate ROC)
# Calculate default statistics:
#     Accuracy (Accuracy SD)
#     Kappa (Kappa SD)
# and summary for two classes problem:    
#     AUC (AUC SD)
#     Sensitivity (sensitivity SD)
#     Specificity (Specificity SD)
fiveStats <- function(data, lev = levels(data$obs), model = NULL){
  c(twoClassSummary(data, lev = levels(data$obs), model = NULL),
    defaultSummary(data, lev = levels(data$obs), model = NULL))
}

#---------------------------------------------------------------------------------------------------------------
#fourStats function - designed for cost sensitive modeling (not possible to generate class probalilities and calculate ROC)
# Calculate default statistics:
#     Accuracy (Accuracy SD)
#     Kappa (Kappa SD)
# and summary for two classes problem:    
#     Sensitivity (sensitivity SD)
#     Specificity (Specificity SD)
fourStats <- function(data, lev = levels(data$obs), model = NULL){
  accKapp <- postResample(data[, "pred"], data[, "obs"])
  out <- c(accKapp,
           sensitivity(data[, "pred"], data[, "obs"], lev[1]),
           specificity(data[, "pred"], data[, "obs"], lev[2]))
  names(out)[3:4] <- c("Sens", "Spec")
  out
}
















