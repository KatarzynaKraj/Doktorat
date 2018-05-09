rm(list = ls(environment())) #clear environment
#cat("\014")  #clear console

library(caret)
library(C50)
library(doParallel)
library(xlsx)
registerDoParallel(12)

source("D:\\Doktorat\\kod\\additional_functions\\additional_functions.R")
#source("D:\\Doktorat\\kod\\additional_functions\\C50Cost_for_false_positives.R")

setwd("D:\\Doktorat\\dane")

#------------------------------------------------------------------------------------------------------------------
#Set data 
area <- "raba" #raba / pradnik
scale <- "jednoskalowe" #jednoskalowe / wieloskalowe
image_name <- "12_06_2010" #12_06_2010_28_08_2009 / 12_06_2010 / 28_08_2009
list_of_variants <- 1:8

#Set training process parameters
fitControl = trainControl(method = "repeatedcv", #The resampling method: "boot", "boot632", "optimism_boot", "boot_all", "cv", "repeatedcv", "LOOCV", "LGOCV" (for repeated training/test splits), "none" (only fits one model to the entire training set), "oob" (only for random forest, bagged trees, bagged earth, bagged flexible discriminant analysis, or conditional tree forest models), timeslice, "adaptive_cv", "adaptive_boot" or "adaptive_LGOCV"
                          number  = 5, #number of folds
                          repeats = 10, #number of repetitions of corssvalidation
                          #p = 0.2 #For leave-group out cross-validation: the training percentage
                          verboseIter = TRUE, #print training log
                          returnData = FALSE, #save data 
                          returnResamp = "all", #how much of resampled summary metrics should be saved
                          savePredictions = "all", #how much of the hold-out predictions should be saved
                          classProbs = TRUE, #a logical; should class probabilities be computed for classification models (along with predicted values) in each resample?
                          summaryFunction = fiveStats, #defaultSummary(data, lev = NULL, model = NULL), postResample(pred, obs), twoClassSummary(data, lev = NULL, model = NULL), mnLogLoss(data, lev = NULL, model = NULL), multiClassSummary(data, lev = NULL, model = NULL), prSummary(data, lev = NULL, model = NULL)
                          #sampling = "smote", #Method to deal with class imbalance / Aplied Predictive modeling p.427 / a single character value describing the type of additional sampling that is conducted after resampling (usually to resolve class imbalances). Values are "none", "down", "up", "smote", or "rose". The latter two values require the DMwR and ROSE packages, respectively. This argument can also be a list to facilitate custom sampling and these details can be found on the caret package website for sampling (link below).
                          #index = , #a list with elements for each resampling iteration. Each list element is a vector of integers corresponding to the rows used for training at that iteration.
                          #indexOut = , #a list (the same length as index) that dictates which data are held-out for each resample (as integers). If NULL, then the unique set of samples not contained in index is used.
                          allowParallel = TRUE)
#Set tuning parameters
tune_C50 = expand.grid(trials = c(50, 100), #boosting iterations 
                       model = c("rules", "tree"), #version of model
                       winnow = FALSE)#removing unimportant predictors
                       #cost = 1:10) #cost for False-Negatives (Positive class set always as first from levels; not possible to set cost for False Negatives)

#Classification -----------------------------------------------------------------------------------------------------------------------
# Read data
training_data = as.data.frame(read.csv(file = paste(area, "\\", scale, "\\treningowe_testowe\\landsat_klas_training.csv", sep = ""), 
                       header = TRUE, 
                       sep = ",",
                       dec = ".",
                       check.names = FALSE))
#Remove sample number
training_data = training_data[ ,-1]

#Calculate samples weights
model_weights <- ifelse(training_data$nieprz_proc == "nieprzepuszczalne",
                        (1/table(training_data$nieprz_proc)[1]) * 0.5,
                        (1/table(training_data$nieprz_proc)[2]) * 0.5)

#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#----
#------------------------------ 
#Creating matrix of additional C5.0 algorithm settings
options <- data.frame(pruning_parameter = c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE),
                      CF_parameter = c(0.1, 0.25, 0.1, 0.1, 0.1, 0.25),
                      fuzzyThreshold_parameter = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
for (variant_id in list_of_variants) {
  #Default settings for each variant of classification
  set.seed(100)
  C50_fit <- list(results = data.frame(),
                  pred = data.frame())
  
  #Print loop status
  print(paste("variant", variant_id, "is processed...", sep = " "))
  
  #Read variant data
  variant_directory <-  paste("D:\\Doktorat\\dane\\", area, "\\", scale, "\\warianty_klasyfikacji\\", image_name, "\\wariant_klasyfikacji_", toString(variant_id),".txt", sep = "")
  variant <- as.character(read.table(variant_directory, 
                                     header = FALSE, 
                                     sep = ",", 
                                     stringsAsFactors = FALSE ))
  variant_data <- training_data[ ,variant]

  #Loop for testing pruning_parameter, CF_parameter and fuzzyThreshold_parameter
  for (option_id in 1:6) {
    C50_fit_temp = train(x = variant_data[, -1],
                        y = variant_data[, 1],
                        method = "C5.0",
                        weights = model_weights, #define unequal weights for each sample to deal with class imbalance / Aplied Predictive modeling p.426
                        metric = "Sens",
                        #preProc = "center",
                        trControl = fitControl,
                        tuneGrid = tune_C50,
                        control = C50::C5.0Control(noGlobalPruning = options$pruning_parameter[option_id], #A logical to toggle whether the final, global pruning step to simplify the tree.
                                                  CF = options$CF_parameter[option_id], #A number in (0, 1) for the confidence factor. Confidence factor (CF) affects the way that error rates are estimated and lets you control the severity of pruning of the tree, meaning lower factor levels will likely prune away the leaves which overspecify the classification.
                                                  minCases = 2, #an integer for the smallest number of samples that must be put in at least two of the splits.
                                                  fuzzyThreshold = options$fuzzyThreshold_parameter[option_id])) #A logical toggle to evaluate possible advanced splits of the data. See Quinlan (1993) for details and examples. Quinlan R (1993). C4.5: Programs for Machine Learning. Morgan Kaufmann Publishers, http://www.rulequest.com/see5-unix.html
                                              
    C50_fit_temp$results$noGlobalPruning <- options$pruning_parameter[option_id]
    C50_fit_temp$results$CF <- options$CF_parameter[option_id]
    C50_fit_temp$results$fuzzyThreshold <- options$fuzzyThreshold_parameter[option_id]
    C50_fit_temp$results$variant <- variant_id
    
    C50_fit_temp$pred$noGlobalPruning <- options$pruning_parameter[option_id]
    C50_fit_temp$pred$CF <- options$CF_parameter[option_id]
    C50_fit_temp$pred$fuzzyThreshold <- options$fuzzyThreshold_parameter[option_id]
    C50_fit_temp$pred$variant <- variant_id
    
    C50_fit$results <- rbind(C50_fit$results, C50_fit_temp$results)
    C50_fit$pred <- rbind(C50_fit$pred, C50_fit_temp$pred)
  }
  #Save results
  workspace_name <- paste("C50_fit_", toString(variant_id), sep = "")
  assign(workspace_name, C50_fit)
 
  save(list = paste("C50_fit_", toString(variant_id), sep = ""),
       file = paste("D:\\Doktorat\\wyniki\\", area, "\\wyniki_kroswalidacja\\",scale, "\\Klasyfikacja\\C50\\", image_name, "\\wyniki_kroswalidacja_", toString(variant_id),".RData", sep = ""))
  
  write.csv(C50_fit$pred, 
             file = paste("D:\\Doktorat\\wyniki\\", area, "\\wyniki_kroswalidacja\\",scale, "\\Klasyfikacja\\C50\\", image_name, "\\pred_", toString(variant_id),".csv", sep = ""), 
             row.names = FALSE)

  write.csv(C50_fit$results, 
             file = paste("D:\\Doktorat\\wyniki\\", area, "\\wyniki_kroswalidacja\\",scale, "\\Klasyfikacja\\C50\\", image_name, "\\results_", toString(variant_id),".csv", sep = ""),
             row.names = FALSE)
  
  }

closeAllConnections();
#End of file