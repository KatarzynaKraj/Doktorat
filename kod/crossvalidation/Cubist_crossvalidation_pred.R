rm(list = ls(environment())) #clear environment
#cat("\014")  #clear console

library(caret)
library(Cubist)
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
image_name <- "12_06_2010_28_08_2009" #12_06_2010_28_08_2009 / 12_06_2010 / 28_08_2009
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
                          summaryFunction = defaultSummary, #defaultSummary(data, lev = NULL, model = NULL), postResample(pred, obs), twoClassSummary(data, lev = NULL, model = NULL), mnLogLoss(data, lev = NULL, model = NULL), multiClassSummary(data, lev = NULL, model = NULL), prSummary(data, lev = NULL, model = NULL)
                          #sampling = "smote", #Method to deal with class imbalance / Aplied Predictive modeling p.427 / a single character value describing the type of additional sampling that is conducted after resampling (usually to resolve class imbalances). Values are "none", "down", "up", "smote", or "rose". The latter two values require the DMwR and ROSE packages, respectively. This argument can also be a list to facilitate custom sampling and these details can be found on the caret package website for sampling (link below).
                          #index = , #a list with elements for each resampling iteration. Each list element is a vector of integers corresponding to the rows used for training at that iteration.
                          #indexOut = , #a list (the same length as index) that dictates which data are held-out for each resample (as integers). If NULL, then the unique set of samples not contained in index is used.
                          allowParallel = TRUE)
                          #search = "random") #Random search procedure is used (Bergstra and Bengio, 2012). See http://topepo.github.io/caret/random-hyperparameter-search.html for details and an example.
#Set tuning parameters
tune_Cubist = expand.grid(committees = c(1, 5, 10, 50, 100), # boosting–like scheme - iterative model trees are created in sequence. The first tree follows the procedure described in the last section. Subsequent trees are created using adjusted versions to the training set outcome: if the model over–predicted a value, the response is adjusted downward for the next model (and so on). Unlike traditional boosting, stage weights for each committee are not used to average the predictions from each model tree; the final prediction is a simple average of the predictions from each model tree.
                          neighbors = c(0, 1, 2, 5, 9)) # adjust the predictions from the rule–based model. First, a model tree (with or without committees) is created. Once a sample is predicted by this model, Cubist can find it’s nearest neighbors and determine the average of these training set points. See Quinlan (1993a) for the details of the adjustment.

#Classification -----------------------------------------------------------------------------------------------------------------------
# Read data
training_data = as.data.frame(read.csv(file = paste(area, "\\", scale, "\\treningowe_testowe\\landsat_proc_training.csv", sep = ""), 
                       header = TRUE, 
                       sep = ",",
                       dec = ".",
                       check.names = FALSE))
#Remove sample number
training_data = training_data[ ,-1]

#Calculate samples weights
# model_weights <- ifelse(training_data$nieprz_proc == "nieprzepuszczalne",
#                         (1/table(training_data$nieprz_proc)[1]) * 0.5,
#                         (1/table(training_data$nieprz_proc)[2]) * 0.5)

#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#----
for (variant_id in list_of_variants) {
  #Default settings for each variant of classification
  set.seed(100)
  Cubist_fit <- list(results = data.frame(),
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

  Cubist_fit_temp = train(x = variant_data[, -1],
                      y = variant_data[, 1],
                      method = "cubist",
                      #weights = model_weights, #define unequal weights for each sample to deal with class imbalance / Aplied Predictive modeling p.426
                      metric = "RMSE",
                      #preProc = "center",
                      trControl = fitControl,
                      tuneGrid = tune_Cubist) 
   print(Cubist_fit_temp)                                          
   Cubist_fit_temp$results$variant <- variant_id
   Cubist_fit_temp$pred$variant <- variant_id
    
   Cubist_fit$results <- rbind(Cubist_fit$results, Cubist_fit_temp$results)
   Cubist_fit$pred <- rbind(Cubist_fit$pred, Cubist_fit_temp$pred)
    
  #Save results
  workspace_name <- paste("Cubist_fit_", toString(variant_id), sep = "")
  assign(workspace_name, Cubist_fit)
 
  save(list = paste("Cubist_fit_", toString(variant_id), sep = ""),
       file = paste("D:\\Doktorat\\wyniki\\", area, "\\wyniki_kroswalidacja\\",scale, "\\Regresja\\Cubist\\", image_name, "\\wyniki_kroswalidacja_", toString(variant_id),".RData", sep = ""))
  
  write.csv(Cubist_fit$pred, 
             file = paste("D:\\Doktorat\\wyniki\\", area, "\\wyniki_kroswalidacja\\",scale, "\\Regresja\\Cubist\\", image_name, "\\pred_", toString(variant_id),".csv", sep = ""), 
             row.names = FALSE)

  write.csv(Cubist_fit$results, 
             file = paste("D:\\Doktorat\\wyniki\\", area, "\\wyniki_kroswalidacja\\",scale, "\\Regresja\\Cubist\\", image_name, "\\results_", toString(variant_id),".csv", sep = ""),
             row.names = FALSE)
  
  }

closeAllConnections();
#End of file