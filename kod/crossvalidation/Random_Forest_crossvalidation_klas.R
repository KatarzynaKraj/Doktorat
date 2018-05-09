rm(list = ls(environment())) #clear environment
#cat("\014")  #clear console

library(caret)
library(randomForest)
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
                          allowParallel = TRUE,
                          search = "random") #Random search procedure is used (Bergstra and Bengio, 2012). See http://topepo.github.io/caret/random-hyperparameter-search.html for details and an example.
# #Set tuning parameters

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
for (variant_id in list_of_variants) {
  #Default settings for each variant of classification
  set.seed(100)
  RF_fit <- list(results = data.frame(),
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

  RF_fit_temp = train(x = variant_data[, -1],
                      y = variant_data[, 1],
                      method = "rf",
                      weights = model_weights, #define unequal weights for each sample to deal with class imbalance / Aplied Predictive modeling p.426
                      metric = "Sens",
                      #preProc = "center",
                      trControl = fitControl,
                      tuneLength = 15) # 15 mtry values randomly selected across total number of predictors (because variants have various total number of features)
                      #default ntree is set as 500, no possibility to tune
  
   print(RF_fit_temp)                                          
   RF_fit_temp$results$variant <- variant_id
   RF_fit_temp$pred$variant <- variant_id
    
   RF_fit$results <- rbind(RF_fit$results, RF_fit_temp$results)
   RF_fit$pred <- rbind(RF_fit$pred, RF_fit_temp$pred)
    
  #Save results
  workspace_name <- paste("RF_fit_", toString(variant_id), sep = "")
  assign(workspace_name, RF_fit)
 
  save(list = paste("RF_fit_", toString(variant_id), sep = ""),
       file = paste("D:\\Doktorat\\wyniki\\", area, "\\wyniki_kroswalidacja\\",scale, "\\Klasyfikacja\\Random_Forest\\", image_name, "\\wyniki_kroswalidacja_", toString(variant_id),".RData", sep = ""))
  
  write.csv(RF_fit$pred, 
             file = paste("D:\\Doktorat\\wyniki\\", area, "\\wyniki_kroswalidacja\\",scale, "\\Klasyfikacja\\Random_Forest\\", image_name, "\\pred_", toString(variant_id),".csv", sep = ""), 
             row.names = FALSE)

  write.csv(RF_fit$results, 
             file = paste("D:\\Doktorat\\wyniki\\", area, "\\wyniki_kroswalidacja\\",scale, "\\Klasyfikacja\\Random_Forest\\", image_name, "\\results_", toString(variant_id),".csv", sep = ""),
             row.names = FALSE)
  
  }

closeAllConnections();
#End of file