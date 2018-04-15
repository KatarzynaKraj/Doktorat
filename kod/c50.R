rm(list = ls(environment())) #clear environment
#cat("\014")  #clear console

library(caret)
library(C50)
library(doParallel)
registerDoParallel(12)

#Klasyfikacja-----------------------------------------------------------------------------------------------------------------------
# Wczytywanie danych z pliku tekstowego
uczace = read.csv(file = "D:\\Doktorat\\dane\\raba\\jednoskalowe\\treningowe_testowe\\landsat_klas_training.csv", 
                  header = TRUE, 
                  sep = ",",
                  dec = ".",
                  check.names = FALSE)
# Utworzenie ramki danych
uczace = data.frame(uczace, check.names = FALSE)

# Obliczenie wymiarow danych
r_uczace = dim(uczace)[1]
c_uczace = dim(uczace)[2]

training_data = uczace[ ,2:c_uczace]

#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----
# Wczytywanie wariant?w klasyfikacji
global_directory = "E:\\Kasia\\E\\Robocze_R\\Landsat_C50_Cubist\\Raba\\12_06_2010_28_08_2009\\warianty_klasyfikacji\\"

fitControl = trainControl(method="repeatedcv", #The resampling method: "boot", "boot632", "optimism_boot", "boot_all", "cv", "repeatedcv", "LOOCV", "LGOCV" (for repeated training/test splits), "none" (only fits one model to the entire training set), "oob" (only for random forest, bagged trees, bagged earth, bagged flexible discriminant analysis, or conditional tree forest models), timeslice, "adaptive_cv", "adaptive_boot" or "adaptive_LGOCV"
                          number=5, #number of folds
                          repeats=10, #number of repetitions of corssvalidation
                          #p = 0.2 #For leave-group out cross-validation: the training percentage
                          verboseIter = TRUE, #print training log
                          returnData = FALSE, #save data 
                          returnResamp = "all", #how much of resampled summary metrics should be saved
                          savePredictions = "all", #how much of the hold-out predictions should be saved
                          classProbs = TRUE, #a logical; should class probabilities be computed for classification models (along with predicted values) in each resample?
                          summaryFunction = twoClassSummary, #defaultSummary(data, lev = NULL, model = NULL), postResample(pred, obs), twoClassSummary(data, lev = NULL, model = NULL), mnLogLoss(data, lev = NULL, model = NULL), multiClassSummary(data, lev = NULL, model = NULL), prSummary(data, lev = NULL, model = NULL)
                          sampling = "up", #Method to deal with class imbalance / Aplied Predictive modeling p.427 / a single character value describing the type of additional sampling that is conducted after resampling (usually to resolve class imbalances). Values are "none", "down", "up", "smote", or "rose". The latter two values require the DMwR and ROSE packages, respectively. This argument can also be a list to facilitate custom sampling and these details can be found on the caret package website for sampling (link below).
                          #index = , #a list with elements for each resampling iteration. Each list element is a vector of integers corresponding to the rows used for training at that iteration.
                          #indexOut = , #a list (the same length as index) that dictates which data are held-out for each resample (as integers). If NULL, then the unique set of samples not contained in index is used.
                          allowParallel = TRUE)
tune_C50 = expand.grid(trials = c(10, 50, 100), #boosting iterations 
                       model = c("tree", "rules"), #version of model
                       winnow = FALSE) #removing unimportant predictors

#------------------------------ 
i = 1
i
nieprz_directory = paste("wariant_klasyfikacji_", toString(i),".txt", sep = "")
nieprz_path = paste(global_directory, nieprz_directory, sep = "")

wariant = read.table(nieprz_path, header=FALSE, sep=",", stringsAsFactors=FALSE ) 
wariant_klasyfikacji = as.character(wariant)

set.seed(100) 
data=training_data[ ,wariant_klasyfikacji]
x = data[ ,-1]
y = data[ ,1]

model_weights <- ifelse(data$nieprz_proc == "nieprzepuszczalne",
                        (1/table(data$nieprz_proc)[1]) * 0.5,
                        (1/table(data$nieprz_proc)[2]) * 0.5)

C50_fit_1 = train(x,
                  y, 
                  method = "C5.0",
                  #weights = model_weights, #define unequal weights for each sample to deal with class imbalance / Aplied Predictive modeling p.426
                  metric = "Spec",
                  #preProc = "center", 
                  trControl = fitControl, 
                  tuneGrid = tune_C50,
                  control = C50::C5.0Control(noGlobalPruning = FALSE, #A logical to toggle whether the final, global pruning step to simplify the tree.
                                        CF = 0.1, #A number in (0, 1) for the confidence factor. Confidence factor (CF) affects the way that error rates are estimated and lets you control the severity of pruning of the tree, meaning lower factor levels will likely prune away the leaves which overspecify the classification.
                                        minCases = 2,#an integer for the smallest number of samples that must be put in at least two of the splits.
                                        fuzzyThreshold = TRUE)) #A logical toggle to evaluate possible advanced splits of the data. See Quinlan (1993) for details and examples. Quinlan R (1993). C4.5: Programs for Machine Learning. Morgan Kaufmann Publishers, http://www.rulequest.com/see5-unix.html
C50_fit_1$results
closeAllConnections();
#------------------------------ 
i = 2
i
nieprz_directory = paste("wariant_klasyfikacji_", toString(i),".txt", sep = "")
nieprz_path = paste(global_directory, nieprz_directory, sep = "")

wariant = read.table(nieprz_path, header=FALSE, sep=",", stringsAsFactors=FALSE ) 
wariant_klasyfikacji = as.character(wariant)

set.seed(100) 
data=training_data[ ,wariant_klasyfikacji]
x = data[ ,-1]
y = data[ ,1]

C50_fit_2 = train(x,
                  y, 
                  method = "C5.0",
                  trControl = fitControl, 
                  tuneGrid = tune_C50,
                  control = C5.0Control(noGlobalPruning = FALSE, 
                                        CF = 0.1, 
                                        minCases = 2, 
                                        fuzzyThreshold = TRUE)) 
C50_fit_2
#------------------------------ 
i = 3
i
nieprz_directory = paste("wariant_klasyfikacji_", toString(i),".txt", sep = "")
nieprz_path = paste(global_directory, nieprz_directory, sep = "")

wariant = read.table(nieprz_path, header=FALSE, sep=",", stringsAsFactors=FALSE ) 
wariant_klasyfikacji = as.character(wariant)

set.seed(100) 
data=training_data[ ,wariant_klasyfikacji]
x = data[ ,-1]
y = data[ ,1]

C50_fit_3 = train(x,
                  y, 
                  method = "C5.0",
                  trControl = fitControl, 
                  tuneGrid = tune_C50,
                  control = C5.0Control(noGlobalPruning = TRUE,
                                        CF = 0.1,
                                        minCases = 2,
                                        fuzzyThreshold = TRUE))
C50_fit_3
#------------------------------ 
i = 4
i
nieprz_directory = paste("wariant_klasyfikacji_", toString(i),".txt", sep = "")
nieprz_path = paste(global_directory, nieprz_directory, sep = "")

wariant = read.table(nieprz_path, header=FALSE, sep=",", stringsAsFactors=FALSE ) 
wariant_klasyfikacji = as.character(wariant)

set.seed(100) 
data=training_data[ ,wariant_klasyfikacji]
x = data[ ,-1]
y = data[ ,1]

C50_fit_4 = train(x,
                  y, 
                  method = "C5.0",
                  trControl = fitControl, 
                  tuneGrid = tune_C50,
                  control = C5.0Control(noGlobalPruning = FALSE,
                                        CF = 0.1,
                                        minCases = 2,
                                        fuzzyThreshold = TRUE))
C50_fit_4
#------------------------------ 
i = 5
i
nieprz_directory = paste("wariant_klasyfikacji_", toString(i),".txt", sep = "")
nieprz_path = paste(global_directory, nieprz_directory, sep = "")

wariant = read.table(nieprz_path, header=FALSE, sep=",", stringsAsFactors=FALSE ) 
wariant_klasyfikacji = as.character(wariant)

set.seed(100) 
data=training_data[ ,wariant_klasyfikacji]
x = data[ ,-1]
y = data[ ,1]

C50_fit_5 = train(x,
                  y, 
                  method = "C5.0",
                  trControl = fitControl, 
                  tuneGrid = tune_C50,
                  control = C5.0Control(noGlobalPruning = FALSE,
                                        CF = 0.1,
                                        minCases = 2,
                                        fuzzyThreshold = TRUE))
C50_fit_5
#------------------------------ 
i = 6
i
nieprz_directory = paste("wariant_klasyfikacji_", toString(i),".txt", sep = "")
nieprz_path = paste(global_directory, nieprz_directory, sep = "")

wariant = read.table(nieprz_path, header=FALSE, sep=",", stringsAsFactors=FALSE ) 
wariant_klasyfikacji = as.character(wariant)

set.seed(100) 
data=training_data[ ,wariant_klasyfikacji]
x = data[ ,-1]
y = data[ ,1]

C50_fit_6 = train(x,
                  y, 
                  method = "C5.0",
                  trControl = fitControl, 
                  tuneGrid = tune_C50,
                  control = C5.0Control(noGlobalPruning = TRUE,
                                        CF = 0.25,
                                        minCases = 2,
                                        fuzzyThreshold = FALSE))
C50_fit_6
#------------------------------ 
i = 7
i
nieprz_directory = paste("wariant_klasyfikacji_", toString(i),".txt", sep = "")
nieprz_path = paste(global_directory, nieprz_directory, sep = "")

wariant = read.table(nieprz_path, header=FALSE, sep=",", stringsAsFactors=FALSE ) 
wariant_klasyfikacji = as.character(wariant)

set.seed(100) 
data=training_data[ ,wariant_klasyfikacji]
x = data[ ,-1]
y = data[ ,1]

C50_fit_7 = train(x,
                  y, 
                  method = "C5.0",
                  trControl = fitControl, 
                  tuneGrid = tune_C50,
                  control = C5.0Control(noGlobalPruning = TRUE,
                                        CF = 0.25,
                                        minCases = 2,
                                        fuzzyThreshold = FALSE))
C50_fit_7
#------------------------------ 
i = 8
i
nieprz_directory = paste("wariant_klasyfikacji_", toString(i),".txt", sep = "")
nieprz_path = paste(global_directory, nieprz_directory, sep = "")

wariant = read.table(nieprz_path, header=FALSE, sep=",", stringsAsFactors=FALSE ) 
wariant_klasyfikacji = as.character(wariant)

set.seed(100) 
data=training_data[ ,wariant_klasyfikacji]
x = data[ ,-1]
y = data[ ,1]

C50_fit_8 = train(x,
                  y, 
                  method = "C5.0",
                  trControl = fitControl, 
                  tuneGrid = tune_C50,
                  control = C5.0Control(noGlobalPruning = FALSE,
                                        CF = 0.1,
                                        minCases = 2,
                                        fuzzyThreshold = TRUE))
C50_fit_8

#-----------------------------------------------------------------------------------
resamps = resamples(list(C1 = C50_fit_1,
                         C2 = C50_fit_2,
                         C3 = C50_fit_3,
                         C4 = C50_fit_4,
                         C5 = C50_fit_5,
                         C6 = C50_fit_6,
                         C7 = C50_fit_7,
                         C8 = C50_fit_8))
resamps

summary(resamps)
difValues = diff(resamps)
difValues
summary(difValues)



# Zapis wynikow crosswalidacji
#output_directory = "E:\\Kasia\\E\\Robocze_R\\Landsat_C50_Cubist\\12_06_2010_28_08_2009\\wyniki_kroswalidacja\\C50\\"
#write.table (wynik,
#     file = paste(output_directory, "wyniki_kroswalidacji_klasyfikacja_",toString(i),".txt", sep = ""), row.names=FALSE, col.names=FALSE,sep=" ", quote=FALSE)

