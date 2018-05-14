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
image_name <- "28_08_2009" #12_06_2010_28_08_2009 / 12_06_2010 / 28_08_2009
method <- "Klasyfikacja" #Klasyfikacja / Regresja
algorythm <- "C50" #C50 / Cubist / Random_Forest
list_of_variants <- 1:8



#Classification -----------------------------------------------------------------------------------------------------------------------
#Read the best tunning parameters
tunning_parameters = read.xlsx(file = paste("D:\\Doktorat\\wyniki", area, "wyniki_kroswalidacja",scale,  method, algorythm, image_name, "best_tunning_parameters.xlsx", sep = "\\"),
                               sheetIndex = 1,
                               as.data.frame=TRUE, 
                               header=TRUE)

# Read training data
training_data = as.data.frame(read.csv(file = paste(area, "\\", scale, "\\treningowe_testowe\\landsat_klas_training.csv", sep = ""), 
                                       header = TRUE, 
                                       sep = ",",
                                       dec = ".",
                                       check.names = FALSE))
# Read test data
test_data = as.data.frame(read.csv(file = paste(area, "\\", scale, "\\treningowe_testowe\\landsat_klas_test.csv", sep = ""), 
                                   header = TRUE, 
                                   sep = ",",
                                   dec = ".",
                                   check.names = FALSE))
#Remove sample number
training_data = training_data[ ,-1]
test_data = test_data[ ,-1]

#Calculate samples weights
model_weights <- ifelse(training_data$nieprz_proc == "nieprzepuszczalne",
                        (1/table(training_data$nieprz_proc)[1]) * 0.5,
                        (1/table(training_data$nieprz_proc)[2]) * 0.5)

#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#----
#------------------------------ 
Accuracy_stat_test <- data.frame()

for (variant_id in list_of_variants) {
  #Set the best tuning parameters for variant
  the_best_tunning_parameters <- tunning_parameters[tunning_parameters$variant == variant_id, ]
  
  #Default settings for each variant of classification
  set.seed(100)

  #Print loop status
  print(paste("variant", variant_id, "is processed...", sep = " "))
  
  #Read variant data
  variant_directory <-  paste("D:\\Doktorat\\dane\\", area, "\\", scale, "\\warianty_klasyfikacji\\", image_name, "\\wariant_klasyfikacji_", toString(variant_id),".txt", sep = "")
  variant <- as.character(read.table(variant_directory, 
                                     header = FALSE, 
                                     sep = ",", 
                                     stringsAsFactors = FALSE ))
  
  variant_training_data <- training_data[ ,variant]
  variant_test_data <- test_data[ ,variant]
  
  #build single model based on training data
  C50_model_tmp  = C5.0(x = variant_training_data[, -1],
                      y = variant_training_data[, 1],
                      trials = the_best_tunning_parameters$trials,
                      rules = ifelse(the_best_tunning_parameters$model == "tree", FALSE, TRUE),
                      weights = model_weights,
                      control = C50::C5.0Control(noGlobalPruning = the_best_tunning_parameters$noGlobalPruning, 
                                                 CF = the_best_tunning_parameters$CF,
                                                 minCases = 2, 
                                                 fuzzyThreshold = the_best_tunning_parameters$fuzzyThreshold))
  
  #classify training data using fitted model
  predict_training_class_tmp <- predict(C50_model_tmp,
                                        newdata = variant_training_data,
                                        type = "class")
  predict_training_prob_tmp <- predict(C50_model_tmp,
                                       newdata = variant_training_data,
                                       type = "prob")
  write.xlsx(predict_training_class_tmp, 
            file = paste("D:\\Doktorat\\wyniki", area, "wyniki_testowe",scale,  method, algorythm, image_name, "predicted_training", paste("predict_training_class_",toString(variant_id),".xlsx", sep = "") , sep = "\\"), 
            col.names = FALSE,
            row.names = FALSE,
            append = FALSE)
  write.xlsx(predict_training_prob_tmp, 
             file = paste("D:\\Doktorat\\wyniki", area, "wyniki_testowe",scale,  method, algorythm, image_name, "predicted_training", paste("predict_training_prob_",toString(variant_id),".xlsx", sep = "") , sep = "\\"), 
             col.names = TRUE,
             row.names = FALSE,
             append = FALSE)
  
  #classify test data using fitted model
  predict_test_class_tmp<- predict(C50_model_tmp,
                                   newdata = variant_test_data,
                                   type = "class")
  predict_test_prob_tmp <- predict(C50_model_tmp,
                                   newdata = variant_test_data,
                                   type = "prob")
  write.xlsx(predict_test_class_tmp, 
             file = paste("D:\\Doktorat\\wyniki", area, "wyniki_testowe",scale,  method, algorythm, image_name, "predicted_test", paste("predict_test_class_",toString(variant_id),".xlsx", sep = "") , sep = "\\"), 
             col.names = FALSE,
             row.names = FALSE,
             append = FALSE)
  write.xlsx(predict_test_prob_tmp, 
             file = paste("D:\\Doktorat\\wyniki", area, "wyniki_testowe",scale,  method, algorythm, image_name, "predicted_test", paste("predict_test_prob_",toString(variant_id),".xlsx", sep = "") , sep = "\\"), 
             col.names = TRUE,
             row.names = FALSE,
             append = FALSE)
  
  #Create confusion matrixes for test data
  training_confusion_matrix_tmp <- confusionMatrix(data = predict_training_class_tmp, 
                                                   reference = variant_training_data[, 1],
                                                   positive = "nieprzepuszczalne",
                                                   mode = "everything")
  test_confusion_matrix_tmp <- confusionMatrix(data = predict_test_class_tmp, 
                                               reference = variant_test_data[, 1],
                                               positive = "nieprzepuszczalne",
                                               mode = "everything")

  write.xlsx(training_confusion_matrix_tmp$table, 
            file = paste("D:\\Doktorat\\wyniki", area, "wyniki_testowe",scale,  method, algorythm, image_name, "conf_m_training", paste("conf_matrix_training_",toString(variant_id),".xlsx", sep = "") , sep = "\\"), 
            col.names = TRUE,
            row.names = FALSE,
            append = FALSE)
  write.xlsx(test_confusion_matrix_tmp$table, 
             file = paste("D:\\Doktorat\\wyniki", area, "wyniki_testowe",scale,  method, algorythm, image_name, "conf_m_test", paste("conf_matrix_test_",toString(variant_id),".xlsx", sep = "") , sep = "\\"), 
             col.names = TRUE,
             row.names = FALSE,
             append = FALSE)
  
  #Get additional accuracy statistics
  Accuracy_stat_test_tmp <- t(c(variant = variant_id, 
                              test_confusion_matrix_tmp$overall, 
                              test_confusion_matrix_tmp$byClass))

  Accuracy_stat_test  <- rbind(Accuracy_stat_test, 
                               Accuracy_stat_test_tmp)
  write.xlsx(as.data.frame(Accuracy_stat_test_tmp), 
             file = paste("D:\\Doktorat\\wyniki", area, "wyniki_testowe",scale,  method, algorythm, image_name, "Accuracy_statistics", paste("Accuracy_stat_test_",toString(variant_id),".xlsx", sep = "") , sep = "\\"), 
             col.names = TRUE,
             row.names = FALSE,
             append = FALSE)
  
  
  
  model_name <- paste("C50_model_", toString(variant_id), sep = "")
  assign(model_name, C50_model_tmp)
  
  predict_training_class_name <- paste("predict_training_class_", toString(variant_id), sep = "")
  assign(predict_training_class_name, predict_training_class_tmp)
  
  predict_training_prob_name <- paste("predict_training_prob_", toString(variant_id), sep = "")
  assign(predict_training_prob_name, predict_training_prob_tmp)
  
  
  predict_test_class_name <- paste("predict_test_class_", toString(variant_id), sep = "")
  assign(predict_test_class_name, predict_test_class_tmp)
  
  predict_test_prob_name <- paste("predict_test_prob_", toString(variant_id), sep = "")
  assign(predict_test_prob_name, predict_test_prob_tmp)
  
  training_confusion_matrix_name <- paste("training_confusion_matrix_", toString(variant_id), sep = "")
  assign(training_confusion_matrix_name, training_confusion_matrix_tmp)
  
  test_confusion_matrix_name <- paste("test_confusion_matrix_", toString(variant_id), sep = "")
  assign(test_confusion_matrix_name, test_confusion_matrix_tmp)
  
  Accuracy_stat_test_name <- paste("Accuracy_stat_test_", toString(variant_id), sep = "")
  assign(Accuracy_stat_test_name, Accuracy_stat_test_tmp)
}

write.xlsx(Accuracy_stat_test, 
           file = paste("D:\\Doktorat\\wyniki", area, "wyniki_testowe",scale,  method, algorythm, image_name, "Accuracy_statistics", "all_accuracy_statistics.xlsx", sep = "\\"), 
           col.names = TRUE,
           row.names = FALSE,
           append = FALSE)


closeAllConnections();
#End of file

