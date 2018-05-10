rm(list = ls(environment())) #clear environment
#cat("\014")  #clear console

library(dplyr)
library(xlsx)

setwd("D:\\Doktorat\\dane")

#------------------------------------------------------------------------------------------------------------------
#Set data 
area <- "raba" #raba / pradnik
scale <- "jednoskalowe" #jednoskalowe / wieloskalowe
#image_name <- "28_08_2009" #12_06_2010_28_08_2009 / 12_06_2010 / 28_08_2009
list_of_variants <- 1:8

# Read data
training_data = as.data.frame(read.csv(file = paste(area, "\\", scale, "\\treningowe_testowe\\landsat_klas_training.csv", sep = ""), 
                                       header = TRUE, 
                                       sep = ",",
                                       dec = ".",
                                       check.names = FALSE))
#Remove sample number
training_data = training_data[ ,-1]

predictor_counts <- data.frame()
for (image_name in c("12_06_2010_28_08_2009", "12_06_2010", "28_08_2009")){ 
  for (variant_id in list_of_variants) {
    #Read variant data
    variant_directory <-  paste("D:\\Doktorat\\dane\\", area, "\\", scale, "\\warianty_klasyfikacji\\", image_name, "\\wariant_klasyfikacji_", toString(variant_id),".txt", sep = "")
    variant <- as.character(read.table(variant_directory, 
                                       header = FALSE, 
                                       sep = ",", 
                                       stringsAsFactors = FALSE ))
    variant_data <- training_data[ ,variant]
    
    predictor_counts <- rbind(predictor_counts, cbind(image_name, variant_id, (ncol(variant_data) -1)))
  }
}

predictor_counts <- setNames(predictor_counts, c("image", "variant", "number_of_predictors"))
predictor_counts


write.xlsx(predictor_counts,
           file = paste("D:\\Doktorat\\wyniki\\", area, "\\dane_wejsciowe\\predictors_count.xlsx", sep = ""),
           col.names = TRUE,
           row.names = FALSE,
           append = FALSE)