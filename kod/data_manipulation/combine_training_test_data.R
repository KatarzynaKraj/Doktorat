rm(list = ls(environment()))
library(dplyr)

setwd("D:\\Doktorat\\dane\\pradnik\\jednoskalowe\\treningowe_testowe\\")


#-------------------------------------------------------------------------------------
#Klasyfikacja
klas_training_data <- read.csv(file = "landsat_klas_training.csv", 
                          header = TRUE, 
                          sep = ",",
                          dec = ".",
                          check.names = FALSE)

klas_test_data <- read.csv(file = "landsat_klas_test.csv", 
                      header = TRUE, 
                      sep = ",",
                      dec = ".",
                      check.names = FALSE)

klas_training_test_data <- rbind(klas_training_data,
                            klas_test_data)
klas_training_test_data_sorted <- arrange(klas_training_test_data, 
                                          nr_pix)

write.csv(klas_training_test_data_sorted, 
          file = "landsat_klas_training_test.csv",
          row.names = FALSE,
          quote = FALSE)


#Regresja
proc_training_data <- read.csv(file = "landsat_proc_training.csv", 
                               header = TRUE, 
                               sep = ",",
                               dec = ".",
                               check.names = FALSE)

proc_test_data <- read.csv(file = "landsat_proc_test.csv", 
                           header = TRUE, 
                           sep = ",",
                           dec = ".",
                           check.names = FALSE)

proc_training_test_data <- rbind(proc_training_data,
                           proc_test_data)
proc_training_test_data_sorted <- arrange(proc_training_test_data, 
                                          nr_pix)

write.csv(proc_training_test_data_sorted, 
          file = "landsat_proc_training_test.csv",
          row.names = FALSE,
          quote = FALSE)