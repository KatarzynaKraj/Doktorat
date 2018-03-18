rm(list = ls(environment()))

setwd("D:\\Doktorat\\dane\\pradnik\\jednoskalowe\\treningowe_testowe\\")
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

klas_training_test_data <- read.csv(file = "landsat_klas_training_test.csv", 
                                    header = TRUE, 
                                    sep = ",",
                                    dec = ".",
                                    check.names = FALSE)

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

proc_training_test_data <- read.csv(file = "landsat_proc_training_test.csv", 
                                    header = TRUE, 
                                    sep = ",",
                                    dec = ".",
                                    check.names = FALSE)