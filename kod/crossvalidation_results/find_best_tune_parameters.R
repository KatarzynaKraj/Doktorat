rm(list = ls(environment())) #clear environment
#cat("\014")  #clear console

library(dplyr)
library(xlsx)

setwd("D:\\Doktorat\\wyniki")

#------------------------------------------------------------------------------------------------------------------
#Set data 
area <- "raba" #raba / pradnik
scale <- "jednoskalowe" #jednoskalowe / wieloskalowe
image_name <- "28_08_2009" #12_06_2010_28_08_2009 / 12_06_2010 / 28_08_2009
method <- "Regresja" #Klasyfikacja / Regresja
algorythm <- "Random_Forest" #C50 / Cubist / Random_Forest



#Classification -----------------------------------------------------------------------------------------------------------------------
# Read crossvalidation results for all variants
crossvalidation_results <- read.xlsx(file = paste(area, "\\wyniki_kroswalidacja\\", scale, "\\", method, "\\", algorythm, "\\", image_name, "\\all_variants_results.xlsx", sep = ""),
                                     sheetIndex = 1,
                                     as.data.frame=TRUE, 
                                     header=TRUE)

if (method == "Klasyfikacja" & algorythm == "C50") {
  stat <- crossvalidation_results %>%
    group_by(variant) %>% 
    arrange(variant,
            desc(Sens)) %>%
    filter(row_number() == 1) %>% 
    select(variant, model, winnow, trials, noGlobalPruning, CF, fuzzyThreshold, Sens, Spec, Accuracy, ROC, Kappa, SensSD, SpecSD, AccuracySD, ROCSD, KappaSD) %>% 
    as.data.frame
} else if (method == "Klasyfikacja" & algorythm == "Random_Forest") {
  stat <- crossvalidation_results %>%
    group_by(variant) %>% 
    arrange(variant,
            desc(Sens)) %>%
    filter(row_number() == 1) %>% 
    select(variant, mtry, Sens, Spec, Accuracy, ROC, Kappa, SensSD, SpecSD, AccuracySD, ROCSD, KappaSD) %>% 
    as.data.frame
} else if (method == "Regresja" & algorythm == "Cubist") {
  stat <- crossvalidation_results %>%
    group_by(variant) %>% 
    arrange(variant,
            RMSE) %>%
    filter(row_number() == 1) %>% 
    select(variant, committees, neighbors, RMSE, Rsquared, MAE, RMSESD, RsquaredSD, MAESD) %>% 
    as.data.frame
} else if (method == "Regresja" & algorythm == "Random_Forest") {
  stat <- crossvalidation_results %>%
    group_by(variant) %>% 
    arrange(variant,
            RMSE) %>%
    filter(row_number() == 1) %>% 
    select(variant, mtry, RMSE, Rsquared, MAE, RMSESD, RsquaredSD, MAESD) %>% 
    as.data.frame
} 

write.xlsx(stat,
           file = paste(area, "\\wyniki_kroswalidacja\\", scale, "\\", method, "\\", algorythm, "\\", image_name, "\\best_tunning_parameters.xlsx", sep = ""),
           col.names = TRUE,
           row.names = FALSE,
           append = FALSE)