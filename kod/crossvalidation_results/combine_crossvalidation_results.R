rm(list = ls(environment())) #clear environment
#cat("\014")  #clear console

library(dplyr)
library(ggplot2)
library(xlsx)

setwd("D:\\Doktorat\\wyniki")

#------------------------------------------------------------------------------------------------------------------
#Set data 
area <- "raba" #raba / pradnik
scale <- "jednoskalowe" #jednoskalowe / wieloskalowe
image_name <- "12_06_2010_28_08_2009" #12_06_2010_28_08_2009 / 12_06_2010 / 28_08_2009
method <- "Regresja" #Klasyfikacja / Regresja
algorythm <- "Random_Forest" #C50 / Cubist / Random_Forest
list_of_variants <- 1:8


#Classification -----------------------------------------------------------------------------------------------------------------------
# Read crossvalidation summaries from csv files
crossvalidation_summaries <- c()

for (i in list_of_variants) {
  summary_tmp <- read.csv(file = paste(area, "\\wyniki_kroswalidacja\\", scale, "\\", method, "\\", algorythm, "\\", image_name, "\\results_", toString(i), ".csv", sep = ""),
                          header = TRUE, 
                          sep = ",",
                          dec = ".",
                          check.names = FALSE)
  #summary_tmp <- cbind(summary_tmp, variant = i)
  
  crossvalidation_summaries <- rbind(crossvalidation_summaries, summary_tmp)
}

if (method == "Klasyfikacja") {
  stat <- crossvalidation_summaries %>%
    group_by(variant) %>% 
    arrange(variant,
            desc(Sens))
} else {
  stat <- crossvalidation_summaries %>%
    group_by(variant) %>% 
    arrange(variant,
            desc(RMSE))
}



write.xlsx(crossvalidation_summaries, 
           file = paste(area, "\\wyniki_kroswalidacja\\", scale, "\\", method, "\\", algorythm, "\\", image_name, "\\all_variants_results.xlsx", sep = ""), 
           col.names = TRUE, 
           row.names = FALSE, 
           append = FALSE)
