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
image_name <- "12_06_2010" #12_06_2010_28_08_2009 / 12_06_2010 / 28_08_2009
list_of_variants <- 1:8


#Classification -----------------------------------------------------------------------------------------------------------------------
# Read crossvalidation summaries from csv files
crossvalidation_summaries <- c()

for (i in list_of_variants) {
  summary_tmp <- read.csv(file = paste(area, "\\wyniki_kroswalidacja\\", scale, "\\Klasyfikacja\\C50\\", image_name, "\\results_", toString(i), ".csv", sep = ""),
                          header = TRUE, 
                          sep = ",",
                          dec = ".",
                          check.names = FALSE)
  summary_tmp <- cbind(summary_tmp, variant = i)
  
  crossvalidation_summaries <- rbind(crossvalidation_summaries, summary_tmp)
}

stat <- crossvalidation_summaries %>%
        group_by(variant) %>% 
        arrange(variant,
                desc(Sens))


write.xlsx(crossvalidation_summaries, 
           file = paste(area, "\\wyniki_kroswalidacja\\", scale, "\\Klasyfikacja\\C50\\", image_name, "\\all_variants_results.xlsx", sep = ""), 
           col.names = TRUE, 
           row.names = FALSE, 
           append = FALSE)


stat <- klas_training_test_data %>%
  mutate(pix_status = ifelse(klas_training_test_data$nr_pix %in% training_samples, "training", "test")) %>%
  mutate(all_samples = n()) %>%
  group_by(pix_status, all_samples) %>%
  mutate(training_test_samples = n(),
         perc_training_test = round(training_test_samples/all_samples, 2))  %>%
  group_by(pix_status, nieprz_proc, all_samples, training_test_samples, perc_training_test) %>%
  summarise(samples = n()) %>%
  mutate(perc = round(samples/training_test_samples, 2)) %>%
  as.data.frame()
stat_klas_training_test_data