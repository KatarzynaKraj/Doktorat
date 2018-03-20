rm(list = ls(environment()))
library(dplyr)
library(ggplot2)

setwd("D:\\Doktorat\\dane\\raba\\jednoskalowe\\treningowe_testowe\\")

#Read input data---------------------------------------------------------------------------------------
#Classification
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


#Regression
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


#Get numbers of training and test samples---------------------------------------------------------------------------------------
training_samples <- klas_training_data[ , "nr_pix"]
test_samples <- klas_test_data[ , "nr_pix"]
  




#Data analysis---------------------------------------------------------------------------------------
#numerical statistics for full training and test set
#Class data
stat_klas_training_test_data <- klas_training_test_data %>%
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
  
#Percentage data  
#All data
stat_proc_training_test_data <- proc_training_test_data %>%
                                mutate(pix_status = ifelse(proc_training_test_data$nr_pix %in% training_samples, "training", "test")) %>%
                                mutate(all_samples = n()) %>%
                                group_by(pix_status, all_samples) %>%
                                mutate(training_test_samples = n(),
                                       perc_training_test = round(training_test_samples/all_samples, 2))  %>%
                               group_by(pix_status, all_samples, training_test_samples, perc_training_test) %>%
                               summarize(min = min(nieprz_proc),
                                         max = max(nieprz_proc),
                                         first_quantile = quantile(nieprz_proc, 0.25),
                                         median = median(nieprz_proc),
                                         third_quantile = quantile(nieprz_proc, 0.75),
                                         mean = mean(nieprz_proc),
                                         sd = sd(nieprz_proc)) %>%
                              as.data.frame
stat_proc_training_test_data

#Data within breaks
stat_breaks <- c(0, 0.0000000001, 0.2, 0.5, 1)
stat_labels <- c("0%", "(0 - 20%)", "[20% - 50%)", "[50% - 100%]")
stat_proc_training_test_data_breaks <- proc_training_test_data %>%
                                      mutate(intervals = cut(nieprz_proc, 
                                                             breaks = stat_breaks,
                                                             labels = stat_labels,
                                                             include.lowest = TRUE, 
                                                             right = FALSE)) %>%
                                      mutate(pix_status = ifelse(proc_training_test_data$nr_pix %in% training_samples, "training", "test")) %>%
                                      mutate(all_samples = n()) %>%
                                      group_by(pix_status, 
                                               all_samples) %>%
                                      mutate(training_test_samples = n(),
                                             perc_training_test = round(training_test_samples/all_samples, 2))  %>%
                                      group_by(pix_status, 
                                               all_samples, 
                                               training_test_samples, 
                                               perc_training_test, 
                                               intervals) %>%
                                      mutate(interval_samples = n()) %>% 
                                      group_by(pix_status, 
                                               all_samples, 
                                               training_test_samples, 
                                               perc_training_test, 
                                               intervals, 
                                               interval_samples) %>%
                                      mutate(perc_interval_samples = round(interval_samples/training_test_samples, 2)) %>% 
                                      group_by(pix_status, 
                                               all_samples, 
                                               training_test_samples, 
                                               perc_training_test, 
                                               intervals, 
                                               interval_samples, 
                                               perc_interval_samples) %>%
                                      summarize(min = min(nieprz_proc),
                                                max = max(nieprz_proc),
                                                first_quantile = quantile(nieprz_proc, 0.25),
                                                median = median(nieprz_proc),
                                                third_quantile = quantile(nieprz_proc, 0.75),
                                                mean = mean(nieprz_proc),
                                                sd = sd(nieprz_proc)) %>%
                                      as.data.frame
stat_proc_training_test_data_breaks
  
#graphical analysis
#Bars
#All data
p_bar_all <- ggplot(data = stat_proc_training_test_data_breaks, 
                 aes(x = intervals, 
                     y = perc_interval_samples,  
                     fill = pix_status))
p_bar_all <- p_bar_all + geom_bar(stat = "identity")
p_bar_all <- p_bar_all + facet_grid(~pix_status)
p_bar_all <- p_bar_all + scale_y_continuous(labels = scales::percent)
p_bar_all <- p_bar_all + labs(y = "Percentage of samples", x = "Imperviousness index intervals")
p_bar_all <- p_bar_all + geom_text(aes( x = intervals,
                                        y = perc_interval_samples, 
                                        label = interval_samples,
                                        vjust = -.5))
p_bar_all

#Bars
#Data within breaks
p_bar_ <- ggplot(data = stat_proc_training_test_data_breaks, 
                    aes(x = intervals, 
                        y = perc_interval_samples,  
                        fill = pix_status))
p_bar_breaks <- p_bar_breaks + geom_bar(stat = "identity")
p_bar_breaks <- p_bar_breaks + facet_grid(~pix_status)
p_bar_breaks <- p_bar_breaks + scale_y_continuous(labels = scales::percent)
p_bar_breaks <- p_bar_breaks + labs(y = "Percentage of samples", x = "Imperviousness index intervals")
p_bar_breaks








