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
  




#Data analysis-----------------------------------------------------------------------------------------------------------------
#numerical statistics for full training and test set --------------------------------------------------------------------------
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

#numerical statistics for training and test set divided into imperviousness index intervals------------------------------------------------

#Create dataset divided into training/test and imperviousness index intervals
#Data within breaks
stat_breaks <- c(0, 0.0000000001, 0.2, 0.5, 1)
stat_labels <- c("0%", "(0 - 20%)", "[20% - 50%)", "[50% - 100%]")

proc_training_test_data_breaks <- proc_training_test_data %>%
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
                                         perc_interval_samples)

stat_proc_training_test_data_breaks <- proc_training_test_data_breaks %>%
                                        summarize(min = min(nieprz_proc),
                                                  max = max(nieprz_proc),
                                                  first_quantile = quantile(nieprz_proc, 0.25),
                                                  median = median(nieprz_proc),
                                                  third_quantile = quantile(nieprz_proc, 0.75),
                                                  mean = mean(nieprz_proc),
                                                  sd = sd(nieprz_proc)) %>%
                                        as.data.frame
stat_proc_training_test_data_breaks
  
#graphical analysis------------------------------------------------------------------------------------------------------------
#Bars
p_bar <- ggplot(data = stat_proc_training_test_data_breaks, 
                 aes(x = intervals, 
                     y = perc_interval_samples,  
                     fill = pix_status))
p_bar <- p_bar + geom_bar(stat = "identity")
p_bar <- p_bar + facet_grid(~pix_status)
p_bar <- p_bar + scale_y_continuous(labels = scales::percent)
p_bar <- p_bar + labs(y = "Percentage of samples", x = "Imperviousness index intervals")
p_bar <- p_bar + geom_text(aes( x = intervals,
                                        y = perc_interval_samples, 
                                        label = interval_samples,
                                        vjust = -.5))
p_bar

#Boxplots
p_boxplot<- ggplot(data = proc_training_test_data_breaks, 
                       aes(x = intervals, 
                           y = nieprz_proc,
                          fill = pix_status))
p_boxplot <- p_boxplot + geom_boxplot()
p_boxplot <- p_boxplot + facet_grid(~pix_status)
p_boxplot <- p_boxplot + scale_y_continuous(labels = scales::percent)
p_boxplot <- p_boxplot + labs(y = "Percentage value of imperviousness index", x = "Imperviousness index intervals")
p_boxplot

















