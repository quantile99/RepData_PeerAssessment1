#####################################################
# What is mean total number of steps taken per day? #
# ###################################################
# For this part of the assignment, you can ignore the missing values in the dataset.
source("1 - Load Preprocess Data.R")

# 1. Calculate the total number of steps taken per day
library(dplyr)
steps_per_day <- 
    activity %>% 
    group_by(date) %>% 
    summarise_each(funs(sum), steps)
head(steps_per_day)


# 2. Make a histogram of the total number of steps taken each day
hist(steps_per_day$steps, breaks = 10)

# 2. Calculate and report the mean and median total number of steps taken per day
summary <-
steps_per_day %>% 
    filter(!is.na(steps)) %>%
    summarise_each(funs(mean, median), steps)


summary[[1]]

