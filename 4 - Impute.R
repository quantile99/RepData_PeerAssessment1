###########################
# Imputing missing values #
###########################
# Note that there are a number of days/intervals where there are missing values 
# (coded as NA). The presence of missing days may introduce bias into some 
# calculations or summaries of the data.

# 1. Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)

#Total number of rows with missing values:
sum(is.na(activity$steps))


# 2. Devise a strategy for filling in all of the missing values in the dataset.
# The strategy does not need to be sophisticated. For example, you could use the
# mean/median for that day, or the mean for that 5-minute interval, etc

# convert interval to factor
activity$interval2 <- as.factor(activity$interval)
avg_steps_per_interval$interval2 <- as.factor(avg_steps_per_interval$interval)

#merge on interval with daily average
activity <- merge(x = activity, y = avg_steps_per_interval, by.x = "interval2", by.y = "interval2")

# impute NA steps
activity <- activity %>%
    mutate(impute_steps = ifelse(is.na(activity$steps.x),activity$steps.y,activity$steps.x))

str(activity)



# 3. Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.
activity2 <- activity[,c("impute_steps","date","interval.x")]
activity2$steps <- activity2$impute_steps 
activity2$impute_steps <- NULL
activity2$interval <- activity2$interval.x 
activity2$interval.x <- NULL

activity2 <-
 activity2 %>% arrange(date, interval)


# 4. Make a histogram of the total number of steps taken each day and Calculate
# and report the mean and median total number of steps taken per day. Do these 
# values differ from the estimates from the first part of the assignment? What 
# is the impact of imputing missing data on the estimates of the total daily 
# number of steps?

steps_per_day2 <- 
    activity2 %>% 
    group_by(date) %>% 
    summarise_each(funs(sum), steps)
head(steps_per_day2)


# 2. Make a histogram of the total number of steps taken each day
hist(steps_per_day2$steps, breaks = 10)



