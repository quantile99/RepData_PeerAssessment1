###############################################
# What is the average daily activity pattern? #
###############################################

# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)

# Average over days
avg_steps_per_interval <- 
    activity %>% 
    filter(!is.na(steps)) %>%
    group_by(interval) %>% 
    summarise_each(funs(mean), steps)

# Convert interval to date
avg_steps_per_interval <- 
    avg_steps_per_interval %>%
    mutate(hour =  sprintf("%02d", (interval %/% 100))  ,
              minute = sprintf("%02d", (interval %% 100))  ,
              timeStr = paste(hour, minute, sep = ":"))
    
avg_steps_per_interval$time <-          
    strptime(avg_steps_per_interval$timeStr, format = "%H:%M")

# Plot
with(avg_steps_per_interval,
    plot(time, steps, pch = "", type = "l"))


# 2. Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?

avg_steps_per_interval[avg_steps_per_interval$steps == max(avg_steps_per_interval$steps), 5]

