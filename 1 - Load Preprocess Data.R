######################################
# Loading and preprocessing the data #
######################################
# The variables included in this dataset are:
# -steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
# -date: The date on which the measurement was taken in YYYY-MM-DD format
# -interval: Identifier for the 5-minute interval in which measurement was taken

# 1. Load the data (i.e. read.csv())
activity <- read.csv("activity.csv", stringsAsFactors = T)
str(activity)

# 2. Process/transform the data (if necessary) into a format suitable for your analysis