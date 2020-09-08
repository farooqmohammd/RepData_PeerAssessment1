# unzipping the activity.zip file 

if(!file.exists('activity.csv')){
  unzip('activity.zip')
  file.remove('activity.zip')
  
}



#loading the data and checking its contents

activity_raw <- read.csv('activity.csv',header = TRUE,na.strings = 'NA')

head(activity_raw)
str(activity_raw)
dim(activity_raw)
summary(activity_raw)

#converting date column to date data type
class(activity_raw$date)
activity_raw$date <- as.Date(activity_raw$date, format = '%Y-%m-%d')
class(activity_raw$date)

#What is mean total number of steps taken per day?

# finding number of distinct dates
length(unique(activity_raw$date))

library(dplyr)
steps_per_day <- activity_raw %>% group_by(date) %>% 
  summarise(steps_per_day = sum(steps))

hist(steps_per_day$steps_per_day, main = 'Histogram of steps taken each day',
     xlab = 'steps per day', ylim = c(0,30), breaks = 6 )


mean_steps <- mean(steps_per_day$steps_per_day, na.rm = TRUE)
median_steps <- median(steps_per_day$steps_per_day, , na.rm = TRUE)

# What is the average daily activity pattern?

average <- activity_raw %>% group_by(interval) %>% 
  summarise(average_steps = mean(steps, na.rm = TRUE))

# time series plot 
library(ggplot2)
ggplot(data = average,aes(x = interval, y = average_steps)) + geom_line() +
  ylab('Average steps') + labs(title = 'average steps per interval')

#Which 5-minute interval, on average across all the days in the dataset, contains 
#the maximum number of steps?

maxINTERVAL <- average[which.max(average$average_steps),]$interval

#Imputing missing values

#Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)

NA_data <- activity_raw[is.na(activity_raw$steps),] 
number_of_NA <- length(NA_data$steps)

# The strategy for filling NA

new_data <- activity_raw

for (i in 1:nrow(new_data)){
  if(is.na(new_data[i,'steps'])){
    new_data[i,'steps'] <- average[which(average$interval==new_data[i,'interval']),'average_steps']
    
  }
}

steps_wo_na <- new_data %>% group_by(date) %>% 
  summarise(sum_steps = sum(steps))

hist(steps_wo_na$sum_steps, main = 'Histogram of steps taken each day after filling NAs',
     xlab = 'steps per day', ylim = c(0,18), breaks = seq(0,23000,1000) )

mean_steps_new <- mean(steps_wo_na$sum_steps, na.rm = TRUE)
median_steps_new <- median(steps_wo_na$sum_steps, na.rm = TRUE)

#Are there differences in activity patterns between weekdays and weekends?

#creating weekday fun
weekday_type <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else 
    return("weekend")
}

new_data$date <- as.Date(new_data$date)

# creating factor variable
new_data$factor <- sapply(new_data$date, weekday_type)

#calculating average steps per day for weekday and weekend
data_weekday <- new_data %>% filter(factor=='weekday') 
data_weekend <- new_data %>% filter(factor=='weekend') 


weekday_avg <- data_weekday %>% group_by(interval) %>% summarise(average = mean(steps))
weekend_avg <- data_weekend %>% group_by(interval) %>% summarise(average = mean(steps))

# plotting time series
par(mfrow=c(2,1), mar=c(4,4,2,1))

plot(average~interval, data=weekday_avg, type="l",
       main="Avg steps for Weekdays", xlab="Time Interval", col="red")
plot(average~interval, data=weekend_avg, type="l",
       main="Avg steps for Weekends", xlab="Time Interval", col="red")


