---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
The repository containing zip data file was cloned from github.

```r
# unzipping the activity.zip file 

if(!file.exists('activity.csv')){
  unzip('activity.zip')
  file.remove('activity.zip')
}
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

After checking the contents, date column was converted to date class.

```r
activity_raw <- read.csv('activity.csv',header = TRUE,na.strings = 'NA')

head(activity_raw)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity_raw)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
dim(activity_raw)
```

```
## [1] 17568     3
```

```r
summary(activity_raw)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
#converting date column to date data type
class(activity_raw$date)
```

```
## [1] "character"
```

```r
activity_raw$date <- as.Date(activity_raw$date, format = '%Y-%m-%d')
```


## What is mean total number of steps taken per day?
finding distinct dates in the dataset.

```r
# finding number of distinct dates
length(unique(activity_raw$date))
```

```
## [1] 61
```

Plotting the histogram and calculating the average number of steps 

```r
steps_per_day <- activity_raw %>% group_by(date) %>% 
  summarise(steps_per_day = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(steps_per_day$steps_per_day, main = 'Histogram of steps taken each day',
     xlab = 'steps per day', ylim = c(0,30), breaks = 6 )
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
mean_steps <- mean(steps_per_day$steps_per_day, na.rm = TRUE)
median_steps <- median(steps_per_day$steps_per_day,  na.rm = TRUE)

mean_steps
```

```
## [1] 10766.19
```

```r
median_steps
```

```
## [1] 10765
```
mean of the total number of steps taken per day is 1.0766189\times 10^{4}
median of the total number of steps taken per day is 10765

## What is the average daily activity pattern?

Grouping the data by inteval.

```r
average <- activity_raw %>% group_by(interval) %>% 
  summarise(average_steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
average
```

```
## # A tibble: 288 x 2
##    interval average_steps
##       <int>         <dbl>
##  1        0        1.72  
##  2        5        0.340 
##  3       10        0.132 
##  4       15        0.151 
##  5       20        0.0755
##  6       25        2.09  
##  7       30        0.528 
##  8       35        0.868 
##  9       40        0     
## 10       45        1.47  
## # ... with 278 more rows
```



```r
# time series plot 
ggplot(data = average, aes(x = interval, y = average_steps)) + geom_line() +
  ylab('Average steps') + labs(title = 'average steps per interval')
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


```r
#Which 5-minute interval, on average across all the days in the dataset, contains 
#the maximum number of steps?

maxINTERVAL <- average[which.max(average$average_steps),]$interval

maxINTERVAL
```

```
## [1] 835
```


## Imputing missing values

```r
#Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)

NA_data <- activity_raw[is.na(activity_raw$steps),] 
number_of_NA <- length(NA_data$steps)

number_of_NA
```

```
## [1] 2304
```

total number of missing rows is 2304


```r
# The strategy for filling NA

new_data <- activity_raw

for (i in 1:nrow(new_data)){
  if(is.na(new_data[i,'steps'])){
    new_data[i,'steps'] <- average[which(average$interval==new_data[i,'interval']),'average_steps']
    
  }
}

head(new_data)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```
 
plotting the histogram

```r
steps_wo_na <- new_data %>% group_by(date) %>% 
  summarise(sum_steps = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(steps_wo_na$sum_steps, main = 'Histogram of steps taken each day after filling NAs',
     xlab = 'steps per day', ylim = c(0,18), breaks = seq(0,23000,1000) )
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->



```r
mean_steps_new <- mean(steps_wo_na$sum_steps, na.rm = TRUE)
median_steps_new <- median(steps_wo_na$sum_steps, na.rm = TRUE)

mean_steps_new
```

```
## [1] 10766.19
```

```r
median_steps_new
```

```
## [1] 10766.19
```
* The new mean of the total number of steps taken per day is 1.0766189\times 10^{4}
* The new median of the total number of steps taken per day is 1.0766189\times 10^{4}

### What is the impact of imputing missing data on the estimates of the total daily number of steps?
The mean of number of steps has not increased. But there is a slight increase in median which is expected.

## Are there differences in activity patterns between weekdays and weekends?

```r
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
```

subsetting by the weekday factor

```r
#calculating average steps per day for weekday and weekend
data_weekday <- new_data %>% filter(factor=='weekday') 
data_weekend <- new_data %>% filter(factor=='weekend') 


weekday_avg <- data_weekday %>% group_by(interval) %>% summarise(average = mean(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
weekend_avg <- data_weekend %>% group_by(interval) %>% summarise(average = mean(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```


```r
# plotting time series
par(mfrow=c(2,1), mar=c(4,4,2,1))

plot(average~interval, data=weekday_avg, type="l",
       main="Avg steps for Weekdays", xlab="Time Interval", col="red")
plot(average~interval, data=weekend_avg, type="l",
       main="Avg steps for Weekends", xlab="Time Interval", col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->



