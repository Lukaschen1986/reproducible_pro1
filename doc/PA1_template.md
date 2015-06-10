---
title: "PA1_template"
output: html_document
---

# PA1_template

## Loading and preprocessing the data

```r
getwd()
```

```
## [1] "D:/R/Data Science/lesson5/pro1"
```

```r
setwd("D:/R//Data Science/lesson5/pro1")

df <- read.csv(file = "activity.csv", header = T)
head(df)
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
summary(df)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
str(df)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?  
### 1. Calculate the total number of steps taken per day

```r
steps_per_day <- aggregate(df[c("steps")], list(date = df$date), FUN = sum, na.rm = T)
steps_per_day
```

```
##          date steps
## 1  2012-10-01     0
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08     0
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01     0
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04     0
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09     0
## 41 2012-11-10     0
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14     0
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30     0
```

### 2. Make a histogram of the total number of steps taken each day

```r
hist(steps_per_day$steps, col = "red", xlab = "Steps", main = "Histogram of Total Number of Steps Each Day")
rug(steps_per_day$steps)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

### 3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(steps_per_day$steps)
```

```
## [1] 9354.23
```

```r
median(steps_per_day$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?  
### 1. Make a time series plot

```r
average_number_of_steps <- aggregate(df[c("steps")], list(interval = df$interval), FUN = mean, na.rm = T)

with(average_number_of_steps, plot(interval, steps, type = "l", col = "red"))
legend("topright", lwd = 1, col = "red", legend = c("steps"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

### 2. the 5-minute interval which contains the maximum number of steps

```r
average_number_of_steps$interval[which.max(average_number_of_steps$steps)]
```

```
## [1] 835
```

## Imputing missing values  
### 1. Calculate and report the total number of missing values

```r
sum(is.na(df))
```

```
## [1] 2304
```

### 2. a strategy for imputing missing data

```r
df_2 <- merge(df, average_number_of_steps, by = c("interval"))

library(dplyr)
df_arrange <- arrange(df_2, steps.x)

df_NoNA <- df_arrange[1:15264,]
df_NA <- df_arrange[15265:17568,]

df_NA$steps.x <- df_NA$steps.y
```

### 3. a new dataset with the missing data filled in

```r
df_new <- rbind(df_NoNA, df_NA)
df_new$steps.y <- NULL

sum(is.na(df_new))
```

```
## [1] 0
```

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
steps_per_day_NoNA <- aggregate(df_new[c("steps.x")], list(date = df_new$date), FUN = sum, na.rm = T)

hist(steps_per_day_NoNA$steps.x, col = "red", xlab = "Steps", main = "Histogram of Total Number of Steps Each Day")
rug(steps_per_day_NoNA$steps.x)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
mean(steps_per_day_NoNA$steps.x)
```

```
## [1] 10766.19
```

```r
median(steps_per_day_NoNA$steps.x)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?  
### 1. Create a new factor variable in the dataset with two levels

```r
df_new$date <- as.Date(df_new$date)
df_new$weekday <- format(df_new$date, format = "%a")

df_new$weekday.2[df_new$weekday == "Mon"] <- "weekday"
df_new$weekday.2[df_new$weekday == "Tue"] <- "weekday"
df_new$weekday.2[df_new$weekday == "Wed"] <- "weekday"
df_new$weekday.2[df_new$weekday == "Thu"] <- "weekday"
df_new$weekday.2[df_new$weekday == "Fri"] <- "weekday"
df_new$weekday.2[df_new$weekday == "Sat"] <- "weekend"
df_new$weekday.2[df_new$weekday == "Sun"] <- "weekend"

table(df_new$weekday.2)
```

```
## 
## weekday weekend 
##   12960    4608
```

### 2. Make a panel plot containing a time series plot

```r
average_number_of_steps_weekday <- aggregate(df_new[c("steps.x")], list(interval = df_new$interval, weekday = df_new$weekday.2), FUN = mean, na.rm = T)

library(ggplot2)
ggplot(average_number_of_steps_weekday, aes(x = interval, y = steps.x)) + geom_line(aes(color = weekday)) + facet_grid(weekday ~ .)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
