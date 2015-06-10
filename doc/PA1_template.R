# Loading and preprocessing the data
getwd()
setwd("D:/R//Data Science/lesson5/pro1")

df <- read.csv(file = "activity.csv", header = T)
head(df)
summary(df)
str(df)

# 1 What is mean total number of steps taken per day?
## 1.1 Calculate the total number of steps taken per day
steps_per_day <- aggregate(df[c("steps")], list(date = df$date), FUN = sum, na.rm = T)
steps_per_day

## 1.2 Make a histogram of the total number of steps taken each day
hist(steps_per_day$steps, col = "red", xlab = "Steps", main = "Histogram of Total Number of Steps Each Day")
rug(steps_per_day$steps)

## 1.3 Calculate and report the mean and median of the total number of steps taken per day
mean(steps_per_day$steps)
median(steps_per_day$steps)

# 2 What is the average daily activity pattern?
## 2.1 Make a time series plot
average_number_of_steps <- aggregate(df[c("steps")], list(interval = df$interval), FUN = mean, na.rm = T)

with(average_number_of_steps, plot(interval, steps, type = "l", col = "red"))
legend("topright", lwd = 1, col = "red", legend = c("steps"))

## 2.2 the 5-minute interval which contains the maximum number of steps
average_number_of_steps$interval[which.max(average_number_of_steps$steps)]

# 3 Imputing missing values
## 3.1 Calculate and report the total number of missing values
sum(is.na(df))

## 3.2 a strategy for imputing missing data
df_2 <- merge(df, average_number_of_steps, by = c("interval"))

library(dplyr)
df_arrange <- arrange(df_2, steps.x)

df_NoNA <- df_arrange[1:15264,]
df_NA <- df_arrange[15265:17568,]

df_NA$steps.x <- df_NA$steps.y

## 3.3 a new dataset with the missing data filled in
df_new <- rbind(df_NoNA, df_NA)
df_new$steps.y <- NULL

sum(is.na(df_new))

## 3.4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
steps_per_day_NoNA <- aggregate(df_new[c("steps.x")], list(date = df_new$date), FUN = sum, na.rm = T)

hist(steps_per_day_NoNA$steps.x, col = "red", xlab = "Steps", main = "Histogram of Total Number of Steps Each Day")
rug(steps_per_day_NoNA$steps.x)

mean(steps_per_day_NoNA$steps.x)
median(steps_per_day_NoNA$steps.x)

# 4 Are there differences in activity patterns between weekdays and weekends?
## 4.1 Create a new factor variable in the dataset with two levels
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

## 4.2 Make a panel plot containing a time series plot
average_number_of_steps_weekday <- aggregate(df_new[c("steps.x")], list(interval = df_new$interval, weekday = df_new$weekday.2), FUN = mean, na.rm = T)

library(ggplot2)
ggplot(average_number_of_steps_weekday, aes(x = interval, y = steps.x)) + geom_line(aes(color = weekday)) + facet_grid(weekday ~ .)

library(knitr)
knit2html("PA1_template.Rmd")
