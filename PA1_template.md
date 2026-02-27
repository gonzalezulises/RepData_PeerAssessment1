---
title: "Reproducible Research: Peer Assessment 1"
output:
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


``` r
unzip("activity.zip")
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

``` r
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```


## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day:


``` r
steps_per_day <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
head(steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

2. Histogram of the total number of steps taken each day:


``` r
hist(steps_per_day$steps,
     main = "Total Number of Steps Taken Each Day",
     xlab = "Steps per Day",
     col = "steelblue",
     breaks = 20)
```

![](PA1_template_files/figure-html/histogram1-1.png)<!-- -->

3. Mean and median of total steps per day:


``` r
mean_steps <- mean(steps_per_day$steps)
median_steps <- median(steps_per_day$steps)
cat("Mean steps per day:", mean_steps, "\n")
```

```
## Mean steps per day: 10766.19
```

``` r
cat("Median steps per day:", median_steps, "\n")
```

```
## Median steps per day: 10765
```


## What is the average daily activity pattern?

1. Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days:


``` r
avg_steps_interval <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)

plot(avg_steps_interval$interval, avg_steps_interval$steps,
     type = "l",
     main = "Average Daily Activity Pattern",
     xlab = "5-Minute Interval",
     ylab = "Average Number of Steps",
     col = "steelblue")
```

![](PA1_template_files/figure-html/timeseries-1.png)<!-- -->

2. Which 5-minute interval contains the maximum number of steps?


``` r
max_interval <- avg_steps_interval[which.max(avg_steps_interval$steps), ]
cat("Interval with maximum average steps:", max_interval$interval, "\n")
```

```
## Interval with maximum average steps: 835
```

``` r
cat("Maximum average steps:", max_interval$steps, "\n")
```

```
## Maximum average steps: 206.1698
```


## Imputing missing values

1. Total number of missing values in the dataset:


``` r
total_na <- sum(is.na(activity$steps))
cat("Total number of missing values:", total_na, "\n")
```

```
## Total number of missing values: 2304
```

2. Strategy: fill missing values with the mean for that 5-minute interval across all days.

3. Create a new dataset with missing data filled in:


``` r
activity_imputed <- activity

for (i in 1:nrow(activity_imputed)) {
    if (is.na(activity_imputed$steps[i])) {
        interval_val <- activity_imputed$interval[i]
        activity_imputed$steps[i] <- avg_steps_interval$steps[avg_steps_interval$interval == interval_val]
    }
}

cat("Missing values after imputation:", sum(is.na(activity_imputed$steps)), "\n")
```

```
## Missing values after imputation: 0
```

4. Histogram of total steps per day after imputation, and mean/median:


``` r
steps_per_day_imputed <- aggregate(steps ~ date, data = activity_imputed, FUN = sum)

hist(steps_per_day_imputed$steps,
     main = "Total Steps Per Day (After Imputation)",
     xlab = "Steps per Day",
     col = "coral",
     breaks = 20)
```

![](PA1_template_files/figure-html/histogram2-1.png)<!-- -->

``` r
mean_imputed <- mean(steps_per_day_imputed$steps)
median_imputed <- median(steps_per_day_imputed$steps)
cat("Mean steps per day (imputed):", mean_imputed, "\n")
```

```
## Mean steps per day (imputed): 10766.19
```

``` r
cat("Median steps per day (imputed):", median_imputed, "\n")
```

```
## Median steps per day (imputed): 10766.19
```

``` r
cat("Difference in mean:", mean_imputed - mean_steps, "\n")
```

```
## Difference in mean: 0
```

``` r
cat("Difference in median:", median_imputed - median_steps, "\n")
```

```
## Difference in median: 1.188679
```

The impact of imputing missing data is minimal: the mean remains unchanged because we used interval means, and the median shifts slightly toward the mean.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a factor variable for weekday/weekend:


``` r
activity_imputed$day_type <- ifelse(
    weekdays(activity_imputed$date) %in% c("Saturday", "Sunday"),
    "weekend", "weekday"
)
activity_imputed$day_type <- factor(activity_imputed$day_type, levels = c("weekday", "weekend"))
table(activity_imputed$day_type)
```

```
## 
## weekday weekend 
##   12960    4608
```

2. Panel plot comparing weekday vs weekend activity patterns:


``` r
avg_by_daytype <- aggregate(steps ~ interval + day_type, data = activity_imputed, FUN = mean)

library(lattice)
xyplot(steps ~ interval | day_type, data = avg_by_daytype,
       type = "l",
       layout = c(1, 2),
       xlab = "Interval",
       ylab = "Number of steps",
       main = "Average Steps: Weekday vs Weekend")
```

![](PA1_template_files/figure-html/panelplot-1.png)<!-- -->
