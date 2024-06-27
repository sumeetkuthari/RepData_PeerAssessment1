---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Check if the data is unzipped.
If not then we'll unzip it.

```r
if(!file.exists("activity.csv")){
  unzip("activity.zip")
}
```

Read the data into a data frame.

```r
activity <- read.table("activity.csv", header = TRUE, sep = ",")
activity$date <- as.POSIXct(activity$date, format = "%Y-%m-%d")
```
## What is mean total number of steps taken per day?
Calculate the total number of steps taken per day.

```r
totalSteps <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```

Make a histogram of the total number of steps taken each day.

```r
hist(totalSteps$steps, main = "Total Steps per Day", xlab = "Number of Steps", ylab = "Frequency", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day.

```r
meanSteps <- mean(totalSteps$steps)
medianSteps <- median(totalSteps$steps)
print("Mean of the total number of steps taken per day:")
```

```
## [1] "Mean of the total number of steps taken per day:"
```

```r
print(meanSteps)
```

```
## [1] 10766.19
```

```r
print("Median of the total number of steps taken per day:")
```

```
## [1] "Median of the total number of steps taken per day:"
```

## What is the average daily activity pattern?
Calculate the average number of steps taken, averaged across all days, at each 5-minute interval.

```r
averageSteps <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
```

Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days.

```r
plot(averageSteps$interval, averageSteps$steps, type = "l", xlab = "5-minute interval", ylab = "Average number of steps", main = "Average number of steps taken per 5-minute interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxInterval <- averageSteps[which.max(averageSteps$steps), ]$interval
print(maxInterval)
```

```
## [1] 835
```
The 5-minute interval that contains the maximum number of steps is 835.
## Imputing missing values
Calculate and report the total number of missing values in the dataset.

```r
missingValues <- sum(is.na(activity$steps))
```

The total number of missing values in the dataset is 2304.

Devise a strategy for filling in all of the missing values in the dataset.

```r
activityImputed <- activity
activityImputed$steps[is.na(activityImputed$steps)] <- mean(activityImputed$steps, na.rm = TRUE)
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
totalStepsImputed <- aggregate(steps ~ date, data = activityImputed, sum)
```

Make a histogram of the total number of steps taken each day.

```r
hist(totalStepsImputed$steps, main = "Total Steps per Day", xlab = "Number of Steps", ylab = "Frequency", col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day.

```r
meanStepsImputed <- mean(totalStepsImputed$steps)
medianStepsImputed <- median(totalStepsImputed$steps)
```

The mean of the total number of steps taken per day is 1.0766189\times 10^{4} and the median is 1.0766189\times 10^{4}.
## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
activityImputed$day <- weekdays(activityImputed$date)
activityImputed$dayType <- ifelse(activityImputed$day %in% c("Saturday", "Sunday"), "weekend", "weekday")
activityImputed$dayType <- factor(activityImputed$dayType)
```

Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.

```r
averageStepsImputed <- aggregate(steps ~ interval + dayType, data = activityImputed, mean)
library(lattice)
xyplot(steps ~ interval | dayType, data = averageStepsImputed, layout = c(1, 2), type = "l", xlab = "5-minute interval", ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
