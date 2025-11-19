---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

``` r
fullData <- read.csv("activity.csv")
fullData$date <- as.Date(fullData$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

``` r
# Total number of steps taken per day

stepsPerDay <- aggregate(steps ~ date, fullData, FUN = sum)

# Histogram of total steps per day

g <- ggplot(stepsPerDay, aes(x = steps))
g + geom_histogram(fill = "yellow", binwidth = 1000) +
labs(title = "Histogram of Steps Taken Each Day",
x = "Steps",
y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

``` r
# Mean of steps

stepsMean <- mean(stepsPerDay$steps, na.rm = TRUE)
stepsMean
```

```
## [1] 10766.19
```

``` r
# Median of steps

stepsMedian <- median(stepsPerDay$steps, na.rm = TRUE)
stepsMedian
```

```
## [1] 10765
```

## What is the average daily activity pattern?

``` r
# Average number of steps per 5-minute interval

stepsPerInterval <- aggregate(steps ~ interval, fullData, mean, na.rm = TRUE)

# Time series plot of average number of steps per interval

h <- ggplot(stepsPerInterval, aes(x = interval, y = steps))
h + geom_line() +
labs(title = "Time Series Plot of Average Steps per Interval",
x = "Interval",
y = "Average Steps across All Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

``` r
# 5-minute interval with maximum average steps

maxInterval <- stepsPerInterval[which.max(stepsPerInterval$steps), ]
maxInterval
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

``` r
# Number of missing values in the original dataset

noMissingValue <- nrow(fullData[is.na(fullData$steps), ])
noMissingValue
```

```
## [1] 2304
```

``` r
# Make a copy of the data

fullData1 <- read.csv("activity.csv", header = TRUE, sep = ",")

# Create a column with weekday names

fullData1$day <- weekdays(as.Date(fullData1$date))

# Average number of steps per 5-minute interval and weekday name

stepsAvg1 <- aggregate(steps ~ interval + day, fullData1, mean, na.rm = TRUE)

# Rows with NA steps

nadata <- fullData1[is.na(fullData1$steps), ]

# Merge NA rows with interval+day averages for substitution

newdata1 <- merge(nadata, stepsAvg1, by = c("interval", "day"))

# Rows without NAs

cleanData <- fullData1[!is.na(fullData1$steps), ]

# Reorder the substituted data to match cleanData structure

# (Drop original NA 'steps' column and keep the averaged steps)

newdata2 <- newdata1[, c(5, 4, 1, 2)]
colnames(newdata2) <- c("steps", "date", "interval", "day")

# Merge the new average data (for NAs) with the non-NA data

mergeData <- rbind(cleanData, newdata2)

# Total steps per day in the merged (imputed) data

stepsPerDayFill <- aggregate(steps ~ date, mergeData, FUN = sum)

# Histogram of total steps per day (imputed data)

g1 <- ggplot(stepsPerDayFill, aes(x = steps))
g1 + geom_histogram(fill = "green", binwidth = 1000) +
labs(title = "Histogram of Steps Taken Each Day (Imputed Data)",
x = "Steps",
y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

``` r
# Mean of total steps with imputed data

stepsMeanFill <- mean(stepsPerDayFill$steps, na.rm = TRUE)
stepsMeanFill
```

```
## [1] 10821.21
```

``` r
# Median of total steps with imputed data

stepsMedianFill <- median(stepsPerDayFill$steps, na.rm = TRUE)
stepsMedianFill
```

```
## [1] 11015
```

## Are there differences in activity patterns between weekdays and weekends?

``` r
# New variable indicating weekday or weekend

mergeData$DayType <- ifelse(mergeData$day %in% c("Saturday", "Sunday"),
"Weekend", "Weekday")

# Average steps per interval across weekday vs weekend

stepsPerIntervalDT <- aggregate(steps ~ interval + DayType,
mergeData,
FUN = mean)

# Panel plot

j <- ggplot(stepsPerIntervalDT, aes(x = interval, y = steps))
j + geom_line() +
labs(title = "Time Series Plot of Average Steps per Interval: Weekdays vs Weekends",
x = "Interval",
y = "Average Number of Steps") +
facet_grid(DayType ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

