# Reproducible Research: Peer Assessment 1

It is expected your current working directory is set to application root path in order to see all the files, existing in the assessment repository

## Loading and preprocessing the data



Let's convert *date* variable to ```Date``` type



## What is mean total number of steps taken per day?

Let us produce histogram of total steps taken per day


```r
stepsPerDay <- aggregate(steps ~ date, data = data, FUN = function(x) c(steps = sum(x, na.rm = TRUE)))

plot(stepsPerDay$date, stepsPerDay$steps, type = "h", xlab = "Date", ylab = "Steps", lwd = c(5))
```

![](./PA1_template_files/figure-html/plotHist-1.png) 

Oddly, we see median for the whole ```steps``` column is equal to zero, so as in the day-by-day report. This gives a presumption of data having a lot of zero values


```r
summary(data$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```

```r
stats <- aggregate(steps ~ date, data = data, FUN = function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE)))
stats
```

```
##          date steps.mean steps.median
## 1  2012-10-02  0.4375000    0.0000000
## 2  2012-10-03 39.4166667    0.0000000
## 3  2012-10-04 42.0694444    0.0000000
## 4  2012-10-05 46.1597222    0.0000000
## 5  2012-10-06 53.5416667    0.0000000
## 6  2012-10-07 38.2465278    0.0000000
## 7  2012-10-09 44.4826389    0.0000000
## 8  2012-10-10 34.3750000    0.0000000
## 9  2012-10-11 35.7777778    0.0000000
## 10 2012-10-12 60.3541667    0.0000000
## 11 2012-10-13 43.1458333    0.0000000
## 12 2012-10-14 52.4236111    0.0000000
## 13 2012-10-15 35.2048611    0.0000000
## 14 2012-10-16 52.3750000    0.0000000
## 15 2012-10-17 46.7083333    0.0000000
## 16 2012-10-18 34.9166667    0.0000000
## 17 2012-10-19 41.0729167    0.0000000
## 18 2012-10-20 36.0937500    0.0000000
## 19 2012-10-21 30.6284722    0.0000000
## 20 2012-10-22 46.7361111    0.0000000
## 21 2012-10-23 30.9652778    0.0000000
## 22 2012-10-24 29.0104167    0.0000000
## 23 2012-10-25  8.6527778    0.0000000
## 24 2012-10-26 23.5347222    0.0000000
## 25 2012-10-27 35.1354167    0.0000000
## 26 2012-10-28 39.7847222    0.0000000
## 27 2012-10-29 17.4236111    0.0000000
## 28 2012-10-30 34.0937500    0.0000000
## 29 2012-10-31 53.5208333    0.0000000
## 30 2012-11-02 36.8055556    0.0000000
## 31 2012-11-03 36.7048611    0.0000000
## 32 2012-11-05 36.2465278    0.0000000
## 33 2012-11-06 28.9375000    0.0000000
## 34 2012-11-07 44.7326389    0.0000000
## 35 2012-11-08 11.1770833    0.0000000
## 36 2012-11-11 43.7777778    0.0000000
## 37 2012-11-12 37.3784722    0.0000000
## 38 2012-11-13 25.4722222    0.0000000
## 39 2012-11-15  0.1423611    0.0000000
## 40 2012-11-16 18.8923611    0.0000000
## 41 2012-11-17 49.7881944    0.0000000
## 42 2012-11-18 52.4652778    0.0000000
## 43 2012-11-19 30.6979167    0.0000000
## 44 2012-11-20 15.5277778    0.0000000
## 45 2012-11-21 44.3993056    0.0000000
## 46 2012-11-22 70.9270833    0.0000000
## 47 2012-11-23 73.5902778    0.0000000
## 48 2012-11-24 50.2708333    0.0000000
## 49 2012-11-25 41.0902778    0.0000000
## 50 2012-11-26 38.7569444    0.0000000
## 51 2012-11-27 47.3819444    0.0000000
## 52 2012-11-28 35.3576389    0.0000000
## 53 2012-11-29 24.4687500    0.0000000
```

## What is the average daily activity pattern?


```r
stepsPerInterval <- aggregate(steps ~ interval, data = data, FUN = function(x) c(steps = mean(x, na.rm = TRUE)))

plot(stepsPerInterval$interval, stepsPerInterval$steps, type = "l", xlab = "Interval", ylab = "Average of steps")
```

![](./PA1_template_files/figure-html/plotIntervals-1.png) 


As we can see the highest average of 206.1698113 is reached during 835 interval

## Imputing missing values


There are 2304 missing values for ```steps``` variable in our dataset. Let us try to autopopulate them by following algorithm:

* Populate missing values with zeroes
* Replace those zeroes with mean value for the particular day
  * Replaced values will naturally be very low because of the first step



Statistics and plot with prepopulated values is as follows:


```r
newStats <- aggregate(steps ~ date, data = newData, FUN = function(x) c(mean = mean(x), median = median(x)))
newStats
```

```
##          date steps.mean steps.median
## 1  2012-10-01  0.0000000    0.0000000
## 2  2012-10-02  0.4375000    0.0000000
## 3  2012-10-03 39.4166667    0.0000000
## 4  2012-10-04 42.0694444    0.0000000
## 5  2012-10-05 46.1597222    0.0000000
## 6  2012-10-06 53.5416667    0.0000000
## 7  2012-10-07 38.2465278    0.0000000
## 8  2012-10-08  0.0000000    0.0000000
## 9  2012-10-09 44.4826389    0.0000000
## 10 2012-10-10 34.3750000    0.0000000
## 11 2012-10-11 35.7777778    0.0000000
## 12 2012-10-12 60.3541667    0.0000000
## 13 2012-10-13 43.1458333    0.0000000
## 14 2012-10-14 52.4236111    0.0000000
## 15 2012-10-15 35.2048611    0.0000000
## 16 2012-10-16 52.3750000    0.0000000
## 17 2012-10-17 46.7083333    0.0000000
## 18 2012-10-18 34.9166667    0.0000000
## 19 2012-10-19 41.0729167    0.0000000
## 20 2012-10-20 36.0937500    0.0000000
## 21 2012-10-21 30.6284722    0.0000000
## 22 2012-10-22 46.7361111    0.0000000
## 23 2012-10-23 30.9652778    0.0000000
## 24 2012-10-24 29.0104167    0.0000000
## 25 2012-10-25  8.6527778    0.0000000
## 26 2012-10-26 23.5347222    0.0000000
## 27 2012-10-27 35.1354167    0.0000000
## 28 2012-10-28 39.7847222    0.0000000
## 29 2012-10-29 17.4236111    0.0000000
## 30 2012-10-30 34.0937500    0.0000000
## 31 2012-10-31 53.5208333    0.0000000
## 32 2012-11-01  0.0000000    0.0000000
## 33 2012-11-02 36.8055556    0.0000000
## 34 2012-11-03 36.7048611    0.0000000
## 35 2012-11-04  0.0000000    0.0000000
## 36 2012-11-05 36.2465278    0.0000000
## 37 2012-11-06 28.9375000    0.0000000
## 38 2012-11-07 44.7326389    0.0000000
## 39 2012-11-08 11.1770833    0.0000000
## 40 2012-11-09  0.0000000    0.0000000
## 41 2012-11-10  0.0000000    0.0000000
## 42 2012-11-11 43.7777778    0.0000000
## 43 2012-11-12 37.3784722    0.0000000
## 44 2012-11-13 25.4722222    0.0000000
## 45 2012-11-14  0.0000000    0.0000000
## 46 2012-11-15  0.1423611    0.0000000
## 47 2012-11-16 18.8923611    0.0000000
## 48 2012-11-17 49.7881944    0.0000000
## 49 2012-11-18 52.4652778    0.0000000
## 50 2012-11-19 30.6979167    0.0000000
## 51 2012-11-20 15.5277778    0.0000000
## 52 2012-11-21 44.3993056    0.0000000
## 53 2012-11-22 70.9270833    0.0000000
## 54 2012-11-23 73.5902778    0.0000000
## 55 2012-11-24 50.2708333    0.0000000
## 56 2012-11-25 41.0902778    0.0000000
## 57 2012-11-26 38.7569444    0.0000000
## 58 2012-11-27 47.3819444    0.0000000
## 59 2012-11-28 35.3576389    0.0000000
## 60 2012-11-29 24.4687500    0.0000000
## 61 2012-11-30  0.0000000    0.0000000
```


```r
plot(aggregate(steps ~ date, data = newData, FUN = function(x) c(steps = sum(x))), type = "h", xlab = "Date", ylab = "Steps", lwd = c(5))
```

![](./PA1_template_files/figure-html/newPlotHist-1.png) 

Let us calculate what is the difference between original data set and modified


```r
differ <- merge(stats, newStats, by = "date")
differ$steps.x[, "mean"] - differ$steps.y[, "mean"]
```

```
##  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [36] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
```

```r
differ$steps.x[, "median"] - differ$steps.y[, "median"]
```

```
##  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [36] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
```

All zeroes says to us that no missing values where present during any other day but fully throughout the day. which is why we have smaller joined table of difference comparison

## Are there differences in activity patterns between weekdays and weekends?

Add extra columns to prepopulated dataset, identifying if the day is weekday or weekend and plot everything:


```r
newData <- transform(newData, weekday = as.POSIXlt(date)$wday <= 5)
newData <- transform(newData, weekend = as.POSIXlt(date)$wday > 5)

newStepsPerIntervalWeekday <- aggregate(steps ~ interval, data = newData[newData$weekday == TRUE, ], FUN = function(x) c(steps = mean(x)))
newStepsPerIntervalWeekend <- aggregate(steps ~ interval, data = newData[newData$weekend == TRUE, ], FUN = function(x) c(steps = mean(x)))

par(mfrow = c(2, 1))

plot(newStepsPerIntervalWeekday$interval, newStepsPerIntervalWeekday$steps, type = "l", xlab = "Interval", ylab = "Average of steps during weekdays")
plot(newStepsPerIntervalWeekend$interval, newStepsPerIntervalWeekend$steps, type = "l", xlab = "Interval", ylab = "Average of steps during weekends")
```

![](./PA1_template_files/figure-html/weekDays-1.png) 
