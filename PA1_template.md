# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
a <- read.csv("./activity.csv", header = TRUE)
head(a)
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
str(a)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Then split the data frame into convenient variables representing each name


```r
steps <- a$steps
dates <- a$date
interval <- a$interval
```

## What is mean total number of steps taken per day?
First determine the total number of steps per day; placed in a variable sumday


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.1
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
timedat <- group_by(a, date)
sumday <- summarise(timedat, totalsteps = sum(steps, na.rm=TRUE))
hist(sumday$totalsteps, breaks = 9, main = "Histogram of Total Steps Per Day", xlab = "Total Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
meantot <- mean(sumday$totalsteps)
medtot <- median(sumday$totalsteps)
```

The mean total number of steps taken per day is 9354.2295082
The median total number of steps taken per day is 10395


## What is the average daily activity pattern?
Group data by interval, then summarize the interval data by average steps, which collapses all the steps per day


```r
library(dplyr)
intdat <- group_by(a, interval)
intday <- summarise(intdat, avgsteps = mean(steps, na.rm=TRUE))
plot(intday$interval, intday$avgsteps, type="l", main="Time series: time interval vs average steps taken", xlab = "Day Interval (min)", ylab="average steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
maxint <- intday$interval[which(intday$avgsteps == max(intday$avgsteps))]
```

The highest activity interval on average is at the 835 interval.


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
