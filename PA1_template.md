# Reproducible Research: Peer Assessment 1

This is the analysis of the "Activity Monitoring Data" in which the variables are: 1) steps: no. of steps taken in a 5-minute interval. 2) date: Date on which the measurement was taken. 3) interval: Identifier of the 5-minute interval


## Loading and preprocessing the data
The data is loaded using read.csv() command in R.

```r
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
```

## Histogram and Mean/Median of total no. of steps taken per day

```r
activity$date <- as.Date(activity$date, "%Y-%m-%d")
step <- log(na.omit(activity$steps))
hist(step, col = "blue", main = "Hist of Steps", xlab = "Steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

####The mean and median of total no. of steps taken is as below: 

```r
mean(activity$steps,na.rm=TRUE)
```

```
## [1] 37.38
```

```r
median(activity$steps,na.rm=TRUE)
```

```
## [1] 0
```

_______________________________________________________________________________

## Average daily activity pattern

```r
library(ggplot2)
library(plyr)
avgdaily = ddply(activity, .(interval), summarise, mean = mean(steps, na.rm = TRUE))
chart = qplot(interval, mean, data = avgdaily, ylab = "Average Number Of Steps", geom = "line")
chart + ggtitle("AVG DAILY ACTIVITY PATTERN")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

####5-min interval corresponding to the average max no. of steps 

```r
avgdaily$interval[which.max(avgdaily$mean)]
```

```
## [1] 835
```
#### Total number of incomplete cases

```r
nrow(activity) - sum(complete.cases(activity))
```

```
## [1] 2304
```

_______________________________________________________________________________


## Imputing missing values
#### The missing values in steps is imputed based on the average no. of steps in the dataset. And a new dataset is created with all NAs imputed. Finally, a histogram is created to depict the log transformation of no. of steps taken each day.

```r
library(Hmisc)
new_activity <- activity
new_activity$steps <- as.integer(with(new_activity, impute(steps, mean)))

new_step <- log(na.omit(new_activity$steps))
hist(new_step, col = "green", main = "Hist of Steps", xlab = "Steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

#### Mean and Median of "steps" of the imputed data set is calculated below.

```r
mean(new_activity$steps,na.rm=TRUE)
```

```
## [1] 37.33
```

```r
median(new_activity$steps,na.rm=TRUE)
```

```
## [1] 0
```
#### It is clear from the above that the Mean of "steps" in the imputed data set has redcued a bit but the Median has remained the same at 0.

_______________________________________________________________________________

## Differences in activity patterns between weekdays and weekends
#### This is the final analysis step where we try to figure out if there is any difference in activity patterns between weekdays and weekends. For the analysis done below, we consider "FALSE" indicating Weekdays and "TRUE" indicating weekends.


#### First we use the is.weekend function in the "chron" library to determine weekends and weekdays in the 'activity data'


```r
library(chron)
for (i in 1:nrow(new_activity)){
        new_activity$Weekend[i] <- is.weekend(new_activity$date[i])
    }
```
#### Now, we melt the data using "ddply" and calculate the averages of steps during weekends and weekdays

```r
avgWeekend <- ddply(new_activity, .(Weekend, interval), summarise, mean(steps))
names(avgWeekend)[3] = c("Average")
```

#### Finally, we plot activity pattern averages over weekdays and weekends. Asindicated earlier, "False" indicates WeekDays and "True" indicates WeekEnds


```r
chartplot <- qplot(interval, Average, data = avgWeekend, facets = Weekend ~ .) 
chartplot + labs(title = "Diff in activity patterns during weekdays(FALSE) and weekends (TRUE)",x = "Interval", y = "Average daily steps") + geom_line()
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
_______________________________________________________________________________
