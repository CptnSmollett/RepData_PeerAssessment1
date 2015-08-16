# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
The first step is load raw data and make it ready for analysis.  
Depending on OS family unzipping code might differ, so we assume that data file is located in current working directory in file activity.csv.  
  
Full script:

```r
rawData = read.csv("activity.csv", header = TRUE, sep = ",")
```


## What is mean total number of steps taken per day?
First we calculate total number of steps per day. Then use this data to build histogram:  

```r
stepsPerDay = aggregate(steps ~ date, data = rawData, FUN = sum)
hist(stepsPerDay$steps, main = "Number of steps taken per day", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 
  
Mean and median number of steps per day:  

```r
sprintf("Mean number of steps: %.2f", mean(stepsPerDay$steps))
```

```
## [1] "Mean number of steps: 10766.19"
```

```r
sprintf("Median number of steps: %.f", median(stepsPerDay$steps))
```

```
## [1] "Median number of steps: 10765"
```


## What is the average daily activity pattern?
In order to answer this question we need to process data in a slightly different way, i.e. aggregate steps by time intervals and take the mean value. Now we're ready to make a plot answering the question raised:  

```r
stepsPerInterval = aggregate(steps ~ interval, data = rawData, FUN = mean)
with(stepsPerInterval,
       plot(interval,
            steps,
            type = "l",
            main = "Average number of steps per 5-min intervals accross all days",
            xlab = "Intervals",
            ylab = "Steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 
  
This interval contains maximum average number of steps per day:  

```r
maxStepsInterval = stepsPerInterval[which.max(stepsPerInterval$steps),]
intervalString = paste0(rep("0", 4 - nchar(maxStepsInterval$interval)), maxStepsInterval$interval, collapse = "")
intervalStartTime = strptime(intervalString, "%H%M", tz = "GMT")
```

```
## Warning in strptime(intervalString, "%H%M", tz = "GMT"): unable to identify current timezone 'B':
## please set environment variable 'TZ'
```

```
## Warning in strptime(intervalString, "%H%M", tz = "GMT"): unknown timezone
## 'localtime'
```

```r
intervalEndTime = intervalStartTime
intervalEndTime$min = intervalEndTime$min + 4
sprintf("Interval is %s-%s and maximum average number of steps is %.2f", format(intervalStartTime, "%H:%M"), format(intervalEndTime, "%H:%M"), maxStepsInterval$steps)
```

```
## [1] "Interval is 08:35-08:39 and maximum average number of steps is 206.17"
```


## Imputing missing values
Missing values might skew results of analysis, so let's count them:  

```r
sprintf("Rows with missing data: %d", sum(is.na(rawData$steps)))
```

```
## [1] "Rows with missing data: 2304"
```
  
Now as we know this number is quite significant we have to fill in missing values. Let's use median value for corresponding intervals for this purpose.  

```r
completeRawData = rawData
medianStepsPerInterval = aggregate(steps ~ interval, data = rawData, FUN = median, na.rm = TRUE)

getMedianValues = function(x, medianValues) {
    res = x[1]
    if (is.na(res))
        res = medianValues[medianValues$interval == as.integer(x[3]), "steps"][1]
    as.integer(res)
}
completeRawData$steps = apply(completeRawData, 1, getMedianValues, medianValues = medianStepsPerInterval)
```
  
Let's see how NA values affected number of steps taken each day.

```r
completeStepsPerDay = aggregate(steps ~ date, data = completeRawData, FUN = sum)
hist(completeStepsPerDay$steps, main = "Number of steps taken per day", xlab = "Steps", col = "blue")
hist(stepsPerDay$steps, col = "white", add = TRUE)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 
  
The difference between data with imputed NA values and raw data is in blue. You can clearly see that in only affected the days with low number of steps taken. Although number of these days has raised from 5 to 12 roughly.  
  
Let's see now how mean and median values differ to raw data:  

```r
sprintf("Mean number of steps with imputed NAs is %.2f. For raw data it is %.2f.", mean(completeStepsPerDay$steps), mean(stepsPerDay$steps))
```

```
## [1] "Mean number of steps with imputed NAs is 9503.87. For raw data it is 10766.19."
```

```r
sprintf("Median number of stepswith imputed NAs is %.2f. For raw data it is %.2f.", median(completeStepsPerDay$steps), median(stepsPerDay$steps))
```

```
## [1] "Median number of stepswith imputed NAs is 10395.00. For raw data it is 10765.00."
```
  
The final question we address in this section is what was the impact of imputing missing data on the estimates of the total daily number of steps? Let's compare summaries of steps taken per day for raw and imputed data.  

```r
summary(stepsPerDay$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

```r
summary(completeStepsPerDay$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    6778   10400    9504   12810   21190
```
Minimum and maximum values did not change. At the same time mean and quartiles are a bit lower for imputed values which well corresponds with the histogram above, i.e. more days with lower number of steps taken. This must be due to ignoring missing values in raw data during summary computations.  

## Are there differences in activity patterns between weekdays and weekends?
First of all lets's tell weekdays from weekends upon compkete data set:  

```r
completeRawData$datePosix = as.POSIXlt(completeRawData$date)
completeRawData$dayType = factor(completeRawData$datePosix$wday %in% c(1:5), labels = c("weekend", "weekday"))
```
  
Now we can see the difference in patterns of steps taken during weekdays and weekends. In order to see it we will use average numebr of steps taken during 5-minute untervals across day types:

```r
completeStepsPerDayType = aggregate(steps ~ dayType + interval, data = completeRawData, FUN = sum)

library(ggplot2)
qplot(interval, steps, data = completeStepsPerDayType, facets = dayType ~ ., geom = "line")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
  
Grpah explicitly states that our test subject had a very relaxed life on weekends, although during weekdays he or she woke up quite early and had a decent walk at roughly 8am-9am interval, probably to the office. We can also see a regular walk in weekday evening at 6pm roughly. The reason behind it might be that the subject has taken a ride to get back home at the end of the day on regular basis.  
  
This is it for this assement :)  
