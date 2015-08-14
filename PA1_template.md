# Reproducible Research: Peer Assessment 1


```r
knitr::opts_chunk$set(fig.width=12, fig.height=8, echo=TRUE, warning=FALSE, message=FALSE, fig.path='figure/')
```


## Loading and preprocessing the data

```r
activityFile <- unzip("activity.zip")
activity <- read.csv(activityFile)
activityNoNA <- na.omit(activity)
```




## What is mean total number of steps taken per day?

```r
library(plyr)
library(dplyr)
stepsPerDayTotal <- ddply(activityNoNA, c("date"), summarize, total = sum(steps))
summary(stepsPerDayTotal)
```

```
##          date        total      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

```r
meanTotal <- mean(stepsPerDayTotal$total)

print(paste("mean total number of steps is ", meanTotal))
```

```
## [1] "mean total number of steps is  10766.1886792453"
```


### Plotting the histogram ###

```r
library(ggplot2)
##png(file="histogram.png", width=480, height=480)
g <- ggplot(stepsPerDayTotal, aes(x=total))
binsize <- 5000
theplot <- g + geom_histogram(binwidth=binsize, fill="white", color="red", origin=0)

theplot <- theplot + xlab("Total number of steps") + ggtitle("Histogram of the total number\n of steps taken each day")
theplot <- theplot + scale_y_continuous(limits=c(0, 30), breaks=seq(0,30,5))
print(theplot)
```

![](figure/plotHist-1.png) 

```r
##dev.off()
```



## What is the average daily activity pattern?

```r
by_interval <- group_by(activityNoNA, interval)
stepsPerIntervalAvg <- dplyr::summarize(by_interval, avg = mean(steps))

## find out the interval time with the maximum number of steps
intervalMax <- which(stepsPerIntervalAvg$avg == max(stepsPerIntervalAvg$avg))
intervalTime <- stepsPerIntervalAvg$interval[intervalMax]
print(paste("The interval time with the maximum number of steps is ", intervalTime))
```

```
## [1] "The interval time with the maximum number of steps is  835"
```
### Plotting the pattern ###

```r
##png(file="pattern.png", width=480, height=480)
g <- ggplot(stepsPerIntervalAvg, aes(x=interval, y=avg))
theplot <- g + geom_line(color="black") + xlab("Interval") + ylab("Average number of steps") + ggtitle("A time series plot across all days")
theplot <- theplot + annotate("rect", xmin=800, xmax=900, ymin=0, ymax=225, alpha=.1,fill="red")
theplot <- theplot + annotate("text", x=1500, y=180, label="maximum number of steps\nat the interval 835 ")
print(theplot)
```

![](figure/plotPattern-1.png) 

```r
##dev.off()
```


## Imputing missing values

```r
NAsteps <- filter(activity, is.na(steps))
print(paste("The number of records with missing values is ", nrow(NAsteps)))
```

```
## [1] "The number of records with missing values is  2304"
```

```r
## make a copy of the dataset, fill in NA steps with the mean for that 5-minute interval
activityCopy <- activity
criteria <- is.na(activityCopy$steps) & activityCopy$interval == stepsPerIntervalAvg$interval
activityCopy[criteria, c("steps")] <- stepsPerIntervalAvg$avg

stepsPerDayTotalCopy <- ddply(activityCopy, c("date"), summarize, total = sum(steps))
summary(stepsPerDayTotalCopy)
```

```
##          date        total      
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 9819  
##  2012-10-03: 1   Median :10766  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```
### Plotting the histogram ###

```r
##png(file="histogram_NAreplaced.png", width=480, height=480)
g <- ggplot(stepsPerDayTotalCopy, aes(x=total))
binsize <- 5000
theplot <- g + geom_histogram(binwidth=binsize, fill="white", colour="red", origin=0)

theplot <-  theplot + xlab("Total number of steps") + ggtitle("Histogram of the total number\n of steps taken each day")
theplot <- theplot + scale_x_continuous(breaks=seq(0, 30000, 5000)) +  scale_y_continuous(limits=c(0,40), breaks=seq(0, 50, 5)) 
print(theplot)
```

![](figure/plotHistReplaced-1.png) 

```r
##dev.off()
```



## Are there differences in activity patterns between weekdays and weekends?

```r
library(lubridate)
daysOfWeek <- as.factor(c("weekend", "weekday","weekday","weekday","weekday","weekday", "weekend"))
activityCopy <- mutate(activityCopy, daytype = daysOfWeek[wday(date)])

stepsBydaytype <- ddply(activityCopy, c("daytype","interval"), summarize, avg = mean(steps))

### Display the panel plot ###
##png(file="panel.png", width=480, height=480)
g <- ggplot(stepsBydaytype, aes(x=interval, y=avg))
theplot <- g + geom_line(color="red") + labs(x="Interval", y="Average number of steps") + facet_wrap(~daytype, nrow=2)

theplot <- theplot + scale_x_continuous(limits=c(0,2355), breaks=seq(0, 2355, 250))

print(theplot)
```

![](figure/plotPanel-1.png) 

```r
##dev.off()
```

