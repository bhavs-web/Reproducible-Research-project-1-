---
title: "Reproducible Research project 1"
output:
  word_document: default
  html_document: default
---
Reading the file


```r
filedata <- read.csv("activity.csv")
names(filedata)
```

```
## [1] "steps"    "date"     "interval"
```

```r
head(filedata)
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

Calculating total number of steps per day:

```r
stepsperday <- aggregate(steps~date, filedata, sum, na.rm= TRUE)
hist(stepsperday$steps, main = "Steps taken per day", xlab = "stepsperday", ylab = "steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

Calculating mean and median total number of steps per day:

```r
meanstepstakenperday <- mean(stepsperday$steps)
meanstepstakenperday
```

```
## [1] 10766.19
```

```r
medianstepstaken <- median(stepsperday$steps)
medianstepstaken                           
```

```
## [1] 10765
```

Calculating average number of steps taken across all days:

```r
everyinterval<- aggregate(steps~interval, data=filedata, mean, na.rm=TRUE)
plot(steps~interval, data=everyinterval, type="l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Maximum number of steps:

```r
maxsteps <- everyinterval[which.max(everyinterval$steps),]$interval
maxsteps
```

```
## [1] 835
```

Calculating missing values:

```r
missingval <- sum(is.na(filedata$steps))
missingval
```

```
## [1] 2304
```

```r
meaneveryinterval<-function(interval){
    everyinterval[everyinterval$interval==interval,]$steps
}
```

Creating new dataset with missing values:

```r
newdataset <- filedata
for(i in 1:nrow(newdataset)){
    if(is.na(newdataset[i,]$steps)){
        newdataset[i,]$steps <- meaneveryinterval(newdataset[i,]$interval)
    }
}
stepsperdaynew <- aggregate(steps ~ date, data=newdataset, sum)
hist(stepsperdaynew$steps, main = "Total steps per day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

mean and median of new datset: 

```r
meannew <- mean(stepsperdaynew$steps)
meannew
```

```
## [1] 10766.19
```

```r
mediannew <- median(stepsperdaynew$steps)
```

Indicating whether a given date is a weekday or weekend:

```r
newdataset$date <- as.Date(strptime(newdataset$date, format="%Y-%m-%d"))
newdataset$day <- weekdays(newdataset$date)
for (i in 1:nrow(newdataset)) {
    if (newdataset[i,]$day %in% c("Saturday","Sunday")) {
        newdataset[i,]$day<-"weekend"
    }
    else{
        newdataset[i,]$day<-"weekday"
    }
}
stepsbyday <- aggregate(newdataset$steps ~ newdataset$interval + newdataset$day, newdataset, mean)
```

Plotting the average number of steps taken on x-axis and averaged across all weekday days or weekend days on y-axis:

```r
names(stepsbyday) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsbyday, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)










