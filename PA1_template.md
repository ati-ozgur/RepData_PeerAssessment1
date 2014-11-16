---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
  word_document: default
---

## Libraries 

```r
library("dplyr")
```




## Loading and preprocessing the data
### Unzip Data File

```r
if(!file.exists("activity.csv"))
{
  unzip("activity.zip")
}
```
### Load data

```r
dataActivity <- read.csv("activity.csv",stringsAsFactors=FALSE)
```


### Some summary statistics

```r
summary(dataActivity)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

How many days exists in data set?


```r
unique(dataActivity$date)
```

```
##  [1] "2012-10-01" "2012-10-02" "2012-10-03" "2012-10-04" "2012-10-05"
##  [6] "2012-10-06" "2012-10-07" "2012-10-08" "2012-10-09" "2012-10-10"
## [11] "2012-10-11" "2012-10-12" "2012-10-13" "2012-10-14" "2012-10-15"
## [16] "2012-10-16" "2012-10-17" "2012-10-18" "2012-10-19" "2012-10-20"
## [21] "2012-10-21" "2012-10-22" "2012-10-23" "2012-10-24" "2012-10-25"
## [26] "2012-10-26" "2012-10-27" "2012-10-28" "2012-10-29" "2012-10-30"
## [31] "2012-10-31" "2012-11-01" "2012-11-02" "2012-11-03" "2012-11-04"
## [36] "2012-11-05" "2012-11-06" "2012-11-07" "2012-11-08" "2012-11-09"
## [41] "2012-11-10" "2012-11-11" "2012-11-12" "2012-11-13" "2012-11-14"
## [46] "2012-11-15" "2012-11-16" "2012-11-17" "2012-11-18" "2012-11-19"
## [51] "2012-11-20" "2012-11-21" "2012-11-22" "2012-11-23" "2012-11-24"
## [56] "2012-11-25" "2012-11-26" "2012-11-27" "2012-11-28" "2012-11-29"
## [61] "2012-11-30"
```




## What is mean total number of steps taken per day?



```r
dataActivityNARemoved <- dataActivity[complete.cases(dataActivity),]

totalsGroupedByDate <- dataActivityNARemoved %>%
  group_by(date) %>%
  select(steps, date) %>%
  summarise(
    sumStep = sum(steps, na.rm = TRUE)
  ) 
breakSize <-dim(totalsGroupedByDate)[1]
hist(totalsGroupedByDate$sumStep,breaks=breakSize,main="Total Steps taken each day",ylab="totalSteps",xlab="date")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

### mean 

```r
mean(totalsGroupedByDate$sumStep)
```

```
## [1] 10766.19
```

### median 


```r
median(totalsGroupedByDate$sumStep)
```

```
## [1] 10765
```



## What is the average daily activity pattern?

```r
averageGroupedByInterval <- dataActivity %>%
  group_by(interval) %>%
  select(steps, interval) %>%
  summarise(
    averageStep = mean(steps, na.rm = TRUE)
  ) 

plot(averageGroupedByInterval$interval,averageGroupedByInterval$averageStep,type="l",xlab="intervals",ylab="average steps for every day",main="average number of steps taken each 5 minute interval")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxSteps <- averageGroupedByInterval  %>%
  arrange( desc(averageStep)) 

maxSteps[ 1,]
```

```
## Source: local data frame [1 x 2]
## 
##   interval averageStep
## 1      835    206.1698
```

## Imputing missing values

### total number of missing values in the dataset 


```r
howManyNAinSteps <- sum(!complete.cases(dataActivity[,1]))
howManyNAinSteps
```

```
## [1] 2304
```

```r
howManyNAinDate <- sum(!complete.cases(dataActivity[,2]))
howManyNAinDate
```

```
## [1] 0
```

```r
howManyNAinInterval <- sum(!complete.cases(dataActivity[,3]))
howManyNAinInterval
```

```
## [1] 0
```

Only missing values are in steps, no missing values for other features.

Devise a strategy for filling in all of the missing values in the dataset.
We use the mean for that 5-minute interval.


```r
dataActivityMissingValuesImputed <- dataActivity
dataActivityMissingValuesImputed[!complete.cases(dataActivity[,1]),1] <- averageGroupedByInterval[averageGroupedByInterval[,1] == dataActivity[!complete.cases(dataActivity[,1]),3],2]$averageStep
```

Calculate Histogram with NAs imputed


```r
totalsGroupedByDateNAImputed <- dataActivityMissingValuesImputed %>%
  group_by(date) %>%
  select(steps, date) %>%
  summarise(
    sumStep = sum(steps, na.rm = TRUE)
  ) 
breakSize <-dim(totalsGroupedByDateNAImputed)[1]
hist(totalsGroupedByDateNAImputed$sumStep,breaks=breakSize,main="Total Steps taken each day",ylab="totalSteps",xlab="date")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

### mean 

```r
mean(totalsGroupedByDateNAImputed$sumStep)
```

```
## [1] 10766.19
```

### median 


```r
median(totalsGroupedByDateNAImputed$sumStep)
```

```
## [1] 10766.19
```

### Do these values differ from the estimates from the first part of the assignment? 


```r
mean(totalsGroupedByDateNAImputed$sumStep) - mean(totalsGroupedByDate$sumStep)
```

```
## [1] 0
```

```r
median(totalsGroupedByDateNAImputed$sumStep) - median(totalsGroupedByDate$sumStep)
```

```
## [1] 1.188679
```
Mean does not change due to our strategy to replace missing values with mean.
But median changes.

### What is the impact of imputing missing data on the estimates of the total daily number of steps?

Total daily number of steps increases since we replaced NA values with mean.






## Are there differences in activity patterns between weekdays and weekends?

## What is the average daily activity pattern?

```r
dataActivityMissingValuesImputed[,4] <- as.Date(dataActivityMissingValuesImputed$date, format = "%Y-%m-%d")
names(dataActivityMissingValuesImputed)[4] <- "dateAsDate"

dataActivityMissingValuesImputed[,5] <- weekdays(dataActivityMissingValuesImputed$dateAsDate)
names(dataActivityMissingValuesImputed)[5] <- "Weekdays"

dataActivityMissingValuesImputed[,6] <- ifelse(dataActivityMissingValuesImputed$Weekdays %in% c("Saturday","Sunday"),"weekend","weekday")
names(dataActivityMissingValuesImputed)[6] <- "WeekDayOrWeekEnd"



par(mfrow=c(2,1)) 


averageGroupedByIntervalNAImputedWeekDay <- dataActivityMissingValuesImputed %>%
  group_by(interval,WeekDayOrWeekEnd) %>%
  select(steps, interval,WeekDayOrWeekEnd) %>%
  summarise(
    averageStep = mean(steps, na.rm = TRUE)
  ) %>%
  filter(WeekDayOrWeekEnd == "weekday")


plot(averageGroupedByIntervalNAImputedWeekDay$interval,averageGroupedByIntervalNAImputedWeekDay$averageStep,type="l",xlab="intervals",ylab="average steps for every day",main="Weekday ")

averageGroupedByIntervalNAImputedWeekEnd <- dataActivityMissingValuesImputed %>%
  group_by(interval,WeekDayOrWeekEnd) %>%
  select(steps, interval,WeekDayOrWeekEnd) %>%
  summarise(
    averageStep = mean(steps, na.rm = TRUE)
  ) %>%
  filter(WeekDayOrWeekEnd == "weekend")


plot(averageGroupedByIntervalNAImputedWeekEnd$interval,averageGroupedByIntervalNAImputedWeekEnd$averageStep,type="l",xlab="intervals",ylab="average steps for every day",main="Weekend ")
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png) 
