---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Use the readr package to read the zip file directory and make a copy which contains only rows without
missing values


```r
library(readr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
df<-read_csv('activity.zip')
```

```
## Parsed with column specification:
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

```r
dfnoNA<-df[!is.na(df$steps),]
```


## What is mean total number of steps taken per day?
Take the non-NA rows, group them by date, then output the sum per interval
Create a histogram of the number of steps, and then output the mean and median steps per day.


```r
s<-dfnoNA %>% group_by(date) %>% select(c(steps,interval)) %>% summarise(steps=sum(steps)) 
```

```
## Adding missing grouping variables: `date`
```

```r
hist(s$steps, col='blue', main='Histogram of Number of Steps per Day', xlab = 'Total Steps per Day')
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
sprintf('Mean number of steps per day: %f',mean(s$steps))
```

```
## [1] "Mean number of steps per day: 10766.188679"
```

```r
sprintf('Median number of steps per day: %f',median(s$steps))
```

```
## [1] "Median number of steps per day: 10765.000000"
```


## What is the average daily activity pattern?
Group the non-NA frame by interval, and then get the mean steps per interval.
Use the base plotting system to plot the result, and mark the interval with the maximum with an abline.
Finally, output the interval at which the maximum number of steps are taken on average.


```r
s2<-dfnoNA %>% group_by(interval) %>% select(c(steps,interval)) %>% summarise(steps=mean(steps))
with(s2, plot(x=interval,y=steps, type='l', xlab='Daily 5 Minute Interval'
             ,ylab='Average Number of Steps', main='Average Steps per Daily 5-Minute Interval'))
abline(v=s2$interval[which.max(s2$steps)], col='blue', lty = 3)
text(850,200, paste('Max @ interval',as.character(s2$interval[which.max(s2$steps)])), col = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
sprintf('Maximum steps were taken on average at interval %i',s2$interval[which.max(s2$steps)])
```

```
## [1] "Maximum steps were taken on average at interval 835"
```


## Imputing missing values
Output the number of NA rows from the original set.
Take the non-NA frame, group it by interval, and then get the mean by interval. Merge that with the 
columns from the original data where steps were NA, such that the NA is replaced by the mean for that interval.
Recombine the two frames and order them (just to be neat)

Take the resulting data frame with the imputed values, and plot a histogram of the steps per interval.

Finally, output the median and mean of the imputed values, and show the delta between them and the previous set where the NAs were simply removed.


```r
sprintf('Rows with NA: %i',nrow(df[which(is.na(df$steps)),]))
```

```
## [1] "Rows with NA: 2304"
```

```r
s4<-dfnoNA %>% group_by(interval) %>% select(c(steps,interval)) %>% summarise(steps=mean(steps)) 
impdf<-rbind(dfnoNA,merge(s4,df[is.na(df$steps),c(2,3)], by ='interval')) 
impdf<-impdf[order(impdf$date),]

s3<-impdf %>% group_by(date) %>% summarise(steps=sum(steps)) 
hist(s3$steps, col='blue', main='Histogram of Number of Steps per Day', xlab = 'Total Steps per Day')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
sprintf('Imputed Mean number of steps per day: %f',mean(s3$steps))
```

```
## [1] "Imputed Mean number of steps per day: 10766.188679"
```

```r
sprintf('Imputed Median number of steps per day: %f',median(s3$steps))
```

```
## [1] "Imputed Median number of steps per day: 10766.188679"
```

```r
sprintf('Difference Mean number of steps per day: %f',(mean(s3$steps)-mean(s$steps)))
```

```
## [1] "Difference Mean number of steps per day: 0.000000"
```

```r
sprintf('Difference Median number of steps per day: %f',(median(s3$steps)-median(s$steps)))
```

```
## [1] "Difference Median number of steps per day: 1.188679"
```


## Are there differences in activity patterns between weekdays and weekends?
Create a factor variable of two factors - weekday and weekend.
Make a function that returns whichever factor is appropriate based on the date using the weekend() function.
Then group the resulting data frame by day type and interval, and compute the average for each interval per day type.

Use ggplot to plot the two graphs in a single column to show how they differ.


```r
# using the imputed values
days<-as.factor(c('weekday','weekend'))
typeday<-function(x){a<-weekdays(x) %in% c('Saturday','Sunday');return(days[as.integer(a)+1])}
impdf$day<-typeday(impdf$date)
s5<-impdf %>% group_by(day) %>% group_by(interval, add=TRUE) %>% select(c('steps', 'day','interval')) %>% summarise(steps=mean(steps))

p<-ggplot(s5, aes(interval, steps)) + facet_grid(rows=vars(day)) + geom_line(col='blue') + ylab('Average Number of Steps') + ggtitle('Average Steps per Interval Weekdays vs. Weekends')
print(p)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


