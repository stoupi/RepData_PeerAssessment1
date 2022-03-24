---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

1. Load the data (i.e. `read.csv()`): 


```r
activity <- read.csv(file = "activity.csv")
```

Show the first lines: 

```r
head(activity)
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

Show the class of variables: 

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Convert date from char to date class: 

```r
activity$date <- as.Date(activity$date, "%Y-%m-%d")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day

Use dplyr functions to group data by date and sum the steps by day:

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.0.5
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
steps_by_date <- activity %>%
        group_by(date) %>%
        summarise(steps = sum(steps))

head(steps_by_date)
```

```
## # A tibble: 6 x 2
##   date       steps
##   <date>     <int>
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

Plot an histogram of number of steps by day:


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.5
```

```r
g <- ggplot(data = steps_by_date, aes(x = date, y = steps))
g + geom_bar(stat = 'identity')
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Calculate and report the **mean** and **median** total number of steps taken per day

Mean: 

```r
mean_steps_by_date <- activity %>%
        group_by(date) %>%
        summarise(mean_steps = mean(steps, na.rm = TRUE))

steps_by_date <- mutate(steps_by_date, mean_steps = mean_steps_by_date$mean_steps)

head(steps_by_date)
```

```
## # A tibble: 6 x 3
##   date       steps mean_steps
##   <date>     <int>      <dbl>
## 1 2012-10-01    NA    NaN    
## 2 2012-10-02   126      0.438
## 3 2012-10-03 11352     39.4  
## 4 2012-10-04 12116     42.1  
## 5 2012-10-05 13294     46.2  
## 6 2012-10-06 15420     53.5
```

Median: 

```r
median_steps_by_date <- activity %>%
        group_by(date) %>%
        summarise(median_steps = median(steps, na.rm = TRUE))

steps_by_date <- mutate(steps_by_date, median_steps = median_steps_by_date$median_steps)

head(steps_by_date)
```

```
## # A tibble: 6 x 4
##   date       steps mean_steps median_steps
##   <date>     <int>      <dbl>        <dbl>
## 1 2012-10-01    NA    NaN               NA
## 2 2012-10-02   126      0.438            0
## 3 2012-10-03 11352     39.4              0
## 4 2012-10-04 12116     42.1              0
## 5 2012-10-05 13294     46.2              0
## 6 2012-10-06 15420     53.5              0
```

```r
mean(steps_by_date$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(steps_by_date$steps, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps_by_interval <- activity %>%
        group_by(interval) %>%
        summarise(mean_steps = mean(steps, na.rm = TRUE))

qplot(x = interval, y = mean_steps, data = steps_by_interval, geom = "line", xlab = "5-min intervals", ylab = "Steps averaged across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
temp <- activity %>%
        group_by(interval) %>%
        summarise(max_steps = max(steps, na.rm = TRUE))

steps_by_interval <- steps_by_interval %>%
        mutate(temp)

qplot(x = interval, y = max_steps, data = temp, geom = "line", xlab = "5-min intervals", ylab = "Maximum steps across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

## Imputing missing values  

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)



```r
sapply(activity, function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##     2304        0        0
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Duplicate steps variable to "imputed_steps" variable


```r
activity <- activity %>%
        mutate (steps_imputed = steps)

summary(activity)
```

```
##      steps             date               interval      steps_imputed   
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Min.   :  0.00  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   1st Qu.:  0.00  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Median :  0.00  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   Mean   : 37.38  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2   3rd Qu.: 12.00  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   Max.   :806.00  
##  NA's   :2304                                           NA's   :2304
```
Replace NA values by mean values of the 5-min interval :

```r
activity <- merge(activity, steps_by_interval, by.x = "interval", by.y = "interval", all = FALSE)

activity$steps_imputed[is.na(activity$steps)] <- activity$mean_steps[is.na(activity$steps)]
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_imputed <- activity %>%
        select("interval", "steps_imputed", "date")

names(activity_imputed) <- c("interval", "steps", "date")

summary(activity_imputed)
```

```
##     interval          steps             date           
##  Min.   :   0.0   Min.   :  0.00   Min.   :2012-10-01  
##  1st Qu.: 588.8   1st Qu.:  0.00   1st Qu.:2012-10-16  
##  Median :1177.5   Median :  0.00   Median :2012-10-31  
##  Mean   :1177.5   Mean   : 37.38   Mean   :2012-10-31  
##  3rd Qu.:1766.2   3rd Qu.: 27.00   3rd Qu.:2012-11-15  
##  Max.   :2355.0   Max.   :806.00   Max.   :2012-11-30
```


```r
summary(activity)
```

```
##     interval          steps             date            steps_imputed   
##  Min.   :   0.0   Min.   :  0.00   Min.   :2012-10-01   Min.   :  0.00  
##  1st Qu.: 588.8   1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.:  0.00  
##  Median :1177.5   Median :  0.00   Median :2012-10-31   Median :  0.00  
##  Mean   :1177.5   Mean   : 37.38   Mean   :2012-10-31   Mean   : 37.38  
##  3rd Qu.:1766.2   3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.: 27.00  
##  Max.   :2355.0   Max.   :806.00   Max.   :2012-11-30   Max.   :806.00  
##                   NA's   :2304                                          
##    mean_steps        max_steps     
##  Min.   :  0.000   Min.   :  0.00  
##  1st Qu.:  2.486   1st Qu.: 61.25  
##  Median : 34.113   Median :489.00  
##  Mean   : 37.383   Mean   :393.31  
##  3rd Qu.: 52.835   3rd Qu.:597.75  
##  Max.   :206.170   Max.   :806.00  
## 
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


Histogram of steps per date with imputation:

```r
steps_by_date_imputed <- activity_imputed %>%
        group_by(date) %>%
        summarise(steps = sum(steps))

g <- ggplot(data = steps_by_date_imputed, aes(x = date, y = steps))
g + geom_bar(stat = 'identity')
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->


```r
mean(steps_by_date_imputed$steps)
```

```
## [1] 10766.19
```

```r
median(steps_by_date_imputed$steps)
```

```
## [1] 10766.19
```
The **mean** value is the **same** as the value before imputing missing data because we put the mean value for that particular 5-min interval. The median value shows **a little** difference.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
weekdays_names <- c('lundi', 'mardi', 'mercredi', 'jeudi', 'vendredi')

activity_imputed <- activity_imputed %>%
  mutate(day = weekdays(date)) %>%
  mutate(factor = factor((weekdays(date) %in% weekdays_names), 
         levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))) 
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged
across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
steps_by_interval_imputed <- activity_imputed %>%
        group_by(interval, factor) %>%
        summarise(steps = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

```r
ggplot(steps_by_interval_imputed, aes(x = interval, y = steps)) + geom_line() + facet_grid(factor ~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-20-1.png)<!-- -->







