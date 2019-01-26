
### load the data

load the data through `read.csv`

```r
data <- read.csv("activity.csv", stringsAsFactors = FALSE)
data$date <- as.Date(data$date, "%Y-%m-%d")
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

### What is mean total number of steps taken per day?

use tidyverse to summarise and visualization the data

#### Calculate the total number of steps taken per day

```r
library(tidyverse)
```

```
## -- Attaching packages ------------------------------------- tidyverse 1.2.1 --
```

```
## √ ggplot2 3.1.0     √ purrr   0.2.5
## √ tibble  1.4.2     √ dplyr   0.7.8
## √ tidyr   0.8.2     √ stringr 1.3.1
## √ readr   1.3.1     √ forcats 0.3.0
```

```
## -- Conflicts ---------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
(totalCount <- data %>% group_by(date) %>% summarise(totalCount = sum(steps, na.rm = T)))
```

```
## # A tibble: 61 x 2
##    date       totalCount
##    <date>          <int>
##  1 2012-10-01          0
##  2 2012-10-02        126
##  3 2012-10-03      11352
##  4 2012-10-04      12116
##  5 2012-10-05      13294
##  6 2012-10-06      15420
##  7 2012-10-07      11015
##  8 2012-10-08          0
##  9 2012-10-09      12811
## 10 2012-10-10       9900
## # ... with 51 more rows
```

```r
ggplot(totalCount, aes(x =totalCount)) + geom_histogram(color = "black", fill = "white") + theme_bw()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="PA1_template_files/figure-html/unnamed-chunk-2-1.png" width="672" />

#### Calculate and report the mean and median of the total number of steps taken per day

```r
mean(totalCount$totalCount)
```

```
## [1] 9354.23
```

```r
median(totalCount$totalCount)
```

```
## [1] 10395
```

### What is the average daily activity pattern?

#### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
(dataMeanPerInterval <- data %>% group_by(interval) %>% summarise(mean = mean(steps, na.rm = T)))
```

```
## # A tibble: 288 x 2
##    interval   mean
##       <int>  <dbl>
##  1        0 1.72  
##  2        5 0.340 
##  3       10 0.132 
##  4       15 0.151 
##  5       20 0.0755
##  6       25 2.09  
##  7       30 0.528 
##  8       35 0.868 
##  9       40 0     
## 10       45 1.47  
## # ... with 278 more rows
```

```r
plot(dataMeanPerInterval$interval, dataMeanPerInterval$mean, type = "l")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-4-1.png" width="672" />

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
dataMeanPerInterval$interval[which.max(dataMeanPerInterval$mean)]
```

```
## [1] 835
```

### Imputing missing values

#### Calculate and report the total number of missing values in the dataset 


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

#### filling in all of the missing values in the dataset.
using the mean for that 5-minute interval to fill the dataset


```r
newdata <- data
for(i in dataMeanPerInterval$interval){
  newdata$steps[is.na(newdata$steps) & newdata$interval == i] <- dataMeanPerInterval$mean[dataMeanPerInterval$interval == i]
}
```
#### Make a histogram of the total number of steps taken each day


```r
(totalCount_new <- newdata %>% group_by(date) %>% summarise(totalCount = sum(steps, na.rm = T)))
```

```
## # A tibble: 61 x 2
##    date       totalCount
##    <date>          <dbl>
##  1 2012-10-01     10766.
##  2 2012-10-02       126 
##  3 2012-10-03     11352 
##  4 2012-10-04     12116 
##  5 2012-10-05     13294 
##  6 2012-10-06     15420 
##  7 2012-10-07     11015 
##  8 2012-10-08     10766.
##  9 2012-10-09     12811 
## 10 2012-10-10      9900 
## # ... with 51 more rows
```

```r
ggplot(totalCount_new, aes(x =totalCount)) + geom_histogram(color = "black", fill = "white") + theme_bw()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="PA1_template_files/figure-html/unnamed-chunk-8-1.png" width="672" />

```r
mean(totalCount_new$totalCount)
```

```
## [1] 10766.19
```

```r
median(totalCount_new$totalCount)
```

```
## [1] 10766.19
```

these values differ from the estimates from the first part of the assignment. 

### Are there differences in activity patterns between weekdays and weekends?


```r
newdata$week <- ifelse(weekdays(newdata$date) %in% c("Saturday", "Sunday"), "weekends", "weekdays")
(dataMeanIntervWeekday  <- newdata %>% group_by(interval, week) %>% summarise(mean = mean(steps, na.rm = T)))
```

```
## # A tibble: 288 x 3
## # Groups:   interval [?]
##    interval week       mean
##       <int> <chr>     <dbl>
##  1        0 weekdays 1.72  
##  2        5 weekdays 0.340 
##  3       10 weekdays 0.132 
##  4       15 weekdays 0.151 
##  5       20 weekdays 0.0755
##  6       25 weekdays 2.09  
##  7       30 weekdays 0.528 
##  8       35 weekdays 0.868 
##  9       40 weekdays 0     
## 10       45 weekdays 1.47  
## # ... with 278 more rows
```

```r
ggplot(dataMeanIntervWeekday, aes(x = interval, y = mean)) + geom_line() + facet_grid(week ~ .)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-9-1.png" width="672" />

