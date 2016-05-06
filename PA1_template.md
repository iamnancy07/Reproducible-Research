# PA1_template
Kexin  
May 4, 2016  

##Loading and preprocessing the data


```r
#unzip download zip file
#unzip("./repdata-data-activity.zip", "./activity.csv")
#read csv file
activity <- read.csv("C:/Users/kexinxu/Documents/activity.csv")
```

##What is mean total number of steps taken per day?


```r
library(dplyr)
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
#sum total steps per day
activity %>% group_by(date) %>% summarise(sum = sum(steps, na.rm = TRUE))
```

```
## Source: local data frame [61 x 2]
## 
##          date   sum
##        (fctr) (int)
## 1  2012-10-01     0
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08     0
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## ..        ...   ...
```

```r
steps_by_day <- activity %>% group_by(date) %>% summarise(sum = sum(steps, na.rm = TRUE))
#plot histogram of total steps per day
hist(steps_by_day$sum, main = "Histogram of Total Number of Steps Each Day", xlab = "Total Steps Per Day")
```

![](PA1_template_files/figure-html/sum, histogram, mean, median-1.png) 

```r
#Calculate and report the mean and median of the total number of steps taken per day
steps_by_day %>% summarise(mean = mean(sum, na.rm = TRUE), median = median(sum, na.rm=TRUE))
```

```
## Source: local data frame [1 x 2]
## 
##      mean median
##     (dbl)  (int)
## 1 9354.23  10395
```

##What is the average daily activity pattern?


```r
#calculate average number of steps taken per interval, averaged across all days
steps_by_interval <- activity %>% group_by(interval) %>% summarise(average = mean(steps, na.rm = TRUE))

#time series plot
plot(steps_by_interval$interval, steps_by_interval$average, type = "l", main = "Average Number of Steps per Interval Across All Days", xlab = "Interval", ylab = "Average Steps per Interval")
```

![](PA1_template_files/figure-html/time series plot, max, mean-1.png) 

```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

steps_by_interval %>% mutate(max = max(average, na.rm = TRUE)) %>% filter(average == max)
```

```
## Source: local data frame [1 x 3]
## 
##   interval  average      max
##      (int)    (dbl)    (dbl)
## 1      835 206.1698 206.1698
```

##Imputing missing values
After filling NAs with average steps per interval, the mean and median of total number of steps per day increased. The total daily number of steps per day increased as well

```r
#Calculate and report the total number of missing values in the datase
sum(!complete.cases(activity)) 
```

```
## [1] 2304
```

```r
#filling in all of the missing values with average steps per interval
na_average <- merge(activity, steps_by_interval, by.x = "interval", by.y = "interval")
na_average$steps <- ifelse(is.na(na_average$steps), floor(na_average$average), na_average$steps)

#create a new dataset with missing values filled in
fill_na <- na_average[,1:3]

fill_na %>% group_by(date) %>% summarise(sum = sum(steps))
```

```
## Source: local data frame [61 x 2]
## 
##          date   sum
##        (fctr) (dbl)
## 1  2012-10-01 10641
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08 10641
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## ..        ...   ...
```

```r
steps_by_day1 <- fill_na %>% group_by(date) %>% summarise(sum = sum(steps))
#plot histogram of total steps per day
hist(steps_by_day1$sum, main = "Histogram of Total Number of Steps Each Day With NA Filled-in", xlab = "Total Steps Per Day")
```

![](PA1_template_files/figure-html/count missing values, merge, fill in NAs-1.png) 

```r
#Calculate and report the mean and median of the total number of steps taken per day
steps_by_day1 %>% summarise(mean = mean(sum), median = median(sum))
```

```
## Source: local data frame [1 x 2]
## 
##       mean median
##      (dbl)  (dbl)
## 1 10749.77  10641
```

##Are there differences in activity patterns between weekdays and weekends?


```r
#create weekday/weekend based on date
weekday <- fill_na %>% mutate(ind = ifelse(weekdays(as.POSIXlt(date, format ="%Y-%m-%d")) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


steps_by_interval_weekday <- weekday %>% group_by(ind, interval) %>% summarise(average = mean(steps))

library(lattice)

xyplot(average~interval |factor(ind), data = steps_by_interval_weekday, layout = c(1,2), main = "Weekday vs Weekend", type = "l", ylab = "Average Steps per Interval", xlab = "Interval")
```

![](PA1_template_files/figure-html/convert date, create new variable-1.png) 
