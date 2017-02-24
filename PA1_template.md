Reproducible Research - Project I
================

Description
-----------

### This is a R markdown document generated for the purpose of completing the Reproducible Research Course Project 1. The following analyses and visualizations are performed using the *ggplot2*, *dplyr*, and *scales* packages.

### This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### The following code loads the data to a csv in the indicated directory, converts date column to date datatype, and converts the data set to a data frame.

``` r
activity <- read.csv("./data/activity.csv", sep=",", header=TRUE)
dim(activity)
names(activity)

activity$date <- as.Date(activity$date, "%Y-%m-%d")
activity <- as.data.frame(activity)
head(activity); tail(activity)
```

What is the mean total number of steps taken per day?
-----------------------------------------------------

### The following code calculates the total number of steps taken each day.

``` r
suppressPackageStartupMessages(library(dplyr))
Total.Steps <- as.data.frame(summarize(group_by(activity, date), 
                                             Steps = sum(steps, na.rm=TRUE)))
dim(Total.Steps)
head(Total.Steps)
```

### **Histogram of the total number of steps taken each day**

![](PA1_template_files/figure-markdown_github/plot1-1.png)

### The following calculates the mean and median number of steps taken each day.

``` r
mean(Total.Steps$Steps)
```

    ## [1] 9354.23

``` r
median(Total.Steps$Steps)
```

    ## [1] 10395

What is the average daily activity pattern?
-------------------------------------------

### The following code calculates the means for each interval across days.

``` r
library(dplyr)
daymeans <- as.data.frame(summarize(group_by(na.omit(activity), interval), 
                                    Average.steps = mean(steps, na.rm=TRUE)))
head(daymeans)
```

    ##   interval Average.steps
    ## 1        0     1.7169811
    ## 2        5     0.3396226
    ## 3       10     0.1320755
    ## 4       15     0.1509434
    ## 5       20     0.0754717
    ## 6       25     2.0943396

### **Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

![](PA1_template_files/figure-markdown_github/plot2-1.png)

### The following code determines the maximum value for the mean steps across intervals. The 835th interval contains the the maximum number of steps.

``` r
daymeans[which(daymeans$Average.steps == max(daymeans$Average.steps)),]
```

    ##     interval Average.steps
    ## 104      835      206.1698

**Imputing missing values**
---------------------------

### The following code calculates and reports the total number of missing values (NA values) in the dataset. It also reports the ratio of (NA/Total observations).

``` r
suppressPackageStartupMessages(library(scales)) 
sum(is.na(activity))
```

    ## [1] 2304

``` r
library(scales) 
percent(sum(is.na(activity))/nrow(activity))
```

    ## [1] "13.1%"

### **Strategy for imputing the missing values in the dataset**

### The following shows the NA values at both ends of the dataset.

``` r
head(activity)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

``` r
tail(activity)
```

    ##       steps       date interval
    ## 17563    NA 2012-11-30     2330
    ## 17564    NA 2012-11-30     2335
    ## 17565    NA 2012-11-30     2340
    ## 17566    NA 2012-11-30     2345
    ## 17567    NA 2012-11-30     2350
    ## 17568    NA 2012-11-30     2355

### The strategy is to replace each NA value by the corresponding interval mean.

### For each NA value therefore, the respective interval of the row is first identified and then that NA value is replaced with the corresponding mean for that interval.

``` r
daymeans <- with(na.omit(activity), tapply(steps, interval, mean))
int <- unique(activity$interval)
len <- nrow(activity[is.na(activity),])
```

### The int and len variables are set up to manage the for loop sequences.

### The NAint and NAsteps variables are data segments that will be used to replace the NA value after the loop ends.

### As calculated earlier, the total number of NA values in the dataset is 2304. The total number of 5-minute intervals per day is 288.

``` r
NAint <-  activity[is.na(activity),3]
NAsteps <- activity[is.na(activity),1]
for (j in 1:2304) {
  for (i in 1:288){
    if (NAint[j] == int[i]) 
       NAsteps[j] <- daymeans[i]
    }
}
NAindex <- is.na(activity$steps)
activity$steps<- replace(activity$steps, NAindex, NAsteps)
```

### The following code demonstrates that the NA values have been successfully replaced by the respective steps mean for that 5-minute interval.

``` r
head(activity)
```

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

``` r
tail(activity)
```

    ##           steps       date interval
    ## 17563 2.6037736 2012-11-30     2330
    ## 17564 4.6981132 2012-11-30     2335
    ## 17565 3.3018868 2012-11-30     2340
    ## 17566 0.6415094 2012-11-30     2345
    ## 17567 0.2264151 2012-11-30     2350
    ## 17568 1.0754717 2012-11-30     2355

### In the new updated dataset, the following code calculates and then plots the total number of steps taken each day.

``` r
library(dplyr) 
Total.Steps_new <- as.data.frame(summarize(group_by(activity, date), 
                                       Steps = sum(steps, na.rm=TRUE)))
dim(Total.Steps_new)
head(Total.Steps_new)
```

### **Histogram of the total number of steps taken each day**

![](PA1_template_files/figure-markdown_github/plot3-1.png)

### The following calculates the mean and median number of steps taken each day.

``` r
mean(Total.Steps_new$Steps)
```

    ## [1] 10766.19

``` r
median(Total.Steps_new$Steps)
```

    ## [1] 10766.19

### **Do these values differ from the estimates from the first part of the assignment?**

### Yes, the values differ because the missing values have been filled in in the updated dataset.

### **What is the impact of imputing missing data on the estimates of the total daily number of steps?**

### There is a significant impact. Imputing the missing data with the corresponding interval means has changed the mean and median values of the new activity dataset. Furthermore, as should be the case, the mean and the median values are now identical.

**Are there differences in activity patterns between weekdays and weekends?**
-----------------------------------------------------------------------------

### In order to determine the differences in activity patterns, first of all,

### a new factor variable is created in the dataset, which indicates whether a given date is a weekday or a weekend day

``` r
library(dplyr)
activity <- mutate(activity, day = weekdays(activity$date))
head(activity)
```

    ##       steps       date interval    day
    ## 1 1.7169811 2012-10-01        0 Monday
    ## 2 0.3396226 2012-10-01        5 Monday
    ## 3 0.1320755 2012-10-01       10 Monday
    ## 4 0.1509434 2012-10-01       15 Monday
    ## 5 0.0754717 2012-10-01       20 Monday
    ## 6 2.0943396 2012-10-01       25 Monday

``` r
weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activity$day <- factor((weekdays(activity$date) %in% weekdays),
                       levels=c(FALSE, TRUE), labels=c('Weekend', 'Weekday'))
table(activity$day)
```

    ## 
    ## Weekend Weekday 
    ##    4608   12960

### The following code calculates the means for each interval across days

``` r
library(dplyr)
daymeans.new <- as.data.frame(summarize(group_by(na.omit(activity), interval, day), 
                                    Average.steps = mean(steps, na.rm=TRUE)))
head(daymeans.new)
```

### **Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekdays and weekend days (y-axis)**

![](PA1_template_files/figure-markdown_github/plot4-1.png)

### Yes, there are differences in activity patterns between weekdays and weekends.
