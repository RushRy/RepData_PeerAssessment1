# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

First we need to make sure we reference all required libraries

```r
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
```

Now we need to load the data and inspect it.

```r
activity <- read.csv("activity.csv")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

The 'date' variable was read in as a character Factor and should be a Date datatype.  Also, the 'interval' variable should probably be a Factor.

```r
activity$date <- as.POSIXct(activity$date)
activity$interval <- as.factor(activity$interval)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```


##Steps per Day Analysis

Our first analysis is around the total # of steps per day. Let's create a new dataset to hold these values.

```r
dailySteps <- activity %>%
    filter(!is.na(steps)) %>%
    group_by(date) %>%
    summarize(steps=sum(steps))
str(dailySteps)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	53 obs. of  2 variables:
##  $ date : POSIXct, format: "2012-10-02" "2012-10-03" ...
##  $ steps: int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
```

Let's see a histogram of the # of steps per day

```r
ggplot(dailySteps, aes(x=steps)) +
    geom_histogram(binwidth=1000, color="black") +
    labs(title="Steps per Day", 
         x="# of Steps", 
         y="# of Days"
         )
```

![](ActivityMonitoringAnalysis_files/figure-html/dailystepshisto-1.png)<!-- -->

Let's look at the mean and median # of steps:

* Mean # of steps: 1.0766189\times 10^{4}
* Median # of steps: 10765

