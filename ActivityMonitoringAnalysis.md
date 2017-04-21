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

The 'date' variable was read in as a character Factor and should be a Date datatype.

```r
activity$date <- as.POSIXct(activity$date)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
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

* Mean # of steps: 10,766.19
* Median # of steps: 10,765

## Average Daily Activity Pattern Analysis

We want to look at what the average daily activity pattern looks like.  First, lets get a dataset which shows the total and average # of steps for each 5-minute interval

```r
intervalSteps <- activity %>%
    filter(!is.na(steps)) %>%
    group_by(interval) %>%
    summarize(totSteps=sum(steps), avgSteps=mean(steps))
str(intervalSteps)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	288 obs. of  3 variables:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ totSteps: int  91 18 7 8 4 111 28 46 0 78 ...
##  $ avgSteps: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```




Let's chart a time-series graph to see how the daily pattern.

```r
ggplot(intervalSteps, aes(x=interval,y=avgSteps,group=1)) +
    geom_line(color="blue") +
    geom_vline(xintercept = highestPeriod, color="green") +
    labs(title="Average Steps by Interval",
         xlab="5-min Interval",
         ylab="Avg # of Steps"
         )
```

![](ActivityMonitoringAnalysis_files/figure-html/avgpatternchart-1.png)<!-- -->

The 5-minute period which has the maximum average # of steps across all dates is period **835** marked by the green line.
