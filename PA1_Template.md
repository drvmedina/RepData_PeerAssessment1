
Reproducible Research, Coursera. Peer Assessment 1.
====================================================

Víctor M. Medina

Nov. 14 2015

## Introduction
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data

The data used for this assignment was downloaded from the course web site:
Activity monitoring data.

## Loading and preprocessing data
Open the data set assuming data are already downloaded to your working directory with the name "activicty.csv".

In order to be able to process and transform data, and create some of the plots showed here, open ggplot2 and dplyr libraries.

```{r}
library(dplyr)
library(ggplot2)
```

Open the data set assuming data are already downloaded to your working directory with the name "activity.csv" and convert it to a tbl class object.

```{r}
datos <- read.csv("activity.csv")
datbl <- tbl_df(datos)
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day. 

Create an object "datact" by using group_by function and use summarise function to aggregate steps by day. Create object "datsum" to select and sum the desired columns.

```{r}
datact <- datbl %>% group_by(date) %>% summarise(total.steps = sum(steps))
datsum<- tapply(datbl$steps, datbl$date, sum, na.rm=TRUE)
```


2. Make a histogram of the total number of steps taken each day.
Calculate and report the mean and median of the total number of steps taken per day.

Use "hist" function to create the requested plot. With breaks set to 30.

```{r}
hist(datsum, breaks=30, xlab="Total Steps", 
     main="Total Steps Per Day / Frequency", col="purple")
```

3. Calculate and report the mean and median of the total number of steps taken per day.

```{r}
mean((datact$total.steps), na.rm=TRUE)
median((datact$total.steps), na.rm=TRUE)
```

The mean of the total number of steps taken per day is 10766.19

The median of total number of steps taken per day is 10765


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

First aggregate the total steps per interval and obtain the mean using "mean" function. Then plot the results with ggplot2 using geom_l to obtain a linear plot, add smooth to better understand the pattern by interval.

```{r}
steps.inter <- aggregate(steps ~ interval, data = datos, FUN = mean)

ggplot(steps.inter, aes(x=interval, y=steps)) +   
  labs(title="ACTIVITY PATTERN", x="INTERVAL", y="STEPS PER DAY") +
  geom_line(color="red") +
  geom_smooth(method = "lm", 
              col= "green", size = 1) +
  theme_bw(base_family = "Avenir")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Obtain the interval with the highest mean of steps across the days.

```{r}
steps.inter$interval[which.max(steps.inter$steps)]
```

The interval with the highest mean of steps across the days is interval 835.

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
sum(is.na(datos$steps))
```

The total number of NA values is 2304.

2. Devise a strategy for filling in all of the missing values in the dataset.

Use the median number of steps to fill the missing values in data set, creating a function. (Here i obtained help from https// rpubs.com/c0d3_k1ra/RRPA1 to create the relNA function with data and pervalue arguments)
```{r}
relNA <- function(data, pervalue) {
  cualNA <- which(is.na(data$steps))
  corrNA <- unlist(lapply(cualNA, FUN=function(idx){
    interval = data[idx,]$interval
    pervalue[pervalue$interval == interval,]$steps
  }))
  pasosnuev <- data$steps
  pasosnuev[cualNA] <- corrNA
  pasosnuev}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

Create a new data set from the relNA function.

```{r}
NAllenos <- data.frame(  
  steps = relNA(datos, steps.inter),  
  date = datos$date,  
  interval = datos$interval)
```

A quick look of the new data set.
```{r}
head(NAllenos)
```


4. Make a histogram of the total number of steps taken each day and...

First, use tapply to obtain the sum of each column by interval from the new data set, then use "hist" function to build the requested histogram.

```{r}
datsum<- tapply(NAllenos$steps, NAllenos$date, sum, na.rm=TRUE)

hist(datsum, breaks=30, xlab="Total Steps", 
     main="Total Steps Per Day / Frequency no NA", col="green")
```

...calculate and report the mean and median total number of steps taken per day. 

Here, to be able to obtain the mean and median values, convert "datsum" data set to data frame and again to tbl_df object, then re-calculate the sum of steps per interval.

```{r}
datsum_1 <- as.data.frame(datsum)
datsum2 <- tbl_df(datsum_1)
datsum3<- tapply(NAllenos$steps, NAllenos$date, sum, na.rm=TRUE)
mean(datsum3)
median(datsum3)
```

4.1. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

The mean and median values has no significant difference from those obtained in the first part, with no impact in the results.

## Are there differences in activity patterns between weekdays and weekends? 

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```{r}
NAllenos$date <- as.Date(NAllenos$date, "%Y-%m-%d")
NAllenos$day <- weekdays(NAllenos$date)
NAllenos$wdwe <- c("weekday")
for (i in 1:nrow(NAllenos)){
  if (NAllenos$day[i] == "Saturday" || NAllenos$day[i] == "Sunday"){
    NAllenos$wdwe[i] <- "weekend"
  }
}
NAllenos$wdwe <- as.factor(NAllenos$wdwe)
wd_we <- aggregate(steps ~ interval+wdwe, NAllenos, mean)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```{r}
obj <- ggplot(wd_we, aes(interval, steps))
obj + geom_line(color = "navyblue") +
  geom_smooth(method = "lm", 
              col= "green", size = 1) +
  facet_grid(. ~ wdwe) +
  labs (x = "STEPS(MEAN)") +
  labs (y = "INTERVAL") +
  labs(title = "WEEKDAY / WEEKEND PATTERN") +
  theme_bw(base_family = "Avenir") 
```

There is a clear difference on pattern between weekdays and weekends. Although weekdays shows more activity on mornings, weekends shows more activity with a constant pattern through the day.
