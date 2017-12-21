---
title: "Reproducible Data assignment 1"
output: 
  html_document: 
    keep_md: yes
keep_md: true
---



## Loading and Processing Data


```r
Act_Data <- read.csv("activity.csv")
Act_Data$date <- as.Date(as.character(Act_Data$date))
library(knitr)
```

## What is mean total number of steps taken per day?


```r
SPD <- aggregate(steps ~ date,data = Act_Data, FUN=sum, na.rm=TRUE)
hist(SPD$steps, xlab = "Steps", main = "Total steps per day", col= "red")
```

![](Reproducible_Data_PA_1_files/figure-html/Histogram of total steps per day-1.png)<!-- -->

```r
mean_steps <- mean(SPD$steps)
median_steps <- median(SPD$steps)

mean_steps
```

```
## [1] 10766.19
```

```r
median_steps
```

```
## [1] 10765
```

## What is the daily activity pattern


```r
SPD_Mean <- aggregate(steps ~ interval,data = Act_Data, FUN=sum, na.rm=TRUE)
plot(SPD_Mean$interval, SPD_Mean$steps, type ="l", col = "blue", xlab= "Intervals", ylab= "Total Steps per Interval", main = "Number of Steps per Interval")
```

![](Reproducible_Data_PA_1_files/figure-html/Number of Steps per Interval-1.png)<!-- -->


```r
SPD_Max <- max(SPD_Mean$steps)
SPD_Highest <- SPD_Mean$interval[which(SPD_Mean$steps == SPD_Max)] 

SPD_Highest
```

```
## [1] 835
```

## Inputing Missing Values


```r
SPD_Missing <- sum(is.na(Act_Data))

SPD_Missing
```

```
## [1] 2304
```


```r
SPD_GetMean <- function(interval){
  SPD_Mean[SPD_Mean$interval == interval, ]$steps
}
```





```r
SPD_New <- Act_Data
for (i in 1:nrow(SPD_New)){
  if(is.na(SPD_New[i,]$steps)){
    SPD_New[i,]$steps <- SPD_GetMean(SPD_New[i,]$interval)
  }
}
```


```r
SPD_Total <- aggregate(steps ~ date,data = Act_Data, sum)
hist(SPD_Total$steps,xlab = "Steps", main = "New Total Steps", col= "green")
```

![](Reproducible_Data_PA_1_files/figure-html/Make a histogramof the total number of steps per day-1.png)<!-- -->

```r
SPD_Total_Mean <- mean(SPD_Total$steps)
SPD_Total_Meadian <- median(SPD_Total$steps)

SPD_Total_Mean
```

```
## [1] 10766.19
```

```r
SPD_Total_Meadian
```

```
## [1] 10765
```


## Are there any differences in activity patterns between weekdays and weekends?


```r
SPD_New$date <- as.Date(strptime(SPD_New$date, format="%Y-%m-%d"))
SPD_New$day <- weekdays(SPD_New$date)
  for (i in 1:nrow(SPD_New)) {
    if (SPD_New[i,]$day %in% c("Saturday", "Sunday")) {
      SPD_New[i,]$day <- "weekend"
    }
    else{
      SPD_New[i,]$day <- "weekday"
    }
  }

SPD_WW <- aggregate(SPD_New$steps ~ SPD_New$interval + SPD_New$day, SPD_New, mean)
```



```r
names(SPD_WW) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, SPD_WW, type = "l", layout= c(1,2), xlab= "Interval", ylab= "Number of Steps")
```

![](Reproducible_Data_PA_1_files/figure-html/Make a panal line plot of the 5-minute interval and average steps across all weekdays or weekends-1.png)<!-- -->
       





