
---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data


```r
unzip("activity.zip", exdir=".")
data <- read.csv("activity.csv", sep = ",", na.strings = "NA", stringsAsFactors = F)
data$day <- ymd(data$date)
```
## What is mean total number of steps taken per day?

```r
total <- data %>% group_by(day) %>% summarize(daysteps=sum(steps, na.rm = T)) %>% ungroup()
ggplot(total)+
geom_histogram(aes(x=day, y=daysteps), stat='identity')+
labs(x="Day",
     y="Steps",
     title="Total Numbers of Steps Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
summary(total$daysteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

```r
mean <- round(mean(total$daysteps), digits = 0)
```

#### *The mean total number of steps taken per day is **9354**.* ####



## What is the average daily activity pattern?

```r
mean <- data %>% group_by(interval) %>% summarize(stepsMean=mean(steps, na.rm=T)) %>% ungroup()

ggplot(data=mean, aes(x=interval, y=stepsMean))+
geom_point(shape=1,colour="red")+
geom_line(linetype=1)+
labs(x="Interval (Minute)",
     y="Average Steps Across Days",
     title="Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
maxstep <- max(mean$stepsMean)
maxstep1 <- round(maxstep, digits = 0)
maxinterval <- mean %>% 
               filter(stepsMean==max(mean$stepsMean)) %>% 
               select(interval)
max <- maxinterval[[1]]
```

#### *The interval **835** minutes contains the maximum number of average steps at **206**.* ####


## Imputing missing values

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
data2 <- left_join(data, mean, by="interval")
datafix <- data2 %>% mutate(stepfix=ifelse(is.na(steps), stepsMean, steps))

totalfix <- datafix %>% 
            group_by(day) %>% 
            summarize(daysteps=sum(stepfix, na.rm = T)) %>% ungroup()
ggplot(totalfix)+
geom_histogram(aes(x=day, y=daysteps), stat='identity')+
labs(x="Day",
     y="Steps",
     title="Total Numbers of Steps Per Day with Missing steps Filled")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
summary(totalfix$daysteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

```r
mean <- round(mean(totalfix$daysteps), digits = 0)
```
#### *After missing values for steps was filled with mean for 5-minute interval,  both mean and median total number of steps taken per date **increased**.* ####


## Are there differences in activity patterns between weekdays and weekends?


```r
datafix_wd <- datafix %>% 
            mutate(weekDay=wday(ymd(date),label=T), daytype=ifelse(weekDay %in% c('Sat','Sun'), "weekend","weekday"))

wdmean <- datafix_wd %>% 
         group_by(daytype,interval) %>% 
         summarize(stepsMean2=mean(steps, na.rm=T)) %>% 
         ungroup()

ggplot(wdmean, aes(x=interval, y=stepsMean2))+
    geom_line()+
    facet_wrap(~daytype, nrow = 2)+
    labs(y="Number of Steps",
         title="Difference in activity pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

