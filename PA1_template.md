---
title: "PA1_template"
author: "Claudio Castorina"
date: "November 11, 2015"
output: html_document
---
# Loading and preprocessing the data

```{r,echo=TRUE,results='asis'}
setwd("C:/Users/Claudiokass/Desktop/R")
stepsdata<-read.csv("activity.csv",sep=",",header=T)
```

# What is mean total number of steps taken per day?

```{r,echo=TRUE,results='asis'}
STxYR<-aggregate(steps ~ date, data=stepsdata, FUN=sum)
hist(STxYR$steps)
mean(STxYR$steps,na.rm=T)
```

```{r,echo=TRUE}
median(STxYR$steps,na.rm=T)
```
# What is the average daily activity pattern?

```{r,echo=TRUE}
STxINT<-aggregate(steps ~ interval, data=stepsdata, FUN=mean)
plot(STxINT$interval,STxINT$steps,type="l")
```

```{r,echo=TRUE}
STxINT[which.max(STxINT$steps),"interval"]
```

# Imputing missing values

```{r,echo=TRUE}
sum(is.na(stepsdata))
```

Created a new variable on original database: step mean for every 5-minute interval across the days so that they will fill NA row in "step" field.

```{r,echo=TRUE}
library(plyr)
stepsdata<-ddply(stepsdata,("interval"),transform,intmean =  mean(steps,na.rm=T))
```

```{r,echo=TRUE}
r<-nrow(stepsdata)
for(i in 1:r){if (is.na(stepsdata[i,"steps"])){stepsdata[i,"steps"]<-stepsdata[i,"intmean"]}}

fSTxYR<-aggregate(steps ~ date, data=stepsdata, FUN=sum)
hist(STxYR$steps)
```

As shown below, mean and median are not sensibly different from what has been calculated at the beginning of the report.

```{r,echo=TRUE}
mean(fSTxYR$steps)
```

```{r,echo=TRUE}
median(fSTxYR$steps,na.rm=T)
```

# Are there differences in activity patterns between weekdays and weekends?

```{r,echo=TRUE}
r<-nrow(stepsdata)
for(i in 1:r){if (weekdays(as.Date(stepsdata$date[i]))=="Saturday" | weekdays(as.Date(stepsdata$date[i]))=="Sunday"){stepsdata[i,"WD"]<-"Weekend"}else{stepsdata[i,"WD"]<-"Weekday"}}
library(ggplot2)
stepsdata<-ddply(stepsdata,c("interval","WD"),transform,intmean = mean(steps,na.rm=T))
qplot(interval,steps, data=stepsdata, geom="line")+facet_grid(WD ~.)
```