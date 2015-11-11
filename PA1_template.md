# Loading and preprocessing the data

setwd("C:/Users/Claudiokass/Desktop/R")
stepsdata<-read.csv("activity.csv",sep=",",header=T)

# What is mean total number of steps taken per day?

STxYR<-aggregate(steps ~ date, data=stepsdata, FUN=sum)
hist(STxYR$steps)
mean(STxYR$steps)
median(STxYR$steps,na.rm=T)

# What is the average daily activity pattern?



STxINT<-aggregate(steps ~ interval, data=stepsdata, FUN=mean)

plot(STxINT$interval,STxINT$steps,type="l")
STxINT[which.max(STxINT$steps),"interval"]

# Imputing missing values

sum(is.na(stepsdata))
# create new variable on original database 
library(plyr)
stepsdata<-ddply(stepsdata,("interval"),transform,intmean = mean(steps,na.rm=T))
r<-nrow(stepsdata)

for(i in 1:r){if (is.na(stepsdata[i,"steps"])){stepsdata[i,"steps"]<-stepsdata[i,"intmean"]}}

fSTxYR<-aggregate(steps ~ date, data=stepsdata, FUN=sum)
hist(STxYR$steps)
mean(fSTxYR$steps)
median(fSTxYR$steps,na.rm=T)
r<-nrow(stepsdata)
for(i in 1:r){if (weekdays(as.Date(stepsdata$date[i]))=="Saturday" | weekdays(as.Date(stepsdata$date[i]))=="Sunday"){stepsdata[i,"WD"]<-"Weekend"}else{stepsdata[i,"WD"]<-"Weekday"}}
library(ggplot2)
stepsdata<-ddply(stepsdata,c("interval","WD"),transform,intmean = mean(steps,na.rm=T))
qplot(interval,steps, data=stepsdata, geom="line")+facet_grid(WD ~.)