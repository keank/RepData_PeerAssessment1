# install packages
require(knitr)
require(lubridate)
require(dplyr)
require(ggplot2)

##
# Load the data
setwd('C:/Users/Kean/R/Reproducible_Research/RepData_PeerAssessment1')
data=read.csv('activity.csv') 

# Pre-process the data
data$date=ymd(data$date) # change dates from factor to date

## What is mean total number of steps per day
# Calculate total number of steps taken per day
cdata=data[complete.cases(data),]
stepsperday=cdata %>% group_by(date) %>% summarise(totalsteps=sum(steps))
# Plot a histogram of total number of steps a day
hist(stepsperday$totalsteps, main='Histogram of Total Number of Steps a Day', xlab='Number of Steps', breaks=20, ylim=c(0,20))
# Mean and median number of steps a day
mean(stepsperday$totalsteps)
median(stepsperday$totalsteps)

## Average daily pattern
stepsbyinterval=cdata %>% group_by(interval) %>% summarise(meansteps=mean(steps))
plot(stepsbyinterval$interval, stepsbyinterval$meansteps, type='l', 
     main='Time Series Plot of Average Steps by Interval', xlab='Interval')
stepsbyinterval[which.max(stepsbyinterval$meansteps),]

## Imputing missing values
# Total number of rows with NAs
table(complete.cases(data))[1]
icdata=NULL
# Using mean value for period as a replacement for NAs
for (i in unique(data$interval)){
  icdatat=subset(data[!complete.cases(data),], interval==i)
  icdatat$steps=unlist(rep(subset(stepsbyinterval, interval==i)[2], nrow(subset(data[!complete.cases(data),], interval==i))))
  icdata=rbind(icdata,icdatat)
}
idata=rbind(cdata,icdata)
# Histogram of total number of steps per day
istepsperday=idata %>% grsoup_by(date) %>% summarise(totalsteps=sum(steps))
hist(istepsperday$totalsteps, main='Histogram of Total Number of Steps a Day \n (with Imputation)', xlab='Number of Steps', breaks=20, ylim=c(0,20))
# hist(stepsperday$totalsteps, main='Histogram of Total Number of Steps a Day', xlab='Number of Steps', col='blue', breaks=20, add=T, alpha=0.5)
## note all the days missing values are fully empty 
## subset(data, is.na(steps)) %>% group_by(date) %>% summarise(length(interval)) shows this
mean(istepsperday$totalsteps)
median(istepsperday$totalsteps)

## Are there differences?
idata = idata %>% mutate(dayofweek=weekdays(date))
idata = idata %>% mutate(daytype=ifelse(dayofweek %in% c('Saturday','Sunday'), 'weekend', 'weekday'))
idata$daytype = as.factor(idata$daytype)
idata$dayofweek=NULL

stepsbyinterval_dow=idata %>% group_by(interval, daytype) %>% summarise(meansteps=mean(steps))
stepsbyinterval_dow
qplot(interval,meansteps,data=stepsbyinterval_dow, facets= daytype ~ ., geom='line' 
      ,ylab='Number of Steps', xlab='Interval', main='Average Number of Steps in each interval \nfor Weekdays and Weekends')
