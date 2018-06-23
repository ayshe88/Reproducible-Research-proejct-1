## read the data
fileurl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
f<-file.path(getwd(),"repdata_data_activity.zip")
download.file(fileurl,f)
unzip("repdata_data_activity.zip")
activity<-read.csv("activity.csv")


##process/transform the data (if necessary) into a format suitable for your analysis
activity$date<-as.Date(activity$date)

##What is mean total number of steps taken per day?
##Calculate the total number of steps taken per day
total_steps<-aggregate(activity$steps~activity$date,FUN = sum)
colnames(total_steps)<-c("date","steps")

##Make a histogram of the total number of steps taken each day
hist(total_steps$steps, main = "Total number of steps taken each day", 
     xlab = "Numer of steps", ylab = "frequency",
     col="Pink",breaks = 30)


##Calculate and report the mean and median of the total number of steps taken per day
steps_mean<-mean(total_steps$steps,na.rm = TRUE)
steps_med<-median(total_steps$steps,na.rm = TRUE)

##What is the average daily activity pattern?
##Make a time series plot 
interval_steps<-aggregate(activity$steps~activity$interval,FUN = mean)
colnames(interval_steps)<-c("interval","AV")
plot(interval_steps, xlab ="Interval", ylab = "Average Number of Steps", 
     main = "Average Number of Steps per Interval", type = "l",col="red")
 
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxsteps <- max(interval_steps$AV)
maxinterval<-interval_steps[interval_steps$AV==maxsteps,1]


##Imputing missing values
##Calculate and report the total number of missing values in the dataset
missing_steps<-sum(is.na(activity$steps))

##Devise a strategy for filling in all of the missing values in the dataset. 
imputedActivityData<-cbind(activity,interval_steps$AV)
colnames(imputedActivityData)<-c("steps","date","interval","AV")

##Create a new dataset that is equal to the original dataset but with the missing data filled in
imputedActivityData$steps[is.na(imputedActivityData$steps)] <- imputedActivityData$AV[is.na(imputedActivityData$steps)]
imputedActivityData$weekend<-ifelse(imputedActivityData$weekend==TRUE,c("weekend"),c("weekday"))

##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
total_nonmissing_steps<-aggregate(imputedActivityData$step~imputedActivityData$date,FUN = sum)
colnames(total_nonmissing_steps)<-c("date","steps")
hist(total_nonmissing_steps$steps, xlab = "Total number of steps",
     ylab = "frequency",main = "Total number of steps taken each day", col= "pink", breaks = 30)

new_steps_mean<-mean(total_nonmissing_steps$steps)
new_steps_median<-median(total_nonmissing_steps$steps)

imputedActivityData$weekend<-weekdays(as.Date(imputedActivityData$date)) %in% c("ÇáÓÈÊ","ÇáÇÍÏ")
imputedActivityData$weekend<-as.factor(imputedActivityData$weekend)
final_steps<-aggregate(imputedActivityData$steps~imputedActivityData$interval+imputedActivityData$weekend,FUN=mean)
colnames(final_steps)<-c("interval","weekend","steps")

library(ggplot2)
g<-ggplot(final_steps,aes(x=interval,y=steps))
g+ geom_line(alpha=1/2) + facet_wrap(~ weekend, ncol=1) + labs(x ="Interval") +
  labs(y="Average Number of Steps") 

