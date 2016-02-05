## Loading the data
data1  <- read.csv("activity.csv",na.string="NA")

## Mean total number of steps taken per day
### The total number of steps taken per day
dailySum <- tapply(data1$steps,data1$date,sum,na.rm=TRUE)
### Histogram of the total number of steps taken each day
par(mfrow=c(1,1))
hist(dailySum,xlab="Daily Sum")
dev.copy(png,"plot1.png")
dev.off()
### Mean and median of the total number of steps taken per day
#### Mean
dailyMean<- tapply(data1$steps,data1$date,mean,na.rm=TRUE)
#### Median
dailyMedian <- tapply(data1$steps,data1$date,median,na.rm=TRUE)

## The time series plot
crossDayMean <- tapply(data1$steps,data1$interval,mean,na.rm=TRUE)
plot(data1[1:288,]$interval,crossDayMean,type="l",xlab="interval",ylab="Cross Day Mean")
dev.copy(png,"plot2.png")
dev.off()
### Find the 5-minute interval containing the maximum number of steps
which.max(crossDayMean)

## Imputing missing values
### Report the total number of missing value
numberNA <- sum(is.na(data1$steps))
data2 <- data1
### Filling strategy is mean for that 5 minute interval
#a <- tapply(data2$steps,data2$interval,function(x){x<-crossDayMean})
a <- data.frame(data2[1:288,]$interval,crossDayMean)
colnames(a)<-c("interval","crossDayMeman")
data2<-merge(data2,a,by.x="interval")
for(i in 1:length(data2$steps)){
        if(is.na(data3[i,]$steps)==TRUE){
                data3[i,]$steps=data2[i,]$crossDayMean
        }
}
### Make histogram of total number of steps taken each day
dailySum <- tapply(data2$steps,data2$date,sum,na.rm=TRUE)
hist(dailySum,xlab="Daily Sum")
dev.copy(png,"plot3.png")
dev.off()
### Calculate mean and median total number of steps taken per day
#### Mean
dailyMeanAfter <- tapply(data2$steps,data2$date,mean,na.rm=TRUE)
#### Median
dailyMedianAfter <- tapply(data2$steps,data2$date,median,na.rm=TRUE)

## Are there difference in activity patterns between weekdays and weekends
data2$date <- as.Date(data2$date)
### Create a new variable differentiating weekday and weekend
data2$week <- weekdays(data2$date)
data2$weekday <- weekdays(data2$date)
for(i in 1:length(data2$steps)){
        if(data2[i,]$week==c("Saturday","Sunday")){
                data2[i,]$weekday <- "weekend"
        } else {
                data2[i,]$weekday <- "weekday"
                }
}
### Make the plot
par(mfrow=c(1,2))
crossWeekdayMean <- tapply(data2$steps,as.factor(data2$weekday),mean,na.rm=TRUE)
plot(data2[1:288,]$interval,crossDayMean,type="l",xlab="interval",ylab="Cross Day Mean")
plot(as.factor(c("weekday","weekend")),crossWeekdayMean,type="l",xlab="weekday or weekend",ylab="Cross Weekday Mean")
dev.copy(png,"plot4.png")
dev.off()