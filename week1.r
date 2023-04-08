library(ggplot2)

activity<- read.csv("D:/profile/documents/GitHub/activity.csv")



activity$date<- as.Date.factor(activity$date,  "%m/%d/%Y" )

weekday<- weekdays(activity$date)
activity <-cbind(activity, weekday)

summary(activity)


activity.tsteps<- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))

names(activity.tsteps)<- c("dates", "steps")

hist(activity.tsteps$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "darkblue", ylim = c(0,20), breaks = seq(0,25000, by=2500))

mean(activity.tsteps$steps)


median(activity.tsteps$steps)

average.daily.activity<- aggregate(activity$steps, by= list(activity$interval), FUN = mean , na.rm = TRUE)

names(average.daily.activity)<-c("interval", "mean")

plot(average.daily.activity$interval, average.daily.activity$mean, type = "l", xlab = "Interval", ylab = "Average number of steps", main = "Average number of steps per interval")


average.daily.activity[which.max(average.daily.activity$mean),]$interval

median(activity.tsteps$steps)

verage.daily.activity<- aggregate(activity$steps, by= list(activity$interval), FUN = mean , na.rm = TRUE)

names(average.daily.activity)<-c("interval", "mean")

plot(average.daily.activity$interval, average.daily.activity$mean, type = "l", xlab = "Interval", ylab = "Average number of steps", main = "Average number of steps per interval")


average.daily.activity[which.max(average.daily.activity$mean),]$interval

average.daily.activity<- aggregate(activity$steps, by= list(activity$interval), FUN = mean , na.rm = TRUE)

names(average.daily.activity)<-c("interval", "mean")

plot(average.daily.activity$interval, average.daily.activity$mean, type = "l", xlab = "Interval", ylab = "Average number of steps", main = "Average number of steps per interval")

average.daily.activity[which.max(average.daily.activity$mean),]$interval

sum(is.na(activity$steps))


clean.steps<- average.daily.activity$mean[match(activity$interval,average.daily.activity$interval)]

sum(is.na(activity$steps))

clean.steps<- average.daily.activity$mean[match(activity$interval,average.daily.activity$interval)]

activity.clean <- transform(activity, steps = ifelse(is.na(activity$steps), yes = clean.steps, no = activity$steps))

total.clean.steps<- aggregate(steps ~ date, activity.clean, sum)

names(total.clean.steps)<- c("date", "daily.steps")

hist(total.clean.steps$daily.steps, col = "darkblue", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))

mean(total.clean.steps$daily.steps)
median(total.clean.steps$daily.steps)


activity$datetype <- sapply(activity$date, function(x) {
        if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })
        
        activity.datetype<- aggregate(steps~interval+datetype, activity,mean, na.rm =TRUE)
ggplot(activity.datetype, aes(x = interval, y = steps, color = datetype))+ geom_line() + labs(title = "Average daily steps by date type", x = "Interval", y = "Average number of steps") + facet_wrap(~datetype, ncol = 1, nrow = 2) 


