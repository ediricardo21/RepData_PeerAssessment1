library(readr)
library(ggplot2)
library(dplyr)
activity <- read_csv("activity.csv", col_types = cols(date = col_date(format = "%Y-%m-%d")))

png("plot1.png",width=480,height=480,units="px",bg="transparent")

totalsteps <-  activity %>%  filter(!is.na(steps)) %>%
	group_by(date) %>%
	summarize(total_steps = sum(steps))
hist <- ggplot(totalsteps,aes(total_steps))+
	geom_histogram(boundary=0, binwidth=3000, col="brown", fill="lightblue")+
	ggtitle("Histogram per day")+ylab("Frequency")+xlab("Steps")
hist
print(hist)

dev.off()

steps_mean <- mean(totalsteps$total_steps)
steps_mean
steps_median <- median(totalsteps$total_steps)
steps_median

meansteps <- activity %>%  filter(!is.na(steps)) %>%
	group_by(interval) %>%
	summarize(mean_steps = mean(steps))
png("plot2.png",width=480,height=480,units="px",bg="transparent")
timeserie <- ggplot(meansteps,aes(interval,mean_steps))+geom_line()+ggtitle("Average steps per time interval")+
	xlab("Time")+ylab("Steps")
timeserie
print(timeserie)

# Max
maxim <- meansteps

maxim <- maxim %>% filter(mean_steps==max(maxim$mean_steps))
maxim

sum(is.na(activity$steps))

newdata <- activity
newdata$CompleteSteps <- ifelse(is.na(newdata$steps), 
round(meansteps$mean_steps[match(newdata$interval, meansteps$interval)],2), newdata$steps)

newtotalsteps <-  newdata %>%
	group_by(date) %>%
	summarize(total_steps = sum(CompleteSteps))
png("plot3.png",width=480,height=480,units="px",bg="transparent")
hist2 <- ggplot(newtotalsteps,aes(total_steps))+
	geom_histogram(boundary=0, binwidth=3000, col="brown", fill="lightblue")+
	ggtitle("Histogram per day")+ylab("Frequency")+xlab("Steps")
hist2
print(hist2)

dev.off()

steps_mean2 <- mean(newtotalsteps$total_steps)

steps_median2 <- median(newtotalsteps$total_steps)


mean <- cbind(steps_mean,steps_mean2)
median <- cbind(steps_median,steps_median2)
result <- rbind(mean,median)
rownames(result) <- c("Mean","Median")
colnames(result) <- c("Step1","Step2")
result



newdata$days <- weekdays(newdata$date)


newdata$type <- ifelse(newdata$days == "Monday"|newdata$days == "Tuesday"|
	        	newdata$days == "Wednesday"|newdata$days == "Thursday"|
	        	newdata$days == "Friday","Weekday","Weekend")
newdata$type <- as.factor(newdata$type)

totalsteps3 <- newdata %>% group_by(interval,type) %>% summarise(total_steps = sum(CompleteSteps))

png("plot4.png",width=480,height=480,units="px",bg="transparent")

timeserie2 <- ggplot(totalsteps3,aes(interval,total_steps,color=type))+ facet_wrap(~type , ncol = 1, nrow=2)+geom_line()+
	ggtitle("Average steps per time interval")+
	xlab("Time")+ylab("Steps")
timeserie2
print(timeserie2)

dev.off()

