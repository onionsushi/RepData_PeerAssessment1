setwd("C:/Users/Abel/Desktop/week4/activity monitoring/")
library(dplyr)
library(lattice)
library(ggplot2)
data1_raw <- read.csv("activity.csv")


summary(data1)
str(data1)
##What is mean total number of steps taken per day?


##Calculate the total number of steps taken per day

data1 <- data1_raw[!is.na(data1_raw$steps), ]
data1$date <- as.Date(data1$date)

data1_grouped_date <- group_by(data1,date)
sum_by_date <- summarize_all(data1_grouped_date, sum)
## Make a histogram of the total number of steps taken each day

hist(sum_by_date$steps, breaks = 52)

##Calculate and report the mean and median of the total number of steps taken per day

mean(sum_by_date$steps)
median(sum_by_date$steps)

##What is the average daily activity pattern?	
data1_grouped_interval <- group_by(data1, interval)
mean_by_interval <- summarize_all(data1_grouped_interval, mean)

##Make a time series plot
plot(mean_by_interval$steps~ mean_by_interval$interval, type="l" )

##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
mean_by_interval[rev(order(mean_by_interval$steps)),][1,]

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
summary(is.na(data1_raw))

#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

data1_modified <- data1_raw
selected_intervals <- (data1_raw$interval[is.na(data1_raw)] %/% 100 ) * 12  + (data1_raw$interval[is.na(data1_raw)] %% 100 ) / 5 + 1

#Create a new dataset that is equal to the original dataset but with the missing data filled in.

data1_modified$steps <- replace(data1_raw$steps, is.na(data1_raw$steps), mean_by_interval$steps[selected_intervals])


#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

data1_modified_grouped <- group_by(data1_modified, date)
sum_by_date_modified <- summarize_all(data1_modified_grouped, sum)
sum_by_date_modified$date <- as.Date(sum_by_date_modified$date)

hist(sum_by_date_modified$steps, breaks =  40)
hist(sum_by_date$steps, breaks =  40)



#Are there differences in activity patterns between weekdays and weekends?
#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
data1$weekday <- weekdays(data1$date)
data1$weekday[data1$weekday != "Saturday" & data1$weekday != "Sunday"] <- "weekday"

data1$weekday[data1$weekday == "Saturday" | data1$weekday == "Sunday"] <- "weekend"

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
data1_weekend <-data1[data1$weekday == "weekend",]
data1_weekdays <- data1[data1$weekday == "weekday",]
data1_weekend <- group_by(data1_weekend, interval)
data1_weekend_summarized <- summarize_all(data1_weekend, mean)
data1_weekdays <- group_by(data1_weekdays, interval)
data1_weekdays_summarized <- summarize_all(data1_weekdays, mean)

plot(data1_weekend_summarized$steps~ data1_weekend_summarized$interval, type="l" )
plot(data1_weekdays_summarized$steps~ data1_weekdays_summarized$interval, type="l" )
