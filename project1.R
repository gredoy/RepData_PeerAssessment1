##Loading and preprocessing the data
#Load the data (i.e. read.csv())
dataset <- read.csv("activity.csv", colClasses = "character")

#Process/transform the data (if necessary) into a format 
#suitable for your analysis
steps_as_numeric <- as.numeric(dataset$steps)
dataset[,1] <- steps_as_numeric

date_as_date <- as.Date(dataset$date)
dataset[,2] <- date_as_date

interval_as_numeric <- as.numeric(dataset$interval)
dataset[,3] <- interval_as_numeric

#A version of the dataset with NAs removed
dataset_noNAs <- dataset[complete.cases(dataset),]

#set up the margins and graphing parameters
par(mar=c(3,4,1,1))
par(mfrow=c(1,1))

##What is mean total number of steps taken per day?
#Make a histogram of the total number of steps taken each day
#consider the dataset after removing all the NAs: dataset_noNAs
total_steps_per_day <- sapply(split(dataset_noNAs$steps, dataset_noNAs$date), sum)

hist(total_steps_per_day, col="Red")

#Calculate and report the mean and median total number of steps
#taken per day
#consider the dataset after removing all the NAs: dataset_noNAs
#calculate the average steps taken over every time interval
average_steps_per_day <- sapply(split(dataset_noNAs$steps, dataset_noNAs$date), mean)
median_steps <- sapply(split(dataset_noNAs$steps, dataset_noNAs$date), median)

##What is the average daily activity pattern?
#Make a time series plot (i.e. type = "l") of the 5-minute interval
#(x-axis) and the average number of steps taken, averaged across
#all days (y-axis)
#consider the dataset after removing all the NAs: dataset_noNAs
#calculate the average steps taken over every time interval: average_steps
average_steps_per_interval <- sapply(split(dataset_noNAs$steps, dataset_noNAs$interval), mean)
intervals_list <- split(dataset_noNAs$interval, dataset_noNAs$interval)

#The plot of average steps taken over every interval in the dataset
#across all days
intervals <- names(intervals_list)
plot(intervals, average_steps_per_interval, type="l", xlab="Intervals", ylab="Average steps", col="Blue")

#Which 5-minute interval, on average across all the days in the
#dataset, contains the maximum number of steps?
maximum_steps <- max(average_steps_per_interval)
index_of_maximum_steps <- match(maximum_steps, average_steps_per_interval)
maximum_steps_interval <- names(intervals)[index_of_maximum_steps]

##Imputing missing values
#Calculate and report the total number of missing values in the
#dataset (i.e. the total number of rows with NAs)
row_with_NAs <- sum(is.na(data))

#Devise a strategy for filling in all of the missing values in the
#dataset. The strategy does not need to be sophisticated. For
#example, you could use the mean/median for that day, or the mean
#for that 5-minute interval, etc.
#My strategy: replace all the NAs with the mean for that 5-minute
#interval

#Create a new dataset that is equal to the original dataset but
#with the missing data filled in.
dataset_NAsfilled <- read.csv("activity.csv", colClasses = "character")

#Process/transform the data (if necessary) into a format 
#suitable for your analysis
steps_as_numeric <- as.numeric(dataset_NAsfilled$steps)
dataset_NAsfilled[,1] <- steps_as_numeric

date_as_date <- as.Date(dataset_NAsfilled$date)
dataset_NAsfilled[,2] <- date_as_date

interval_as_numeric <- as.numeric(dataset_NAsfilled$interval)
dataset_NAsfilled[,3] <- interval_as_numeric

#Process/transform the dataset (if necessary) by replacing the
#NA values with the mean for the corresponding 5-minute interval
for (i in seq(intervals)) {
        interval <- as.numeric(intervals[i])
        
        for (row in seq(dataset_NAsfilled$steps)) {
                if (is.na(dataset_NAsfilled$steps[row]) && dataset_NAsfilled$interval[row] == interval) {
                        dataset_NAsfilled$steps[row] = average_steps_per_interval[i]
                }
        }
        #print(i)
}

#Make a histogram of the total number of steps taken each day and
#Calculate and report the mean and median total number of steps 
#taken per day. Do these values differ from the estimates from the
#first part of the assignment? What is the impact of imputing 
#missing data on the estimates of the total daily number of steps?
total_steps_per_day_NAsfilled <- sapply(split(dataset_NAsfilled$steps, dataset_NAsfilled$date), sum)

hist(total_steps_per_day_NAsfilled, col="Green")

average_steps_per_day_NAsfilled <- sapply(split(dataset_NAsfilled$steps, dataset_NAsfilled$date), mean)
median_steps_NAsfilled <- sapply(split(dataset_NAsfilled$steps, dataset_NAsfilled$date), median)

##Are there differences in activity patterns between weekdays
#and weekends?
#Create a new factor variable in the dataset with two
#levels – “weekday” and “weekend” indicating whether a given
#date is a weekday or weekend day.
weekday <- vector(mode="character")
day_of_the_week <- weekdays(dataset_NAsfilled$date)

for (i in seq(dataset_NAsfilled$steps)) {
        #print(i)
        day <- day_of_the_week[i]
        if (day == "Saturday" || day == "Sunday") {
                weekday <- append(weekday, "weekend")
        }
        else {
                weekday <- append(weekday, "weekday")
        }
}
#modify the data frame with the weekday/weekend column/factor
dataset_modified <- cbind(dataset_NAsfilled, weekday)

#Make a panel plot containing a time series plot (i.e. type = "l")
#of the 5-minute interval (x-axis) and the average number of 
#steps taken, averaged across all weekday days or weekend 
#days (y-axis). 
#weekday data
weekday_dataframe <- dataset_modified[dataset_modified$weekday == "weekday",]
average_steps_weekday <- sapply(split(weekday_dataframe$steps, weekday_dataframe$interval), mean)
interval1_list <- split(weekday_dataframe$interval, weekday_dataframe$interval)
interval1 <- names(interval1_list)

#weekend data
weekend_dataframe <- dataset_modified[dataset_modified$weekday == "weekend",]
average_steps_weekend <- sapply(split(weekend_dataframe$steps, weekend_dataframe$interval), mean)
interval2_list <- split(weekend_dataframe$interval, weekend_dataframe$interval)
interval2 <- names(interval2_list)

#set up the margins
par(mar=c(4,4,2,1))
par(mfrow=c(2,1))
plot(interval2, average_steps_weekend, type="l", xlab="Intervals", ylab="Number of average steps", col="Blue", main="weekend")
plot(interval1, average_steps_weekday, type="l", xlab="Intervals", ylab="Number of average steps", col="Blue", main="weekday")
