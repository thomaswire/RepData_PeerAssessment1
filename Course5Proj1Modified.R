```{r}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if(!file.exists("actmon.zip")){
    download.file(fileUrl, destfile = "actmon.zip")
}

unzip("actmon.zip")

actMonData <- read.csv("activity.csv")

# Convert "date" factor to Date class
actMonData$date <- as.Date(actMonData$date)

# Calculate the total number of steps taken per day
dailyStepsByDate <- aggregate(actMonData$steps, by=list(actMonData$date), sum)

# Cleanup column names
names(dailyStepsByDate) <- c("Date", "Steps")

# Plot histogram of daily steps
png("HistogramOfTotalStepsEachDay.png")
hist(dailyStepsByDate$Steps, main = "Total Number of Steps Taken Each Day", xlab = "Number of Steps")
dev.off()

# Calculate mean number of steps per day
mean(dailyStepsByDate$Steps,na.rm = TRUE)

# Calculate median number of steps per day
median(dailyStepsByDate$Steps,na.rm = TRUE)

# Calculate the mean number of steps taken by interval
dailyStepsByInterval <- tapply(actMonData$steps, actMonData$interval, mean, na.rm = TRUE)

# Plot time series of average daily steps
png("TimeSeriesOfAverageStepsEachDay.png")
plot(row.names(dailyStepsByInterval), dailyStepsByInterval, type="l", main = "Average Daily Number of Steps by Interval", xlab = "Interval", ylab = "Average Daily Number of Steps")
dev.off()

# Find interval with maximnum number of steps
maxInterval <- which.max(dailyStepsByInterval)
names(maxInterval[1])

# Count NAs
sum(is.na(actMonData$steps))

# Copy Data
actMonDataFilledNas <- actMonData

# Replace NAs with average for appropriate interval
for (naRowNumber in which(is.na(actMonDataFilledNas$steps))) {
    
    interval <- actMonDataFilledNas[naRowNumber,]$interval
    replacingValue <- dailyStepsByInterval[toString(interval)]
    actMonDataFilledNas[naRowNumber,]$steps <- replacingValue
}


# Calculate the total number of steps taken per day
dailyStepsByDateWithNasFilled <- aggregate(actMonDataFilledNas$steps, by=list(actMonDataFilledNas$date), sum)

# Cleanup column names
names(dailyStepsByDateWithNasFilled) <- c("Date", "Steps")

# Plot histogram of daily steps
png("HistogramOfTotalStepsEachDayWithNasFilled.png")
hist(dailyStepsByDateWithNasFilled$Steps, main = "Total Number of Steps Taken Each Day (NAs Filled)", xlab = "Number of Steps")
dev.off()

# Calculate mean number of steps per day
mean(dailyStepsByDateWithNasFilled$Steps,na.rm = TRUE)

# Calculate median number of steps per day
median(dailyStepsByDateWithNasFilled$Steps,na.rm = TRUE)

# Add WeekdayWeekend column
actMonDataFilledNas$WeekdayWeekend <- factor((weekdays(actMonDataFilledNas$date) %in% c("Saturday", "Sunday")), levels = c(FALSE, TRUE), labels=c("Weekday", "Weekend"))


# Calculate the mean number of steps taken by interval
weekdayData <- actMonDataFilledNas[which(actMonDataFilledNas$WeekdayWeekend == "Weekday"),]
weekendData <- actMonDataFilledNas[which(actMonDataFilledNas$WeekdayWeekend == "Weekend"),]

dailyStepsByIntervalWeekdays <- tapply(weekdayData$steps, weekdayData$interval, mean, na.rm = TRUE)
dailyStepsByIntervalWeekends <- tapply(weekendData$steps, weekendData$interval, mean, na.rm = TRUE)


# Plot time series to compare weekend/weekdays
png("WeekdayWeekendCompTimeSeries.png")
par(mfrow=c(2,1))
plot(row.names(dailyStepsByIntervalWeekdays), dailyStepsByIntervalWeekdays, type="l", main = "Weekdays", xlab = "Interval", ylab = "Average Daily Number of Steps", ylim= c(0,250))
plot(row.names(dailyStepsByIntervalWeekends), dailyStepsByIntervalWeekends, type="l", main = "Weekends", xlab = "Interval", ylab = "Average Daily Number of Steps", ylim= c(0,250))
dev.off()