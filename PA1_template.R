#1
actData <- read.csv('activity.csv')

#2
stepsPerDay <- aggregate(steps ~ date, data = actData, FUN = sum,na.rm=TRUE)

hist(stepsPerDay$steps,
     col='blue',
     breaks=20,
     xlab='Total steps per day',
     ylab='Frequency',
     main='Daily total steps distribution')

meanStepsPerDay <- mean(stepsPerDay$steps)
meanStepsPerDay

medianStepsPerDay <- median(stepsPerDay$steps)
medianStepsPerDay

#3
meanStepsPerInterval <- tapply(actData$steps, actData$interval, mean, na.rm=TRUE )

plot(row.names(meanStepsPerInterval),
     meanStepsPerInterval,
     type="l",
     xlab = 'Interval',
     ylab = 'Steps',
     main = 'Average daily activity pattern')

maxStepsByInt <- max(meanStepsPerInterval)
x <- meanStepsPerInterval[match(maxStepsByInt,meanStepsPerInterval)]
names(x)

#4              
totalMissingValues <- sum(is.na(actData))
totalMissingValues          
                
filledData <- actData
filledData$steps[is.na(filledData$steps)] <- meanStepsPerInterval[as.character(filledData$interval[is.na(filledData$steps)])]
totalMissingValues2 <- sum(is.na(filledData))
totalMissingValues2

stepsPerDayF <- aggregate(steps ~ date, data = filledData, FUN = sum, na.rm=TRUE)
hist(stepsPerDayF$steps,
     col='blue',
     breaks=20,
     xlab='Total steps per day',
     ylab='Frequency',
     main='Daily total steps distribution (Filled)')

meanStepsPerDayF <- mean(stepsPerDayF$steps)
meanStepsPerDayF

medianStepsPerDayF <- median(stepsPerDayF$steps)
medianStepsPerDayF

#5
filledData$date <- as.Date(filledData$date)
filledData$day <- weekdays(filledData$date)
filledData$weekend <- as.factor(ifelse(filledData$day == 'sábado'| filledData$day ==  'domingo', 'weekend', 'weekday'))

meanStepsPerInterval2 <- aggregate(steps ~ interval + weekend, filledData, mean)

library(lattice)
xyplot(steps ~ interval | factor(weekend), 
       data=meanStepsPerInterval2,
       type='l',
       lty=1,
       layout = c(1, 2),
       xlab='Total steps per day',
       ylab='Frequency',
       main='Weekdays and weekends activity patterns')
                                   
