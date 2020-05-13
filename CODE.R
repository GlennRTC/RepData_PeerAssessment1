# LIBRARIES:
library(dplyr)
library(ggplot2)
# READ FILE:
steps_table <- read.csv("activity.csv")
daily_steps <- aggregate(steps~date + interval, steps_table, sum, na.rm = TRUE)
# CREATE HISTOGRAM
hist(daily_steps$steps, breaks = 20, main = "Daily Steps", xlab = 'N of Steps', col = 'skyblue')
# [REPORT]CALCULATE MEAN AND MEDIAN:
steps_mean = mean(daily_steps$steps)
steps_median = median(daily_steps$steps)
# TIME SERIES PLOT:
steps_interval <- aggregate(steps~interval, steps_table, mean, na.rm = TRUE)
plot(steps_interval$interval, steps_interval$steps, type = 'l', col = "skyblue", lwd = 3, pch = 19, xlab = "Interval", ylab = "N of Steps", main = "Average N of Steps")
max_steps = steps_interval[which.max(steps_interval$steps), ]
points(max_steps$interval,  max_steps$steps, col = 'red', lwd = 3, pch = 19)
abline(v = max_steps$interval, col = 'red', lty = 3, lwd = 3)
# 5 MINUTES MAX AVG STEPS
steps_interval[which.max(steps_interval$steps),]$interval

# [REPORT]CALCULATING TOTAL OF MISSING VALUES:
nrow(is.na(steps_table))
# MISSING VALUES HANDELING:
Int_Steps <- function(interval){
    steps_interval[steps_interval$interval==interval,]$steps
}
steps_filled <- steps_table
count = 0 
for(i in 1:nrow(steps_filled)){
    if(is.na(steps_filled[i,]$steps)){
        steps_table[i,]$steps <- Int_Steps(steps_filled[i,]$interval)
        count=count+1
    }
}
head(steps_filled)

# MAKING A HIST:
steps_int_filled <- aggregate(steps~interval, steps_filled, mean)
hist(steps_int_filled$steps, breaks = 20, main = "Daily Steps", xlab = 'N of Steps', col = 'lightgreen')
abline(h = mean(steps_int_filled$steps,  col = 'red', lty = 3, lwd = 3)) +
abline(h = median(steps_int_filled$steps,  col = 'orange', lty = 3, lwd = 3))
# MEAN AND MEDIAN:
mean(steps_int_filled$steps)
median(steps_int_filled$steps)

# CREATING WEEK FACTOR:tapply
steps_filled <- aggregate(steps~date + interval, steps_filled)
weekpart = function(x){
    if(x %in% c('Saturday', 'Sunday')){
        return('Weekend')
    }
    
    return('Weekday')
}

steps_filled$dayname = weekdays(as.Date(steps_filled$date))
steps_filled$daytype = as.factor(apply(as.matrix(steps_filled$dayname), 1, weekpart))
steps_filled = steps_filled[, list(avg_steps = mean(steps_filled, interval != 0)), by = list(interval, daytype)]

# PLOTTING:

require(lattice)
xyplot(steps ~ interval | dayname, steps_filled, layout = c(4,2) , panel = function(x, y, ...) {
    panel.xyplot(x, y, type = "l", col = "skyblue")
    panel.abline(h = mean(y), lty = 2, col = "red")
})


