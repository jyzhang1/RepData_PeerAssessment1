# Reproducible Research Assignment 1
**Please refer to figure folder for the figures**

The following steps are done to get started:

```r
# setting path to the directory containing all the files
setwd("/Users/Jinyan/Documents/DataScience/reproducibleresearch/Assignment1/")
```


```r
# loading the necessary libraries
library(dplyr)
library(ggplot2)
library(data.table)
```


```r
# read the .csv file, setting header to be TRUE
activity <- read.csv("activity.csv", header  = TRUE)
```

## Question 1: What is mean total number of steps taken per day?


```r
# grouping the data by date and summarising the resulting data table
stepsDate <- group_by(activity, date)
stepsSum <- summarise(stepsDate, sum(steps, na.rm = TRUE))
colnames(stepsSum) <- c("date", "steps")
```


```r
# plotting histogram using ggplot2 system
png("plot1.png", width = 480, height = 480, type = "quartz")
g <- ggplot(stepsSum, aes(x = steps))
z <- g + geom_histogram(binwidth = max(stepsSum$steps)/30) + 
        labs(title = "Histogram of Total Steps Taken Per Day") +
        labs(x = "Total Steps Taken", y = "Frequency")
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(z)
```

![plot of chunk plot1](/Users/Jinyan/Documents/DataScience/reproducibleresearch/Assignment1/figures/plot1-1.png) 

This table shows you the summary of the total, mean and median of steps taken
per day:

```r
# calculating the mean and median of the steps taken per day
stepsMean <- summarise(stepsDate, mean(steps, na.rm = TRUE))
stepsMedian <- summarise(stepsDate, median(steps, na.rm = TRUE))
summaryTable <- cbind(stepsSum, stepsMean[, 2], stepsMedian[, 2])
colnames(summaryTable) <- c("Date", 
                            "Steps Taken (Total)", 
                            "Steps Taken (Mean)", 
                            "Steps Taken (Median)"
                            )
print(summaryTable)
```

```
##          Date Steps Taken (Total) Steps Taken (Mean) Steps Taken (Median)
## 1  2012-10-01                   0                NaN                   NA
## 2  2012-10-02                 126          0.4375000                    0
## 3  2012-10-03               11352         39.4166667                    0
## 4  2012-10-04               12116         42.0694444                    0
## 5  2012-10-05               13294         46.1597222                    0
## 6  2012-10-06               15420         53.5416667                    0
## 7  2012-10-07               11015         38.2465278                    0
## 8  2012-10-08                   0                NaN                   NA
## 9  2012-10-09               12811         44.4826389                    0
## 10 2012-10-10                9900         34.3750000                    0
## 11 2012-10-11               10304         35.7777778                    0
## 12 2012-10-12               17382         60.3541667                    0
## 13 2012-10-13               12426         43.1458333                    0
## 14 2012-10-14               15098         52.4236111                    0
## 15 2012-10-15               10139         35.2048611                    0
## 16 2012-10-16               15084         52.3750000                    0
## 17 2012-10-17               13452         46.7083333                    0
## 18 2012-10-18               10056         34.9166667                    0
## 19 2012-10-19               11829         41.0729167                    0
## 20 2012-10-20               10395         36.0937500                    0
## 21 2012-10-21                8821         30.6284722                    0
## 22 2012-10-22               13460         46.7361111                    0
## 23 2012-10-23                8918         30.9652778                    0
## 24 2012-10-24                8355         29.0104167                    0
## 25 2012-10-25                2492          8.6527778                    0
## 26 2012-10-26                6778         23.5347222                    0
## 27 2012-10-27               10119         35.1354167                    0
## 28 2012-10-28               11458         39.7847222                    0
## 29 2012-10-29                5018         17.4236111                    0
## 30 2012-10-30                9819         34.0937500                    0
## 31 2012-10-31               15414         53.5208333                    0
## 32 2012-11-01                   0                NaN                   NA
## 33 2012-11-02               10600         36.8055556                    0
## 34 2012-11-03               10571         36.7048611                    0
## 35 2012-11-04                   0                NaN                   NA
## 36 2012-11-05               10439         36.2465278                    0
## 37 2012-11-06                8334         28.9375000                    0
## 38 2012-11-07               12883         44.7326389                    0
## 39 2012-11-08                3219         11.1770833                    0
## 40 2012-11-09                   0                NaN                   NA
## 41 2012-11-10                   0                NaN                   NA
## 42 2012-11-11               12608         43.7777778                    0
## 43 2012-11-12               10765         37.3784722                    0
## 44 2012-11-13                7336         25.4722222                    0
## 45 2012-11-14                   0                NaN                   NA
## 46 2012-11-15                  41          0.1423611                    0
## 47 2012-11-16                5441         18.8923611                    0
## 48 2012-11-17               14339         49.7881944                    0
## 49 2012-11-18               15110         52.4652778                    0
## 50 2012-11-19                8841         30.6979167                    0
## 51 2012-11-20                4472         15.5277778                    0
## 52 2012-11-21               12787         44.3993056                    0
## 53 2012-11-22               20427         70.9270833                    0
## 54 2012-11-23               21194         73.5902778                    0
## 55 2012-11-24               14478         50.2708333                    0
## 56 2012-11-25               11834         41.0902778                    0
## 57 2012-11-26               11162         38.7569444                    0
## 58 2012-11-27               13646         47.3819444                    0
## 59 2012-11-28               10183         35.3576389                    0
## 60 2012-11-29                7047         24.4687500                    0
## 61 2012-11-30                   0                NaN                   NA
```

## Question 2: What is the average daily activity pattern?


```r
# grouping the data by interval and summarising the resulting data table
stepsInt <- group_by(activity, interval)
stepsIntMean <- summarise(stepsInt, mean(steps, na.rm = TRUE))
colnames(stepsIntMean) <- c("interval", "steps")
```


```r
# plotting time series plot using ggplot2 system
png("plot2.png", width = 480, height = 480, type = "quartz")
h <- ggplot(stepsIntMean, aes(interval, steps))
y <- h + geom_line() +
        labs(title = "Time Series Plot of Interval VS Average Steps Taken") +
        labs(x = "Interval", y = "Average Steps Taken") + 
        geom_vline(xintercept = 835, linetype = "longdash")
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(y)
```

![plot of chunk plot2](/Users/Jinyan/Documents/DataScience/reproducibleresearch/Assignment1/figures/plot2-1.png) 


```r
# determining which 5-minute interval has the highest average
stepsMax <- which(stepsIntMean$steps == max(stepsIntMean$steps))
maxSteps <- stepsIntMean$interval[stepsMax]
```
The 5-minute interval with highest number of steps taken is **835** as
shown in the plot above.

## Question 3: Imputing missing values


```r
# total number of NAs in the data
totalNA <- sum(!is.na(activity$steps))
```
The 5-minute interval with highest number of steps taken is **15264**.

Since on certain days, total steps taken = 0, we shall use average of steps 
taken per interval to fill in the NA values.
We will select from the mean table the respective values from the intervals.


```r
# converting the activity into data table for merge function later
activityDT <- data.table(activity, key = "interval")
stepsIntMeanDT <- data.table(stepsIntMean, key = "interval")
```


```r
# the following 3 lines convert the NA values to the average values for each
# respective intervals
mergedDT <- activityDT[stepsIntMeanDT]
mergedDT$steps <- as.double(mergedDT$steps)
mergedDT <- mergedDT[is.na(steps), steps := i.steps]
```


```r
# selecting the columns that you want after the conversion
# mergedDT is the new dataset with missing data filled in
mergedDT <- mergedDT[, list(steps, date, interval)]
```


```r
# grouping the data by date and summarising the resulting data table
stepsDate2 <- group_by(mergedDT, date)
stepsSum2 <- summarise(stepsDate2, sum(steps))
colnames(stepsSum2) <- c("date", "steps")
```


```r
# plotting histogram using ggplot2 system
png("plot3.png", width = 480, height = 480, type = "quartz")
i <- ggplot(stepsSum2, aes(x = steps))
x <- i + geom_histogram(binwidth = max(stepsSum2$steps)/30) +
        labs(title = "Histogram of Total Steps Taken Per Day (Filled NAs)") +
        labs(x = "Total Steps Taken", y = "Frequency")
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(x)
```

![plot of chunk plot3](/Users/Jinyan/Documents/DataScience/reproducibleresearch/Assignment1/figures/plot3-1.png) 

This table shows you the summary of the total, mean and median of steps taken
per day after filling in the missing values:

```r
stepsMean2 <- summarise(stepsDate2, mean(steps))
stepsMedian2 <- summarise(stepsDate2, median(steps))
summaryTable2 <- cbind(data.frame(stepsSum2), data.frame(stepsMean2)[, 2], 
                       data.frame(stepsMedian2)[, 2])
colnames(summaryTable2) <- c("Date", 
                            "Steps Taken (Total)", 
                            "Steps Taken (Mean)", 
                            "Steps Taken (Median)"
)
print(summaryTable2)
```

```
##          Date Steps Taken (Total) Steps Taken (Mean) Steps Taken (Median)
## 1  2012-10-01            10766.19         37.3825996             34.11321
## 2  2012-10-02              126.00          0.4375000              0.00000
## 3  2012-10-03            11352.00         39.4166667              0.00000
## 4  2012-10-04            12116.00         42.0694444              0.00000
## 5  2012-10-05            13294.00         46.1597222              0.00000
## 6  2012-10-06            15420.00         53.5416667              0.00000
## 7  2012-10-07            11015.00         38.2465278              0.00000
## 8  2012-10-08            10766.19         37.3825996             34.11321
## 9  2012-10-09            12811.00         44.4826389              0.00000
## 10 2012-10-10             9900.00         34.3750000              0.00000
## 11 2012-10-11            10304.00         35.7777778              0.00000
## 12 2012-10-12            17382.00         60.3541667              0.00000
## 13 2012-10-13            12426.00         43.1458333              0.00000
## 14 2012-10-14            15098.00         52.4236111              0.00000
## 15 2012-10-15            10139.00         35.2048611              0.00000
## 16 2012-10-16            15084.00         52.3750000              0.00000
## 17 2012-10-17            13452.00         46.7083333              0.00000
## 18 2012-10-18            10056.00         34.9166667              0.00000
## 19 2012-10-19            11829.00         41.0729167              0.00000
## 20 2012-10-20            10395.00         36.0937500              0.00000
## 21 2012-10-21             8821.00         30.6284722              0.00000
## 22 2012-10-22            13460.00         46.7361111              0.00000
## 23 2012-10-23             8918.00         30.9652778              0.00000
## 24 2012-10-24             8355.00         29.0104167              0.00000
## 25 2012-10-25             2492.00          8.6527778              0.00000
## 26 2012-10-26             6778.00         23.5347222              0.00000
## 27 2012-10-27            10119.00         35.1354167              0.00000
## 28 2012-10-28            11458.00         39.7847222              0.00000
## 29 2012-10-29             5018.00         17.4236111              0.00000
## 30 2012-10-30             9819.00         34.0937500              0.00000
## 31 2012-10-31            15414.00         53.5208333              0.00000
## 32 2012-11-01            10766.19         37.3825996             34.11321
## 33 2012-11-02            10600.00         36.8055556              0.00000
## 34 2012-11-03            10571.00         36.7048611              0.00000
## 35 2012-11-04            10766.19         37.3825996             34.11321
## 36 2012-11-05            10439.00         36.2465278              0.00000
## 37 2012-11-06             8334.00         28.9375000              0.00000
## 38 2012-11-07            12883.00         44.7326389              0.00000
## 39 2012-11-08             3219.00         11.1770833              0.00000
## 40 2012-11-09            10766.19         37.3825996             34.11321
## 41 2012-11-10            10766.19         37.3825996             34.11321
## 42 2012-11-11            12608.00         43.7777778              0.00000
## 43 2012-11-12            10765.00         37.3784722              0.00000
## 44 2012-11-13             7336.00         25.4722222              0.00000
## 45 2012-11-14            10766.19         37.3825996             34.11321
## 46 2012-11-15               41.00          0.1423611              0.00000
## 47 2012-11-16             5441.00         18.8923611              0.00000
## 48 2012-11-17            14339.00         49.7881944              0.00000
## 49 2012-11-18            15110.00         52.4652778              0.00000
## 50 2012-11-19             8841.00         30.6979167              0.00000
## 51 2012-11-20             4472.00         15.5277778              0.00000
## 52 2012-11-21            12787.00         44.3993056              0.00000
## 53 2012-11-22            20427.00         70.9270833              0.00000
## 54 2012-11-23            21194.00         73.5902778              0.00000
## 55 2012-11-24            14478.00         50.2708333              0.00000
## 56 2012-11-25            11834.00         41.0902778              0.00000
## 57 2012-11-26            11162.00         38.7569444              0.00000
## 58 2012-11-27            13646.00         47.3819444              0.00000
## 59 2012-11-28            10183.00         35.3576389              0.00000
## 60 2012-11-29             7047.00         24.4687500              0.00000
## 61 2012-11-30            10766.19         37.3825996             34.11321
```
To find out the effect of filling in the missing values:

```r
before <- sum(summaryTable[, 2])
after <- sum(summaryTable2[, 2])
after <- as.integer(after)
```
The difference between the number of steps taken before and after filling in the
missing values is calculated by summing up the averages, giving 
**before = 570608** and **after = 656737**. Thus, filling in the missing 
values increases the total number of steps taken.

## Question 4: Are there difference in activity patterns between weekdays and weekends?


```r
# converting the dates to weekdays/weekends
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
weekdays1 <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
activity$wDay <- factor((weekdays(activity$date) %in% weekdays1),
                        levels = c(FALSE, TRUE), 
                        labels = c("Weekend", "Weekday"))

# grouping the data by date and weekday and summarising the resulting data table
activityWday <-group_by(activity, wDay, interval)
wdayMean <- summarise(activityWday, mean(steps, na.rm = TRUE))
colnames(wdayMean) <- c("day", "interval", "steps")
```


```r
# making a panel plot of time series using ggplot2 system
png("plot4.png", width = 480, height = 480, type = "quartz")
wp <- ggplot(wdayMean, aes(interval, steps))
w <- wp + geom_line() + facet_grid(day ~ .) + 
        labs(title = "Time Series Plot of Interval VS Average Steps Taken") +
        labs(x = "Interval", y = "Average Steps Taken")
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(w)
```

![plot of chunk plot4](/Users/Jinyan/Documents/DataScience/reproducibleresearch/Assignment1/figures/plot4-1.png) 

## Thank you and congratulations on reading everything! =)
