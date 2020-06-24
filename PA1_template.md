Coursera: Reproducible Research - Week 2 Project 1
--------------------------------------------------

Introduction:
-------------

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:
Dataset: Activity monitoring data \[52K\] The variables included in this
dataset are: steps: Number of steps taking in a 5-minute interval
(missing values are coded as NA) date: The date on which the measurement
was taken in YYYY-MM-DD format interval: Identifier for the 5-minute
interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this dataset.

Loading and preprocessing the data
----------------------------------

Check if the data file already exists if not, download/unzip the file

    if (!file.exists("./activity.csv")) {
       
        file_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
       file_name <- "activity.zip"
       download.file(file_url, file_name, method = "curl")
       unzip(file_name)
    }

Load packages

    library(datasets)
    library(ggplot2)
    library(plyr)

Load the data

    activity <- read.csv("activity.csv")

Convert the variables and apply names to columns

    activity$date <- as.Date(activity$date)
    activity$interval <- as.factor(activity$interval)
    names(activity)

    ## [1] "steps"    "date"     "interval"

    lapply(activity, class)

    ## $steps
    ## [1] "integer"
    ## 
    ## $date
    ## [1] "Date"
    ## 
    ## $interval
    ## [1] "factor"

What is mean total number of steps taken per day?
-------------------------------------------------

Data processing - sum the steps per day and remove any missing values

    data <- ddply(activity[,1:2], .(date), function(set) { sum(set$steps, na.rm = TRUE) })
    names(data) <- c("date", "steps")

Create a Histogram of the total number of steps taken each day

    ggplot(data = data) + aes(x = factor(date), y = steps) + geom_histogram(stat = "identity") + labs(x ="Date", y = "Total number of steps") + theme(axis.text.x=element_text(angle = -90, hjust = 0))

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

Calculate the mean average and median number of total steps taken per
day

    mean(data$steps)

    ## [1] 9354.23

    median(data$steps)

    ## [1] 10395

What is the average daily activity pattern?
-------------------------------------------

Data processing - calculate the mean steps for each interval and remove
any missing values

    data2 <- ddply(activity, .(interval), function(set) { mean(set$steps, na.rm = TRUE) })
    names(data2) <- c("interval", "steps")

Create a Time series plot of the average number of steps taken

    hour_intervals <- c(12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156, 168, 180, 192, 204, 216, 228, 240, 252, 264, 276)

    ggplot(data = data2) + aes(x = factor(interval), y = steps, group = 1) + geom_line() + labs(x ="5-minute interval", y = "Average number of steps") + theme(axis.text.x = element_text(size = 0)) + geom_vline(xintercept= hour_intervals, linetype="dotted") + geom_vline(xintercept= 144, colour = "red") + geom_text(x=144, y = 150, label="12 pm", angle = 90)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

Calculate the 5-minute interval that, on average, contains the maximum
number of steps

    data2[data2$steps == max(data2$steps), ]$interval

    ## [1] 835
    ## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 125 ... 2355

Imputing missing values
-----------------------

Calculate the number of rows with missing steps values

    nrow(activity[is.na(activity$steps),])

    ## [1] 2304

There are 2304 rows with steps = ‘NA’

Develop a strategy for imputing missing data - replace the NAs with the
average 5-minute interval based on the day of the week. \# Add a
variable to retain the original order of the data

    activity$order <- 1:nrow(activity)

Impute the NAs by replacing them with the mean of the total steps

    impute_mean <- function(x) {
      replace(x, is.na(x), mean(x, na.rm = TRUE))
    }

    imputed_data <- ddply(activity, .(interval), transform, steps = impute_mean(steps))

Data processing - order the data to the original order and apply the
variable classes

    imputed_data <- imputed_data[order(imputed_data$order), ]
    names(activity)

    ## [1] "steps"    "date"     "interval" "order"

    lapply(activity, class)

    ## $steps
    ## [1] "integer"
    ## 
    ## $date
    ## [1] "Date"
    ## 
    ## $interval
    ## [1] "factor"
    ## 
    ## $order
    ## [1] "integer"

Now that missing values have been imputed, a histogram can be produced
for the total number of steps taken each day. First the data for the
plot is prepared by summing the number of steps for each day:

    data2 <- ddply(imputed_data[,1:2], .(date), function(set) { sum(set$steps, na.rm = TRUE) })
    names(data2) <- c("date", "steps")

Create a Histogram of the total number of steps taken each day after
missing values are imputed

    ggplot(data = data2) + aes(x = factor(date), y = steps) + geom_histogram(stat = "identity") + labs(x ="Date", y = "Total number of steps") + theme(axis.text.x=element_text(angle = -90, hjust = 0))

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-16-1.png)

Calculate and report the mean and median total number of steps taken per
day

    mean(data2$steps)

    ## [1] 10766.19

    median(data2$steps)

    ## [1] 10766.19

Compare the mean and median total number of steps taken before and after
the data imputation.

Results with missing values Mean: 9354 Median: 10395

Results with imputed missing values Mean: 10766 Median: 10766

Question: Do these values differ from the estimates from the first part
of the assignment? Answer: The mean and median are higher in the dataset
where the missing values have been imputed.

Question: What is the impact of imputing missing data on the estimates
of the total daily number of steps? Answer: The mean and median in the
dataset where the missing values have been imputed are equal.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

A factor variable is created in the dataset with two levels – “weekday”
and “weekend” indicating whether a given date is a weekday or weekend
day.

    dayofweek <- weekdays(imputed_data$date)
    imputed_data$dayofweek <- ifelse(dayofweek == "Saturday" | dayofweek == "Sunday" , c("weekend"), c("weekday"))
    imputed_data$dayofweek <- factor(imputed_data$dayofweek)

Data processing - apply the variable classes

    names(imputed_data)

    ## [1] "steps"     "date"      "interval"  "order"     "dayofweek"

    lapply(imputed_data, class)

    ## $steps
    ## [1] "numeric"
    ## 
    ## $date
    ## [1] "Date"
    ## 
    ## $interval
    ## [1] "factor"
    ## 
    ## $order
    ## [1] "integer"
    ## 
    ## $dayofweek
    ## [1] "factor"

Calculate the mean number of steps per interval for weekdays

    data_weekdays <- ddply(imputed_data[imputed_data$dayofweek == "weekday", ], .(interval), function(set) { mean(set$steps, na.rm = TRUE) })

    names(data_weekdays) <- c("interval", "steps")

Calculate the mean number of steps per interval for weekends

    data_weekends <- ddply(imputed_data[imputed_data$dayofweek == "weekend", ], .(interval), function(set) { mean(set$steps, na.rm = TRUE) })

    names(data_weekends) <- c("interval", "steps")

Create a Panel plot, two time series plots, comparing the average number
of steps taken per 5-minute interval across weekdays and weekends

    plot1 <- ggplot(data = data_weekdays) + aes(x = factor(interval), y = steps, group = 1) + geom_line() + labs(x ="5-minute interval", y = "Average number of steps across weekdays") + theme(axis.text.x = element_text(size = 0)) + geom_vline(xintercept= hour_intervals, linetype="dotted") + geom_vline(xintercept= 144, colour = "red") + geom_text(x=144, y = 150, label="12 pm", angle = 90)

    plot2 <- ggplot(data = data_weekends) + aes(x = factor(interval), y = steps, group = 1) + geom_line() + labs(x ="5-minute interval", y = "Average number of steps across weekends") + theme(axis.text.x = element_text(size = 0)) + geom_vline(xintercept= hour_intervals, linetype="dotted") + geom_vline(xintercept= 144, colour = "red") + geom_text(x=144, y = 150, label="12 pm", angle = 90)

Function to render both plots
<a href="http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)" class="uri">http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)</a>)

    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      library(grid)
      plots <- c(list(...), plotlist)
      numPlots = length(plots)
      if (is.null(layout)) {
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                        ncol = cols, nrow = ceiling(numPlots/cols))
      }
     if (numPlots==1) {
        print(plots[[1]])
      } else {
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        for (i in 1:numPlots) {
          matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
          print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
        }
      }
    }

Create the panel plots and observe any differences in activity patterns
between weekdays and weekends

    multiplot(plot1, plot2, cols = 1)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-24-1.png)
Activity seems to be lower during the weekdays compared to the weekends
