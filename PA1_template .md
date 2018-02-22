---
output:
  html_document: default
  pdf_document: default
---
# Reproducible Research

## Peer-graded Assignment: Course Project 1


## Load libraries
```{r}
library(dplyr)
library(ggplot2)
```

## Loading and preprocessing the data

**_Load data_**

```{r}
activity <- choose.files() %>%
        unzip(files = "activity.csv") %>%
        read.csv(header = TRUE,
                 sep = ",",
                 stringsAsFactors = FALSE)
```


## What is mean total number of steps taken per day?

**_1. Calculate the total number of steps per day_**


```{r}
sum_steps <- with(activity, tapply(steps, date, sum, na.rm = TRUE))

lv <- as.factor(names(sum_steps))
dataFrame <- data.frame(date = lv, sum_steps = as.vector(sum_steps))

with(dataFrame, hist(sum_steps, col = "red"))

dev.copy(png,"plot1.png")

dev.off()
```

**_2. Calculate  the mean and median of the total number of steps taken per day_** 

```{r}


mean_steps <- mean(sum_steps)

median_steps <- median(sum_steps)
```
- Mean :`r mean_steps`

- Median : `r median_steps` 

## What is the average daily activity pattern?

**_1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)_**
```{r}
average_per_interval <-
        aggregate(activity$steps, activity["interval"], mean, na.rm = TRUE)

gpl <-  ggplot(data = average_per_interval, mapping = aes(x = interval, y = x)) +
        geom_line(colour = "red") + ylab("average of number steps")

print (gpl)

dev.copy(png, "average_steps_per_interval.png")

dev.off()

```
**_2. Find 5-minute interval, which on average across all the days in the dataset, contains the maximum number of steps_**
``` {r, echo = FALSE}
with(average_per_interval, interval[which.max(x)])

```
## Imputing missing values

### Summary entire colums of the dataset
``` {r}
lapply(activity, summary)
```
**_1. Total missing values in the dataset_**
```{r}
sum(sapply(activity, is.na))

```
**_2. Fill in entire missing values in the dataset (only in steps column)_**
```{r}
mean_steps <- with(activity, mean(steps, na.rm = TRUE))

activity_new <- activity

activity_new$steps[is.na(activity_new$steps)] <- mean_steps
```

**_3. Histogram of the total number of steps taken each day_**

```{r}
new_sum_steps <-
        aggregate(activity_new$steps, by = activity_new["date"], sum)

hist (new_sum_steps$x, xlab = "sum_steps_without_NA", col = "red", main = "Histogram of the total of number steps with out missing values")

```

**_4. The mean and median total number of steps taken per day_**

```{r}
new_mean_steps <- mean(new_sum_steps$x)

new_median_steps <- median(new_sum_steps$x)

```
- Mean :`r new_mean_steps`

- Median : `r new_median_steps` 

## Differences in activity patterns between weekdays and weekends

```{r}
activity_new$dateType <-
        ifelse(as.POSIXlt(activity_new$date)$wday %in% c(1, 5),
               "weekday",
               "weekend")

new_average_steps_per_interval <- aggregate(activity_new$steps, c(activity_new["interval"],activity_new["dateType"]),mean)

p <- ggplot(new_average_steps_per_interval, aes(interval, x)) + geom_line(colour = "red") + ylab("steps") + ggtitle("Average of number steps by weekend and weekday")

p + facet_grid(dateType ~ .) 

```