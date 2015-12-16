#####
#check for file in current director, download 
if(!file.exists("repdata-data-activity.zip")) {
  temp <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp)
  file <- unzip(temp)
  unlink(temp)}
  file <- unzip("repdata-data-activity.zip")

#####
#add libraries
require(dplyr)
require(ggplot2)
require(data.table) 

#####
#import table 
activity <- read.csv(file, header=TRUE)
data <- activity

#####
#explore
names(data)
head(data)
str(data)
data$interval <- as.factor(data$interval)
sum(is.na(data$steps))
summary(data)

#remove na's from table
data2 <- data %>%
  filter(!is.na(steps))
summary(data2)
barplot(data2$steps)

#####
#Step 2 - total steps per day
dateTotal <- data %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps))

mean1 <- round(mean(dateTotal$steps), 0)
median1 <- median(dateTotal$steps)

ggplot(dateTotal, aes(x = steps)) +
  geom_histogram(fill = "#3399FF", binwidth = 1000) +
  labs(title = "Distribution of Total Steps per Day with NA's Removed", 
       x = "Total steps per day", y = "Frequency")

#####
#Step 3 - make a time series plot showing steps per time frame 

timeTotal <- data2 %>% 
              group_by(interval) %>%
              summarise(avg = mean(steps))
head(timeTotal)
  #get highest average steps in a time frame
timeTotal %>%
  top_n(n=2)

# change timeTotal$interval to numeric so the x axis of the plot can be manipulated 
timeTotalasNum <- timeTotal
timeTotalasNum$interval <- as.numeric(timeTotalasNum$interval)

ggplot(timeTotalasNum, aes(x = interval, y = avg))+
  geom_line(aes(group=1), colour = "#3399FF") +     # Group all points; otherwise no line will show
  scale_x_continuous(breaks=c(seq(from=0, to=288, by=12)),
                     labels=c(seq(from=0, to=288, by=12))) +
  geom_point(size=2) +
  labs(title = "The Distribution of Total Steps per Time Interval", 
       x = "Total steps per time interval", y = "Frequency")
  

max(timeTotal$avg)

#####
#Step 4 - missing values
  #number of missing values
sum(is.na(data$steps))

#replace missing values with average value for given time period 
dt1<- data.table(data)
dt2 <- data.table(timeTotal)
dt1[is.na(steps), steps:= dt2[copy(.SD), avg, on="interval"]]


#calcuate steps per day
dt3 <- dt1[, sum(steps), by = date]
colnames(dt3)[2] <- "steps"
mean2 <- round(mean(dt3$steps), 0)
median2 <- median(dt3$steps)

ggplot(dt3, aes(x = steps)) +
  geom_histogram(fill = "#3399FF", binwidth = 1000)+
  labs(title = "Distribution of Total Steps per Day with NA's Removed", 
       x = "Total steps per day", y = "Frequency")

labels <- c("NA's included", "NAs replaced", "Difference")
mean_diff <- paste(round(((mean1/mean2 - 1)*100), 2), "%", sep="")
median_diff <- paste(round(((median1/median2 - 1)*100), 2),"%", sep="")
means <- c(mean1, mean2, mean_diff)
medians <- c(median1, median2, median_diff)
comparison <- table(c(means, medians))

data.frame(labels, means, medians)


#Step 5 - compare weekdays to weekends
dt1$date<- as.Date(dt1$date)

df4 <- mutate(dt1, weektype = ifelse(weekdays(date) == "Saturday" 
                                         | weekdays(date) == "Sunday", 
                                         "weekend", "weekday"))

#####  ggplot chart
df4$weektype <- as.factor(dt4$weektype)
head(df4)

df5 <- df4 %>%
  group_by(interval, weektype) %>%
  summarise(steps = mean(steps))
s <- ggplot(df5, aes(x = interval, y = steps, color = weektype)) +
      geom_line() +
      facet_wrap(~weektype, ncol = 1, nrow=2)
print(s)

# Alternative plot 
t <- ggplot(df5, aes(x = as.integer(interval), y = steps, color = weektype)) +
  geom_line() +
  scale_x_continuous(limits = c(0, 288),
                     breaks = c(seq(from=0, to=288, by=12)),
                     labels = c(seq(from=0, to=288, by=12))) +
  labs(title = "Comparison of Mean Steps per Interval by Weekdays and Weekends",
       x = "Interval", "Number of Steps", legend = "Weektype")
print(t)

