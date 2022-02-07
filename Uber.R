
##Working Directory

setwd('C:\\Users\\Larry Williams\\Desktop\\Mamata\\Edvancer\\data\\data\\UberData')
getwd()

##Importing required libraries

library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

##Creating vector of colors that can be used in our plots

colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

## Reading uber data from local file

apr_data = read.csv("apr14.csv")
may_data = read.csv("may14.csv")
june_data = read.csv("jun14.csv")
july_data = read.csv("jul14.csv")
aug_data = read.csv("aug14.csv")
sep_data = read.csv("sep14.csv")

## Binding all data as one dataset

data= rbind(apr_data, may_data, june_data, july_data, aug_data, sep_data)
View(data)

## Converting Date.Time variable from character to calender date

data$Date.Time = as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S")
data$Time = format(data$Date.Time, format="%H:%M:%S")
#data$Date.Time <- ymd_hms(data$Date.Time)

data$day = factor(day(data$Date.Time))
data$month = factor(month(data$Date.Time, label = TRUE))
data$year = factor(year(data$Date.Time))
data$dayofweek = factor(wday(data$Date.Time, label = TRUE))


data$hour = factor(hour(hms(data$Time)))
data$minute = factor(minute(hms(data$Time)))
data$second = factor(second(hms(data$Time)))

## To see the trip count for each hour 
hour_data = data %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 
datatable(hour_data)

##Plot

ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

##To see the trip for each hour month wise

month_hour <- data %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())
datatable(month_hour)

ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)

##Plotting data by trips during every day of the month

day_group <- data %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 
datatable(day_group)

ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

day_month_group <- data %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())
datatable(day_month_group)

ggplot(day_month_group, aes(day, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

##Number of Trips taking place during months in a year

month_group <- data %>%
  group_by(month) %>%
  dplyr::summarize(Total = n()) 
datatable(month_group)

ggplot( month_group, aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

##Trips by days and months

month_weekday <- data %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())
datatable(month_weekday)


ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

##Finding out the number of Trips by bases

ggplot(data, aes(Base)) + 
  geom_bar(fill = "darkred") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

##Trips by bases and month

ggplot(data, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colors)

##Heatmap

day_and_hour <- data %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())
datatable(day_and_hour)

ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")

ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day")

ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")