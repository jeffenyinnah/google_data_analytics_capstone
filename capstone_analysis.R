# Install packages
install.packages("tidyverse")
install.packages("lubridate")

# load the libraries
library(tidyverse)
library(lubridate)

# import all the downloaded csv datasets
df1 <- read_csv("data/cyclistic_trip_data_2021_01.csv")
df2 <- read_csv("data/cyclistic_trip_data_2021_02.csv")
df3 <- read_csv("data/cyclistic_trip_data_2021_03.csv")
df4 <- read_csv("data/cyclistic_trip_data_2021_04.csv")
df5 <- read_csv("data/cyclistic_trip_data_2021_05.csv")
df6 <- read_csv("data/cyclistic_trip_data_2021_06.csv")
df7 <- read_csv("data/cyclistic_trip_data_2021_07.csv")
df8 <- read_csv("data/cyclistic_trip_data_2021_08.csv")
df9 <- read_csv("data/cyclistic_trip_data_2021_09.csv")
df10 <- read_csv("data/cyclistic_trip_data_2021_10.csv")
df11 <- read_csv("data/cyclistic_trip_data_2021_11.csv")
df12 <- read_csv("data/cyclistic_trip_data_2021_12.csv")

#merging together all the individual csv files
df_merge <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

#saving the merged dataframe to local storage for backup
write_csv(df_merge, "data/merged_trip.csv")

# importing the merged csv again because of crashing problems   
df_all <- read_csv("data/merged_trip_data.csv")
head(df_all)

# inspecting the new combined dataframe
str(df_all)
summary(df_all)

## add new colunm to calculate the length of rides "rides_length" and calculate weekday "day_of_week"
transformed_df <- df_all %>% 
  mutate(ride_length = as.numeric(ended_at - started_at)) %>% 
  mutate(day_of_week = wday(started_at, label = TRUE, abbr = FALSE, week_start = 1, 
                            locale = Sys.setlocale("LC_ALL", locale = "C"))) %>% 
  mutate(date = as.Date(started_at)) %>% 
  mutate(month = format(as.Date(date), "%m"))

head(transformed_df)

#CLEANING DATA - From the transformed_df dataset we have, there lots of missing values.


#Checking for Missing values
sum(is.na(transformed_df)) #total number of NA is "2710181"

#checking for duplicates
duplicated(transformed_df)
sum(duplicated(transformed_df)) #No duplicate values

#Filtering out data to remove rows that might affect our analysis such as 
# ride_length less than zero (0), 
#trips not starting or ending at a station,


filtered_df <-transformed_df %>% 
  filter(ride_length > 0)%>%
  filter(!is.na(start_station_name)) %>% 
  filter(!is.na(end_station_name)) %>% 
  filter(!is.na(ride_length))
View(filtered_df)

#saving a copy for further analysis
write_csv(filtered_df, "data/cleaned_trip.csv")

#importing saved cleaned dataset
filtered_df <- read_csv("data/cleaned_trip.csv")

# checking the data types of the columns to be sure before calculations
colnames(filtered_df)
str(filtered_df)

# Analyze Phase
##In this phase, to get insights from this dataset i will look at the 
##length of each rides, type of bike, membership type, start and end station for trends. 
head(select(filtered_df, started_at, ended_at, day_of_week, date))

##extract the month and day of each ride and store them in different columns.
filtered_df$day <- format(as.Date(filtered_df$date), "%d") #extracted day of each ride from date
filtered_df$month <- format(as.Date(filtered_df$date), "%m") #month of each rides

##Conduct descriptive analysis on length of each ride
summarise(filtered_df$ride_length)

##calculate average rides for members and casual riders
filtered_df$month <- month(filtered_df$date, label = TRUE, abbr = TRUE) #changing the representation of month from "numeric" to "text like January"
head(filtered_df$month)
avg_rides_members_casual <- filtered_df %>%
  group_by(member_casual) %>%
  summarise(avg_rides = mean(ride_length)) %>% 
  arrange(desc(avg_rides))
head(avg_rides_members_casual)

#comparing members and casual users ride length
mean_score <- aggregate(filtered_df$ride_length ~ filtered_df$member_casual, FUN = mean)
median_Score <- aggregate(filtered_df$ride_length ~ filtered_df$member_casual, FUN = median)
max_score <- aggregate(filtered_df$ride_length ~ filtered_df$member_casual, FUN = max)
min_score <- aggregate(filtered_df$ride_length ~ filtered_df$member_casual, FUN = min)

##calculate average length of rides by users by day of week
avg_rides_length <- filtered_df %>% 
  group_by(member_casual, day_of_week, month, date) %>% 
  summarise(avg_rides = mean(ride_length)) %>% 
  arrange(desc(avg_rides))
View(avg_rides_length)

##calculate number of rides by users by day of week 
num_of_rides <- filtered_df %>%
  group_by(member_casual,day_of_week, month) %>% 
  summarise(number_of_rides = n())
View(num_of_rides)

##############################################################################
# Data Visualization using ggplot2
## Number of Rides by Day of Week
filtered_df %>%
  mutate(day_of_week = wday(started_at, label = TRUE, week_start = 1, 
                            locale = Sys.setlocale("LC_ALL", locale = "C"))) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(num_of_rides = n()) %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = num_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Day of the week", y = "Number of rides",
       title = "Number of Rides by Day of the week")+
  scale_y_continuous(name=expression(num ~ of ~ rides ~ (x10^5)),
                     labels=function(x) x / 100000,
                     limits=c(0,1000000))

## Number of Rides by Month
filtered_df %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(num_of_rides = n()) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x = month, y = num_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Months", y = "Number of rides",
       title = "Number of Rides by Months")+
  scale_y_continuous(name=expression(num ~ of ~ rides ~ (x10^5)),
                     labels=function(x) x / 100000,
                     limits=c(0,700000))


## Ride DUrations by Bike type
## Average Ride lengths by users by Month & Day of week
filtered_df %>%
  mutate(day_of_week = wday(started_at, label = TRUE, week_start = 1, 
                            locale = Sys.setlocale("LC_ALL", locale = "C"))) %>%
  group_by(member_casual, day_of_week) %>% 
  summarise(avg_rides = mean(ride_length)) %>%
  ggplot(aes(x = day_of_week, y = avg_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Day of the week", y = "Average Rides Duration",
       title = "Average Rides by Day of Week")
####################################
filtered_df %>% 
  mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
  group_by(member_casual, month) %>% 
  summarise(avg_rides = mean(ride_length)) %>%
  ggplot(aes(x = month, y = avg_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Months", y = "Average Rides Duration",
       title = "Average Rides by Months ")

## Average ride durations of Bike type
filtered_df %>% 
  group_by(rideable_type) %>% 
  summarise(avg_rides = mean(ride_length)) %>%
  ggplot(aes(x = rideable_type, y = avg_rides, fill = rideable_type)) +
  geom_col(position = "dodge") +
  labs(x = "Bike Types", y = "Average Rides Duration",
       title = "Average Ride durations of Bike Type ")

## Average Ride durations of Bike Types(members vs casual)
filtered_df %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(avg_rides = mean(ride_length)) %>%
  ggplot(aes(x = rideable_type, y = avg_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Bike Types", y = "Average Rides Duration(mins)",
       title = "Average Ride durations of Bike Types(members vs casual)")

## Average ride duration by day of week (bike type)
filtered_df %>%
  mutate(day_of_week = wday(started_at, label = TRUE, week_start = 1, 
                            locale = Sys.setlocale("LC_ALL", locale = "C"))) %>%
  group_by(member_casual, day_of_week, rideable_type) %>% 
  summarise(avg_rides = mean(ride_length)) %>%
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x= day_of_week, y = avg_rides, fill=member_casual)) +
  geom_col(position = "dodge") +
  facet_wrap(~ rideable_type) +
  labs(x = "Day of the week", y = "Average Rides Duration(mins)", 
       title = "Average ride duration by day of week(bike types)", fill="Customer type")


## Average ride duration of users from January 2021 to December 2021
filtered_df %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE, 
                       locale = Sys.setlocale("LC_ALL", locale = "C"))) %>%
  group_by(member_casual, month) %>% 
  summarise(avg_rides = mean(ride_length)) %>%
  ggplot(aes(x = month, y = avg_rides, group = member_casual)) +
  geom_line(aes(color=member_casual)) + geom_point(aes(color=member_casual)) +
  labs(x = "Months", y = "Average Rides Duration(mins)",
       title = "Average Rides Duration(mins) trends(Member vs Casual)")

## Average ride duration using Bike types from January 2021 to December 2021
filtered_df %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE, 
                       locale = Sys.setlocale("LC_ALL", locale = "C"))) %>%
  group_by(rideable_type, month) %>% 
  summarise(avg_rides = mean(ride_length)) %>%
  ggplot(aes(x = month, y = avg_rides, group = rideable_type)) +
  geom_line(aes(color=rideable_type)) + geom_point(aes(color=rideable_type)) +
  labs(x = "Months(Year 2021)", y = "Average Rides Duration(mins)",
       title = "Average Rides Duration(mins) trend(by Bike Types)")









