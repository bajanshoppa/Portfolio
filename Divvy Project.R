### Divvy_Exercise_analysis 20219-2020###

install.packages("magrittr")

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(magrittr)
library(dplyr)

getwd()
setwd("/Users/shoppa/Documents/Documents Old Mac/Kerry's Docs/_Code/R/Porfolio/Divvy_data")
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

colnames(q4_2019)
colnames(q3_2019)
colnames(q2_2019)
colnames(q1_2020)

q4_2019 <- rename(q4_2019, 
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_name = from_station_name,
                  start_station_id = from_station_id,
                  end_staion_name = to_station_name,
                  end_station_id = to_station_id,
                  member_casual = usertype)

q3_2019 <- rename(q3_2019, 
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_name = from_station_name,
                  start_station_id = from_station_id,
                  end_staion_name = to_station_name,
                  end_station_id = to_station_id,
                  member_casual = usertype)

q2_2019 <- rename(q2_2019, 
                  ride_id = "01 - Rental Details Rental ID",
                  rideable_type = "01 - Rental Details Bike ID",
                  started_at = "01 - Rental Details Local Start Time",
                  ended_at = "01 - Rental Details Local End Time",
                  start_station_name = "03 - Rental Start Station Name",
                  start_station_id = "03 - Rental Start Station ID",
                  end_staion_name = "02 - Rental End Station Name",
                  end_station_id = "02 - Rental End Station ID",
                  member_casual = "User Type")

str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

q4_2019 <- mutate(q4_2019,ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

q3_2019 <- mutate(q3_2019,ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

q2_2019 <- mutate(q2_2019,ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

#Removing fields that were dropped in 2020
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear,
            gender, "01 - Rental Details Duration In Seconds Uncapped", 
            "05 - Member Details Member Birthday Year", "Member Gender",
            "tripduration"))
colnames(all_trips)
head(all_trips)
nrow(all_trips)
str(all_trips)
summary(all_trips)

table(all_trips$member_casual)

all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual,
                                "Subscriber"="member",
                                "Customer"="casual"))

table(all_trips$member_casual)

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

str(all_trips)

#Converting "ride_length" to numeric to enable calculations
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#Version control of dataframe as data will be removed
all_trips_v2<-all_trips[!(all_trips$start_station_name == "HQ QR" | 
                            all_trips$ride_length<0),]

# Descriptive analysis on ride_length (in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

summary(all_trips_v2$ride_length)

##Comparing Members and Casual Users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

#Average ride time per day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual 
          + all_trips_v2$day_of_week, FUN = mean)

#Fixing Day order 
all_trips_v2$day_of_week <- 
  ordered(all_trips_v2$day_of_week, 
          levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual 
          + all_trips_v2$day_of_week, FUN = mean)
#Analyze data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)
  
#Create a visualization of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#Create visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

