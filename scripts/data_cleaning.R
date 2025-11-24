# Data cleaning Process

# install and import packages

install.packages("tidyverse")
install.packages("dplyr")
install.packages("lubridate")

library("tidyverse")
library("dplyr")
library("lubridate")

# Pull in the data

trips_2019_raw <- read.csv("data_raw/Divvy_Trips_2019_Q1.csv")
trips_2020_raw <- read.csv("data_raw/Divvy_Trips_2020_Q1.csv")

str(trips_2019_raw)
str(trips_2020_raw)

# Create copies of data for cleaning

trips_2019_working <- read.csv("data_raw/Divvy_Trips_2019_Q1.csv")
trips_2020_working <- read.csv("data_raw/Divvy_Trips_2020_Q1.csv")

# Begin cleaning

# Remove nulls in start times and end timesfor both dataframes

# 2019
trips_2019_working <- trips_2019_working %>% 
  subset(!is.na(start_time))

trips_2019_working <- trips_2019_working %>% 
  subset(!is.na(end_time))

# 2020
trips_2020_working <- trips_2020_working %>% 
  subset(!is.na(started_at))

trips_2020_working <- trips_2020_working %>% 
  subset(!is.na(ended_at))

# Remove nulls in user types

trips_2019_working <- trips_2019_working %>% 
  subset(!is.na(usertype))

trips_2020_working <- trips_2020_working %>% 
  subset(!is.na(member_casual))

# Remove nulls when there is no information about Start location or End location (name AND ID)

# 2019
trips_2019_working <- trips_2019_working %>% 
  subset(!(is.na(from_station_name) & is.na(from_station_id)))

trips_2019_working <- trips_2019_working %>% 
  subset(!(is.na(to_station_name) & is.na(to_station_id)))

# 2020
trips_2020_working <- trips_2020_working %>% 
  subset(!(is.na(start_station_name) & is.na(start_station_id)))

trips_2020_working <- trips_2020_working %>% 
  subset(!(is.na(end_station_name) & is.na(end_station_id)))

# Standardize the column names and data

str(trips_2019_working)
str(trips_2020_working)

trips_2019_working <- trips_2019_working %>% 
  mutate(
    ride_id = as.character(trip_id),
    start_station_name = from_station_name,
    start_station_id = from_station_id,
    end_station_name = to_station_name,
    end_station_id = to_station_id,
    start_time = as_datetime(start_time, format="%Y-%m-%d %H:%M:%S"),
    end_time = as_datetime(end_time, format="%Y-%m-%d %H:%M:%S"),
    usertype = ifelse(usertype == "Subscriber", "member", "casual")
  )

str(trips_2019_working)

trips_2020_working <- trips_2020_working %>% 
  mutate(
    start_time = as_datetime(started_at, format="%Y-%m-%d %H:%M:%S"),
    end_time = as_datetime(ended_at, format="%Y-%m-%d %H:%M:%S"),
    usertype = member_casual
  )

str(trips_2020_working)

# Create new columns for ride_length and day_of_week

trips_2019_working <- trips_2019_working %>% 
  mutate(
    ride_length = difftime(end_time, start_time, units="secs"),
    day_of_week = weekdays(start_time)
  )

str(trips_2019_working)

trips_2020_working <- trips_2020_working %>% 
  mutate(
    ride_length = difftime(end_time, start_time, units="secs"),
    day_of_week = weekdays(start_time)
  )

str(trips_2020_working)

# Remove rows with default data locations and times

trips_2019_working <- trips_2019_working[!(trips_2019_working$start_station_name == "HQ QR" | trips_2019_working$ride_length < 0),]
trips_2020_working <- trips_2020_working[!(trips_2020_working$start_station_name == "HQ QR" | trips_2020_working$ride_length < 0),]


# Select only necessary rows for analysis

trips_2019_cleaned <- trips_2019_working %>% 
  select(
    ride_id,
    start_time, 
    end_time, 
    start_station_name, 
    start_station_id, 
    end_station_name, 
    end_station_id, 
    usertype, 
    ride_length, 
    day_of_week
  )

trips_2020_cleaned <- trips_2020_working %>% 
  select(
    ride_id,
    start_time, 
    end_time, 
    start_station_name, 
    start_station_id, 
    end_station_name, 
    end_station_id, 
    usertype, 
    ride_length, 
    day_of_week
  )

# combine all rows

all_trips_cleaned <- bind_rows(trips_2019_cleaned, trips_2020_cleaned)
str(all_trips_cleaned)

# Save cleaned data as CSVs

write.csv(trips_2019_cleaned, "data_clean/data_cleaned_2019.csv", row.names=FALSE)

write.csv(trips_2020_cleaned, "data_clean/data_cleaned_2020.csv", row.names=FALSE)

write.csv(all_trips_cleaned, "data_clean/data_cleaned_all.csv", row.names=FALSE)
