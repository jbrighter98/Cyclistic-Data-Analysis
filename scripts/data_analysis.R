# Note: Make sure to run the data cleaning script before running this

# Performing data analysis on cleaned data

# install libraries

install.packages("tidyverse")

library("tidyverse")
library(scales)

# Read cleaned CSVs

all_trips <- read.csv("data_clean/data_cleaned_all.csv")

# Analyzing ride_length

summary(all_trips$ride_length)

# Comparing ride length between members and casual users

aggregate(all_trips$ride_length ~ all_trips$usertype, FUN = mean)
aggregate(all_trips$ride_length ~ all_trips$usertype, FUN = median)
aggregate(all_trips$ride_length ~ all_trips$usertype, FUN = max)
aggregate(all_trips$ride_length ~ all_trips$usertype, FUN = min)

# Notice casual users have much longer ride_lengths than members

# Aggregate by day of week, user, and ride_length

all_trips$day_of_week <- ordered(all_trips$day_of_week, levels=c(
  "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips$ride_length ~ all_trips$usertype + all_trips$day_of_week, FUN = mean)

# analyze total rides and average duration per day

all_trips %>%
  group_by(usertype, day_of_week) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% #calculates the number of rides and average duration
  arrange(usertype, day_of_week)

# Visualize the number of rides by rider type

all_trips %>%
  group_by(usertype, day_of_week) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(usertype, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = usertype)) + geom_col(position = "dodge")

# Members ride significantly more often in the middle of the week, most likely due to commuting for work

# Create a visualization for average duration

all_trips %>%
  group_by(usertype, day_of_week) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(usertype, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = average_duration, fill = usertype)) + geom_col(position = "dodge")

# casual users ride significantly more on Thursdays and slightly more on Fridays than the rest of the week
# This might be because of an outlier. Lets check the median:

all_trips %>%
  group_by(usertype, day_of_week) %>%
  summarise(number_of_rides = n(), average_duration = median(ride_length)) %>%
  arrange(usertype, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = average_duration, fill = usertype)) + geom_col(position = "dodge")

# The median gives a much more coherent graph and results: will use the median for making conclusions

# Notice casual users have much longer ride lengths than members on average, but there are more member rides

all_trips %>% 
  group_by(usertype) %>% 
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(x = usertype, y = number_of_rides, fill = usertype)) + geom_col(position = "dodge") +
  labs(title="Number of Rides vs. User Type", subtitle="Count of total number of rides for casual riders and members", 
       caption= "Data gathered from Divvy Bikes", x="User Type", y="Number of Rides") +
  scale_fill_manual("User Type", values=c("member"="#1E88E5", "casual"="#DEB53B"))

all_trips %>% 
  group_by(usertype) %>% 
  summarise(sum_ride_length = sum(ride_length), average_duration = median(ride_length)) %>% 
  ggplot(aes(x = usertype, y = average_duration, fill = usertype)) + geom_col(position = "dodge") +
  labs(title="Median Ride Duration vs. User Type", subtitle="Median duration of rides for casual riders and members", 
       caption= "Data gathered from Divvy Bikes", x="User Type", y="Median Ride Duration") +
  scale_fill_manual("User Type", values=c("member"="#1E88E5", "casual"="#DEB53B"))

# There are over 10x more trips by the members than the casual users
# but casual user rides almost 3x longer on average


#######
# Key Findings

# Members take ~10× more rides than casual users.
# Casual riders take ~3× longer rides.
# Members ride mostly during weekdays (likely commuting).
# Casual riders ride more on weekends (likely leisure/tourism).
# Casual rider trip durations spike on Wednesdays and weekends.

#######

# Create better bar graphs 

summarized_data <- all_trips %>%
  group_by(usertype, day_of_week) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(usertype, day_of_week)
 
ggplot(summarized_data, aes(x = day_of_week, y = number_of_rides, fill = usertype)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual("User Type", values=c("member"="#1E88E5", "casual"="#DEB53B")) +
  scale_y_discrete(labels = label_comma()) +
  labs(title="Number of Rides per Day of the Week", subtitle="Casual Rider vs. Member", 
       caption= "Data gathered from Divvy Bikes", x="Day of the Week", y="Number of Rides") +
  annotate("text", x=1.5, y=120000, label="Members use bikes\nmore on the weekdays", fontface=2, size=4) +
  annotate("text", x=6.5, y=30000, label="Casual riders use bikes\nmore on the weekends", fontface=2, size=4)

# Getting graph with mean to show the outlier

ggplot(summarized_data, aes(x = day_of_week, y = average_duration, fill = usertype)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual("User Type", values=c("member"="#1E88E5", "casual"="#DEB53B")) +
  scale_y_discrete(labels = label_comma()) +
  labs(title="Average ride length per Day of the Week", subtitle="Casual Rider vs. Member", 
       caption= "Data gathered from Divvy Bikes", x="Day of the Week", y="Average Ride Duration") +
  annotate("text", x=3.5, y=7000, label="Significanly longer\ntrips on Thursday", fontface=2, size=4)

# Changing to median to do analysis

summarized_data_median <- all_trips %>%
  group_by(usertype, day_of_week) %>%
  summarise(number_of_rides = n(), average_duration = median(ride_length)) %>%
  arrange(usertype, day_of_week)

ggplot(summarized_data_median, aes(x = day_of_week, y = average_duration, fill = usertype)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual("User Type", values=c("member"="#1E88E5", "casual"="#DEB53B")) +
  scale_y_discrete(labels = label_comma()) +
  labs(title="Median ride length per Day of the Week", subtitle="Casual Rider vs. Member", 
       caption= "Data gathered from Divvy Bikes", x="Day of the Week", y="Median Ride Duration") +
  annotate("text", x=5.5, y=1550, label="Casual riders take longer\ntrips on Wednesdays and the weekends", fontface=2, size=4)
