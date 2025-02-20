library(tidyverse)
library(conflicted)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

colnames(q1_2019)
colnames(q1_2020)

# Rename columns to make them consistent with q1_2020 (as this will be the supposed going-forward table design for Divvy)
(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
))

# Inspecting the data frames
str(q1_2019)
str(q1_2020)

# Convert ride_id and rideable_type to character so that they can stack correctly
q1_2019 <- mutate(q1_2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))

# Combining both datasets
all_trips <- bind_rows(q1_2019, q1_2020)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "tripduration"))

# Inspecting the new table that has been created by combining the datasets.
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
tail(all_trips)
str(all_trips)
summary(all_trips)

# In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

table(all_trips$member_casual)

# Right now, we can only aggregate our data at the ride level. We need to add extra columns such as day, month, year that will allow us to aggregate on broader perspective
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Calculating ride length for each trip in seconds. 
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative. We need to remove this kind of data
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

# Analysis time

# Analysing based on ride_length
summary(all_trips_v2$ride_length) # This will give us Minimum, 1st quarter, Median, Mean, 3rd Quarter and Maximum

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean) # Mean ride length of casual riders and members
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median) # Median ride length of casual riders and members
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max) # Maximum ride length of casual riders and members
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min) # Minimum ride length of casual riders and members

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

# The days of the week are out of order.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday",
                                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
colnames(all_trips_v2)

# analyze ridership data by type and weekday
rides_by_type <- all_trips_v2 %>%
  mutate(weekday = wday(started_at, label=TRUE)) %>% # Creates weekday column
  group_by(member_casual, weekday) %>% # Groups by usertype and weekday
  summarise(
    number_of_rides = n(), # Number of rides
    average_duration = mean(ride_length) # Average duration of rides
  ) %>% 
  arrange(member_casual, weekday) # Sorts by usertype and then by weekday

# analyze ridership data by start location and weekday
rides_by_location <- all_trips_v2 %>%
  mutate(weekday = wday(started_at, label=TRUE, abbr = TRUE)) %>% # Creates weekday column
  group_by(start_station_name, weekday, member_casual) %>% # Groups by starting station and weekday
  summarise(
    number_of_rides = n(), # Number of rides
    .groups = "drop"
  ) %>% 
  arrange(desc(number_of_rides)) # Sort stations by most rides

top_10_station <- rides_by_location %>% 
  group_by(start_station_name) %>% 
  summarise(total_rides = sum(number_of_rides)) %>% 
  top_n(10, total_rides) %>% 
  arrange(desc(total_rides))

rides_by_top_10_stations <- rides_by_location %>% 
  filter(start_station_name %in% top_10_station$start_station_name)
  
# Visualization time

# Number of rides by rider type
rides_by_type %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + 
  geom_col(position = 'dodge') +
  labs(title="Number of rides on Weekdays")

# Average duration
rides_by_type %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + 
  geom_col(position = 'dodge') +
  labs(title="Average duration of rides")

# Visualization of top 10 starting stations by weekday
rides_by_top_10_stations %>%  
  ggplot(aes(x = reorder(start_station_name, number_of_rides), y = number_of_rides, fill = member_casual)) + 
  geom_col(position = 'dodge') + 
  facet_wrap(~ weekday) +  # Separate charts by weekday
  coord_flip() +  # Rotate for readability
  labs(title = "Top 10 Stations with the Most Rides by Weekday",
       x = "Start Station",
       y = "Number of Rides",
       fill = "User Type") +
  theme_minimal()

# Exporting the data
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual +
                      all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')
