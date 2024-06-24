# Load necessary libraries
library(dplyr)
library(lubridate)

# Data Cleaning

# 1. Remove rows and columns with NA values
data_cleaned <- data %>%
  na.omit()

# 2. Remove duplicate rows
data_cleaned <- data_cleaned %>%
  distinct()

# 3. Remove unnecessary columns (delete start_lat, start_lng, end_lat, end_lng)
data_cleaned <- data_cleaned %>%
  select(-start_lat, -start_lng, -end_lat, -end_lng)

# 4. Create new columns for analysis
data_cleaned <- data_cleaned %>%
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at),
    start_hour = hour(started_at),
    month = month(started_at),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall"
    ),
    weekday = wday(started_at, label = TRUE),
    trip_duration = as.numeric(difftime(ended_at, started_at, units = "secs"))
  )

# 5. Remove rows with negative trip duration
data_cleaned <- data_cleaned %>%
  filter(trip_duration >= 0)

# View the cleaned dataframe
print(data_cleaned)

Explanation:
Remove rows and columns with NA values: na.omit() is used to remove any rows that contain NA values.
Remove duplicate rows: distinct() is used to remove any duplicate rows from the dataframe.
Remove unnecessary columns: select(-start_lat, -start_lng, -end_lat, -end_lng) removes the specified columns.
Create new columns for analysis:
ymd_hms() converts the datetime columns to POSIXct format.
hour(), month(), wday() extract specific parts of the datetime for new columns.
case_when() assigns seasons based on the month value.
trip_duration is calculated as the difference between ended_at and started_at in seconds.
Remove rows with negative trip duration: filter(trip_duration >= 0) keeps only rows with non-negative trip durations.
This code snippet processes the data according to the steps you provided. Make sure to adjust the dataframe name if it differs from data.

Visualization 
install.packages("ggplot2")
library(ggplot2)

# Number of Trips by Weekdays
ggplot(data_cleaned, aes(x = weekday)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Number of Trips by Weekday", x = "Weekday", y = "Number of Trips")

# Calculate average trip duration by hour
avg_duration_by_hour <- data_cleaned %>%
  group_by(start_hour) %>%
  summarise(avg_duration = mean(trip_duration))

# Print the result
print(avg_duration_by_hour)

# Plot average trip duration by hour
ggplot(avg_duration_by_hour, aes(x = start_hour, y = avg_duration)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Average Trip Duration by Hour", x = "Hour of Day", y = "Average Trip Duration (seconds)") +
  theme_minimal()

  # Calculate total trips by member type
trips_by_member_type <- data_cleaned %>%
  group_by(member_casual) %>%
  summarise(total_trips = n())

print(trips_by_member_type)

# Create a bar plot for total trips by member type
ggplot(trips_by_member_type, aes(x = member_casual, y = total_trips, fill = member_casual)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Total Trips by Member Type", x = "Member Type", y = "Total Trips") +
  theme_minimal() +
  theme(legend.position = "none")

  # Calculate total trips by member type
trips_by_member_type <- data_cleaned %>%
  group_by(member_casual) %>%
  summarise(total_trips = n())

# Print the result
print(trips_by_member_type)

# Create a bar plot for total trips by member type
ggplot(trips_by_member_type, aes(x = member_casual, y = total_trips, fill = member_casual)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Total Trips by Member Type", x = "Member Type", y = "Total Trips") +
  theme_minimal() +
  theme(legend.position = "none")


  # Aggregate trip count by member type
trip_count <- data_cleaned %>%
  group_by(member_casual) %>%
  summarise(total_trips = n())

# Bar chart for trip frequency by member type
ggplot(trip_count, aes(x = member_casual, y = total_trips, fill = member_casual)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Trips by Member Type", x = "Member Type", y = "Total Trips") +
  theme_minimal() +
  theme(legend.position = "none")

  # Calculate average trips per hour by member type
avg_trips_by_hour <- data_cleaned %>%
  group_by(member_casual, start_hour) %>%
  summarise(avg_trips = mean(trip_duration))

# Line plot for usage patterns by hour of day
ggplot(avg_trips_by_hour, aes(x = start_hour, y = avg_trips, color = member_casual)) +
  geom_line() +
  labs(title = "Average Trip Duration by Hour of Day", x = "Hour of Day", y = "Average Trip Duration (seconds)") +
  theme_minimal()

  # Calculate average trips per weekday by member type
avg_trips_by_weekday <- data_cleaned %>%
  group_by(member_casual, weekday) %>%
  summarise(avg_trips = mean(trip_duration))

# Bar chart for usage patterns by day of week
ggplot(avg_trips_by_weekday, aes(x = weekday, y = avg_trips, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Trip Duration by Day of Week", x = "Day of Week", y = "Average Trip Duration (seconds)") +
  theme_minimal() +
  scale_x_discrete(labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

  # Calculate total trips by season and member type
total_trips_by_season <- data_cleaned %>%
  group_by(season, member_casual) %>%
  summarise(total_trips = n())

# Stacked area chart for usage patterns by season
ggplot(total_trips_by_season, aes(x = season, y = total_trips, fill = member_casual)) +
  geom_area(position = "stack") +
  labs(title = "Total Trips by Season and Member Type", x = "Season", y = "Total Trips") +
  theme_minimal()
