# Case study on Chicago Divvy Bike Share

# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  

```{r Loading required packages}
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("C:/Users/k96su/Documents/R/Case Study/Cyclistic Bike-Share") #sets your working directory 
# to simplify calls to data ...)
```

#=====================
# STEP 1: COLLECT DATA
#=====================
```{r Upload Divvy datasets (csv files) here}
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")
```

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match 
# perfectly before we can use a command to join them into one file
```{r}
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)
```
# Rename columns  to make them consistent with q1_2020
```{r}
(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))
```
# Inspect the data frames and look for any inconsistencies
```{r}
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)
```
# Convert ride_id and rideable_type to character so that they can merge correctly
```{r}
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
```
# Merge individual quarter's data frames into one big data frame
```{r}
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)
```
# check last 6 rows added in the merged data frame and preview data
```{r}
tail(all_trips)
# preview new data frame
View(all_trips)
```
# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
```{r}
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,
            "01 - Rental Details Duration In Seconds Uncapped", 
            "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))
```
#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
```{r  Inspect the new table that has been created}
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics
```
# There are a few problems identified and need to be fixed:
# (1) In the "member_casual" column, there are two names for members ("member" 
# and "Subscriber") and two names for casual riders ("Customer" and "casual"). 
# This needs to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. 
# SO need to add some additional columns of data -- such as day, month, 
# year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the q1_2020 data 
# did not have the "tripduration" column. We will add "ride_length" to the entire data frame for consistency.
# (4) There are some rides where tripduration shows up as negative, including 
# several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders.
# Need to make the dataframe consistent with their current nomenclature
# Note: "Level" is a special property of a column that is retained even if a subset does not contain any values from a 
# specific level
# Begin by seeing how many observations fall under each usertype
```{r Checking observations count}
table(all_trips$member_casual)
```
```{r Reassign to the desired values (we will go with the current 2020 labels)}
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))
```
```{r Check to make sure the proper number of observations were reassigned}
 
table(all_trips$member_casual)
```
# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations 
# we could only aggregate at the ride level
# Followed links http://lubridate.tidyverse.org/ and https://www.statmethods.net/input/dates.html more on date formats in R 
```{r}
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
```
# Add a "ride_length" calculation to all_trips (in seconds)
# Followed this link for calculating time-difference  https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
```{r ride_length calculation}
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
```
```{r Inspect the structure of the columns}
str(all_trips)
```
```{r Convert "ride_length" from Factor to numeric so we can run calculations on the data}
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
```
```{r  Remove duplicate fields}
all_trips %>%
  distinct()
```
```{r Remove missing and empty fields}
 all_trips[complete.cases(all_trips),]
```
# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or 
# ride_length was negative, below code gives  aa preview on the same
```{r Remove irrelevant data}
all_trips[(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),] 
```
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
```{r}
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
```
#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
```{r  Descriptive analysis on ride_length (all figures in seconds)}
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

```

# To condense the four lines above to one line using summary() on the specific attribute
```{r Summarize the data}
summary(all_trips_v2$ride_length)
```
```{r Compare members and casual users}
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
```

# See the average ride time by each day for members vs casual users
```{r Compare Average ride between Casual and Members}
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
```

# Notice that the days of the week are out of order. Let's fix that.
```{r Arranging weekdays}
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```

# Now, let's run the average ride time by each day for members vs casual users
```{r Average Ride time each day Casual vs Members}
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
```
# Checking column names
```{r}
colnames(all_trips_v2)
```

# analyze ridership data by type and weekday
```{r}
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts
```

# Let's visualize the number of rides by rider type
```{r}
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Total Rides of Member vs Casual ", 
       subtitle = "On Weekdays over the last 12 months",
       caption = "Data Collected by Motivate International Inc.")
```
# This graph shows how many rides taken by customers annually on each day
![Total Rides over last 12 months](https://user-images.githubusercontent.com/74862660/117622045-d95b2180-b18f-11eb-9fc4-e48db63985c9.png)

# Let's create a visualization for average duration
```{r}
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = format(n(), scientific = FALSE) # use of format to convert scientific value of rides to numeric
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Average Trip Duration of Member vs Casual ", 
                    subtitle = "On Weekdays over the last 12 months",
                    caption = "Data Collected by Motivate International Inc.")
```
# This graph shows that 23% of customers who are casual riders have high average trip duration annually everyday than the annual member's average trip duration.

![image](https://user-images.githubusercontent.com/74862660/117642456-eb938a80-b1a4-11eb-9bd3-9282929e535e.png)

![Average Trip duration last 12 months](https://user-images.githubusercontent.com/74862660/117622074-e11ac600-b18f-11eb-9436-b4da978b7996.png)

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that will visualize in Excel, Tableau, or presentation software
# Note : This file location is for a PC. If you are working on diferent platform, change the file location accordingly
# (most likely "C:\Users\YOUR_USERNAME\Desktop\...")
# to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
```{r}
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = "C:/Users/k96su/Documents/R/Case Study/Cyclistic Bike-Share/avg_ride_length.csv") 
```
