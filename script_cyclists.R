library(tidyverse) #import libraries
library(ggplot2)
library(lubridate)
#setwd("YourDirectory") # set for your file directory if not set already
m1<-read_csv('202201-divvy-tripdata.csv') #read files, repeat for all csv files
colnames(m1) #check column names
str(m1) #check basic info of datasets
m1 <-  mutate(m1, ride_id = as.character(ride_id))#change datatypes to readable ones, repeat all datasets
#dataset containing data form August has different data types in columns started_at and ended_at
m8 <-  mutate(m8, started_at = as.POSIXct(started_at),ended_at = as.POSIXct(ended_at)) 

q1_trips<-bind_rows(m1, m2, m3, m4) #repeat for all quaters, can merge all data without this step
#after that merge all quaters into one dataset, for example all_trips
#basic information of the dataset
dim(all_trips)
summary(all_trips)
table(all_trips$member_casual)
all_trips$date <- as.Date(all_trips$started_at)#add date column
all_trips$month <- format(as.Date(all_trips$date), "%m") #add month column
all_trips$day <- format(as.Date(all_trips$date), "%d") #add day column
all_trips$year <- format(as.Date(all_trips$date), "%Y") #add year column
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A") #add day of week column
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at) #add lenth of trip column
str(all_trips) #verify changes

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length)) #change values of ride lenth to numeric
is.numeric(all_trips$ride_length) #confirm if it changed
#remove all rows with negative ride_lenth value
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
#remove all rows with NA values
all_trips_v2<-na.omit(all_trips_v2)
#summarize data about casual and member users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
#order weekdays in proper order
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
#display data based on day of week
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
#analyze trips by type and data
all_trips_v2 %>% 
      mutate(weekday = wday(started_at, label = TRUE)) %>%
       group_by(member_casual, weekday) %>%
       summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% arrange(member_casual, weekday)

#visualize trips by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#visualize average duration of trips
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
