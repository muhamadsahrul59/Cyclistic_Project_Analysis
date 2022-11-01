install.packages("tidyverse")
library("tidyverse")
install.packages("janitor")
library("janitor")
install.packages("lubridate")
library("lubridate")
install.packages("ggmap")
library("ggmap")
install.packages("geosphere")
library("geosphere")
install.packages("dplyr")
library("dplyr")
install.packages("ggplot2")
library("ggplot2")

sep21 <- read.csv("C:/Users/msahr/OneDrive/Documents/sertifikat_google/divvytripdata/202109-divvy-tripdata.csv")
oct21 <- read.csv("C:/Users/msahr/OneDrive/Documents/sertifikat_google/divvytripdata/202110-divvy-tripdata.csv")
nov21 <- read.csv("C:/Users/msahr/OneDrive/Documents/sertifikat_google/divvytripdata/202111-divvy-tripdata.csv")
dec21 <- read.csv("C:/Users/msahr/OneDrive/Documents/sertifikat_google/divvytripdata/202112-divvy-tripdata.csv")
jan22 <- read.csv("C:/Users/msahr/OneDrive/Documents/sertifikat_google/divvytripdata/202201-divvy-tripdata.csv")
feb22 <- read.csv("C:/Users/msahr/OneDrive/Documents/sertifikat_google/divvytripdata/202202-divvy-tripdata.csv")
mar22 <- read.csv("C:/Users/msahr/OneDrive/Documents/sertifikat_google/divvytripdata/202203-divvy-tripdata.csv")
apr22 <- read.csv("C:/Users/msahr/OneDrive/Documents/sertifikat_google/divvytripdata/202204-divvy-tripdata.csv")
may22 <- read.csv("C:/Users/msahr/OneDrive/Documents/sertifikat_google/divvytripdata/202205-divvy-tripdata.csv")
jun22 <- read.csv("C:/Users/msahr/OneDrive/Documents/sertifikat_google/divvytripdata/202206-divvy-tripdata.csv")
jul22 <- read.csv("C:/Users/msahr/OneDrive/Documents/sertifikat_google/divvytripdata/202207-divvy-tripdata.csv")
aug22 <- read.csv("C:/Users/msahr/OneDrive/Documents/sertifikat_google/divvytripdata/202208-divvy-tripdata.csv")
sep22 <- read.csv("C:/Users/msahr/OneDrive/Documents/sertifikat_google/divvytripdata/202209-divvy-tripdata.csv")

trips20fill <- rbind(sep21, oct21, nov21, dec21, jan22, feb22, mar22, apr22, may22, jun22, jul22, aug22, sep21)

trips20fill <- trips20fill %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng, start_station_id,end_station_id, end_station_name))

colnames(trips20fill)  #List of column names
nrow(trips20fill)  #How many rows are in data frame?
dim(trips20fill)  #Dimensions of the data frame?
head(trips20fill, 6)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(trips20fill)  #See list of columns and data types (numeric, character, etc)
summary(trips20fill) #inspect the date and its dimensions before moving onto cleaning

#The default format is yyyy-mm-dd
trips20fill$date <- as.Date(trips20fill$started_at)
trips20fill$month <- format(as.Date(trips20fill$date), "%m")
trips20fill$day <- format(as.Date(trips20fill$date), "%d")
trips20fill$year <- format(as.Date(trips20fill$date), "%Y")
trips20fill$day_of_week <- format(as.Date(trips20fill$date), "%A")
trips20fill$time <- format(trips20fill$started_at, format= "%H:%M")
trips20fill$time <- as.POSIXct(trips20fill$time, format= "%H:%M")

#create calculated field to isolate time spent on every ride.
trips20fill$ride_length <- (as.double(difftime(trips20fill$ended_at, trips20fill$started_at))) /60

str(trips20fill) #confirm data type is double [True]

trips20fill$ride_length <- as.numeric(as.character(trips20fill$ride_length)) #change datatype to numeric for further analysis

trips20fill<- trips20fill[!(trips20fill$start_station_name == "HQ QR" | trips20fill$ride_length<0),]

summary(trips20fill$ride_length)

aggregate(trips20fill$ride_length ~ trips20fill$member_casual, FUN = mean)
aggregate(trips20fill$ride_length ~ trips20fill$member_casual, FUN = median)
aggregate(trips20fill$ride_length ~ trips20fill$member_casual, FUN = max)
aggregate(trips20fill$ride_length ~ trips20fill$member_casual, FUN = min)

trips20fill$day_of_week <- ordered(trips20fill$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

trips20fill %>% 
  mutate(day_of_week = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, day_of_week ) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n())

#DATA VISUALIZATION

trips20fill$day_of_week  <- format(as.Date(trips20fill$date), "%A")
trips20fill %>%                              #total rides broken down by weekday
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, day_of_week) %>%
  labs(x='Day of Week', y='Total Number of Rides', title='Rides per Day of Week', fill = 'Type of Membership') + 
  scale_y_continuous(breaks = c(200000, 400000, 600000), labels = c("200K", "400K", "600K"))

trips20fill %>%   #total rides broken down by month
  group_by(member_casual, month) %>%  
  summarise(total_rides = n(),`average_duration_(mins)` = mean(ride_length)) %>% 
  arrange(member_casual) %>% 
  ggplot(aes(x=month, y=total_rides, fill = member_casual)) + geom_col(position = "dodge") + 
  labs(x= "Month", y= "Total Number of Rides", title = "Rides per Month", fill = "Type of Membership") + 
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000, 700000, 800000), labels = c("100K", "200K", "300K", "400K", "700k", "800k")) + theme(axis.text.x = element_text(angle = 45))

trips20fill %>%    #looking at breakdown of bike types rented
  ggplot(aes(x = rideable_type, fill = member_casual)) + geom_bar(position = "dodge") + 
  labs(x= 'Type of Bike', y='Number of Rentals', title='Which bike works the most', fill = 'Type of Membership') +
  scale_y_continuous(breaks = c(500000, 1000000, 1500000, 2000000), labels = c("500K", "1Mil", "1.5Mil", "2Mil"))

trips20fill %>%        #Find the average time spent riding by each membership type per individul day
  mutate(day_of_week = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + labs(x='Days of the week', y='Average duration - Hrs', title='Average Ride Time Per Week', fill='Type of Membership')


