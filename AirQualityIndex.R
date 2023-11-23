#Loading all the necessary libraries
library(tidyverse)
library(ggplot2)
library(caret)
library(dplyr)
library(zoo)

#Setting the working directory to where the CSV File is located
setwd("/Users/USER/Downloads/archive-2")

#Importing the CSV Files
city_day <- read.csv("city_day.csv")
city_hour <- read.csv("city_hour.csv")
station_day <- read.csv("station_day.csv")
station_hour <- read.csv("station_hour.csv")
stations <- read.csv("stations.csv")

#Subsetting, Cleaning, Modifying and Merging Data

#Converting the DateTime column to R's Datetime Format
station_hour$Datetime <- as.POSIXct(station_hour$Datetime, format = "%Y-%m-%d %H:%M:%S")

#Merging the station_hour and stations CSV files using the common column StationId
station_hour <- merge(station_hour, stations, by = "StationId")

#Subsetting the dataframe station_hour to include rows where StationsID is in the stations vector
station_hour <- station_hour[station_hour$StationId %in% stations$StationId, ]

#Sorting station_hour by stationID and date time
station_hour <- station_hour[order(station_hour$StationId, station_hour$Datetime), ]

#The 'Datetime' column is converted to a date format for the new 'Date' column.
#Then, the 'Datetime' column is converted back to a character string.
station_hour$Date <- as.Date(station_hour$Datetime)
station_hour$Datetime <- as.character(station_hour$Datetime)


#CALCULATING AIR QUALITY INDEX
#The AQI calculation uses 7 measures: PM2.5, PM10, SO2, NOx, NH3, CO and O3.
#For PM2.5, PM10, SO2, NOx and NH3 the average value in last 24-hrs is used with the condition of having at least 16 values.
#For CO and O3 the maximum value in last 8-hrs is used.
#Each measure is converted into a Sub-Index based on pre-defined groups.
#Sometimes measures are not available due to lack of measuring or lack of required data points.
#Final AQI is the maximum Sub-Index with the condition that at least one of PM2.5 and PM10 should be available and at least three out of the seven should be available.

station_hour <- station_hour %>%
  group_by(StationId) %>%
  mutate(
    PM10_24hr_avg = zoo::rollapplyr(PM10, 24, mean, fill = NA, partial = TRUE, align = "right", min_obs = 16),
    PM2.5_24hr_avg = zoo::rollapplyr(`PM2.5`, 24, mean, fill = NA, partial = TRUE, align = "right", min_obs = 16),
    SO2_24hr_avg = zoo::rollapplyr(SO2, 24, mean, fill = NA, partial = TRUE, align = "right", min_obs = 16),
    NOx_24hr_avg = zoo::rollapplyr(NOx, 24, mean, fill = NA, partial = TRUE, align = "right", min_obs = 16),
    NH3_24hr_avg = zoo::rollapplyr(NH3, 24, mean, fill = NA, partial = TRUE, align = "right", min_obs = 16),
    CO_8hr_max = zoo::rollapplyr(CO, 8, max, fill = NA, partial = TRUE, align = "right", min_obs = 1),
    O3_8hr_max = zoo::rollapplyr(O3, 8, max, fill = NA, partial = TRUE, align = "right", min_obs = 1)
  )

#Creating a Function for the PM2.5_24_hr_avg Calculation
get_PM25_subindex <- function(x) {
  if (is.na(x)) {
    return(NA)
  } else if (x <= 30) {
    return(x * 50 / 30)
  } else if (x <= 60) {
    return(50 + (x - 30) * 50 / 30)
  } else if (x <= 90) {
    return(100 + (x - 60) * 100 / 30)
  } else if (x <= 120) {
    return(200 + (x - 90) * 100 / 30)
  } else if (x <= 250) {
    return(300 + (x - 120) * 100 / 130)
  } else if (x > 250) {
    return(400 + (x - 250) * 100 / 130)
  } else {
    return(0)
  }
}


#Creating a new column PM2.5Sub Index by applying the Function
station_hour$PM2.5_SubIndex <- sapply(station_hour$PM2.5_24hr_avg, get_PM25_subindex)


#Creating a function for Pm10 SubIndex
get_PM10_subindex <- function(x) {
  if (is.na(x)) {
    return(NA)
  } else if (x <= 50) {
    return(x)
  } else if (x <= 100) {
    return(x)
  } else if (x <= 250) {
    return(100 + (x - 100) * 100 / 150)
  } else if (x <= 350) {
    return(200 + (x - 250))
  } else if (x <= 430) {
    return(300 + (x - 350) * 100 / 80)
  } else if (x > 430) {
    return(400 + (x - 430) * 100 / 80)
  } else {
    return(0)
  }
}

#Applying the function to the dataframe
station_hour$PM10_SubIndex <- sapply(station_hour$PM10_24hr_avg, get_PM10_subindex)

#Creating a function for Sulphur Di Oxide calculation
get_SO2_subindex <- function(x) {
  if (is.na(x)) {
    return(NA)
  } else if (x <= 40) {
    return(x * 50 / 40)
  } else if (x <= 80) {
    return(50 + (x - 40) * 50 / 40)
  } else if (x <= 380) {
    return(100 + (x - 80) * 100 / 300)
  } else if (x <= 800) {
    return(200 + (x - 380) * 100 / 420)
  } else if (x <= 1600) {
    return(300 + (x - 800) * 100 / 800)
  } else if (x > 1600) {
    return(400 + (x - 1600) * 100 / 800)
  } else {
    return(0)
  }
}

#Applying the SO2 function to the station_hour dataframe
station_hour$SO2_SubIndex <- sapply(station_hour$SO2_24hr_avg, get_SO2_subindex)

#Creating a function for NO2 calculation
get_NOx_subindex <- function(x) {
  if (is.na(x)) {
    return(NA)
  } else if (x <= 40) {
    return(x * 50 / 40)
  } else if (x <= 80) {
    return(50 + (x - 40) * 50 / 40)
  } else if (x <= 180) {
    return(100 + (x - 80) * 100 / 100)
  } else if (x <= 280) {
    return(200 + (x - 180) * 100 / 100)
  } else if (x <= 400) {
    return(300 + (x - 280) * 100 / 120)
  } else if (x > 400) {
    return(400 + (x - 400) * 100 / 120)
  } else {
    return(0)
  }
}

#Applying the function to the dataframe
station_hour$NOx_SubIndex <- sapply(station_hour$NOx_24hr_avg, get_NOx_subindex)

#Creating a function for Carbon Monoxide calculation
get_CO_subindex <- function(x) {
  if (is.na(x)) {
    return(NA)
  } else if (x <= 1) {
    return(x * 50 / 1)
  } else if (x <= 2) {
    return(50 + (x - 1) * 50 / 1)
  } else if (x <= 10) {
    return(100 + (x - 2) * 100 / 8)
  } else if (x <= 17) {
    return(200 + (x - 10) * 100 / 7)
  } else if (x <= 34) {
    return(300 + (x - 17) * 100 / 17)
  } else if (x > 34) {
    return(400 + (x - 34) * 100 / 17)
  } else {
    return(0)
  }
}

#Creating a function for NH3 calculation
get_NH3_subindex <- function(x) {
  if (is.na(x)) {
    return(NA)
  } else if (x <= 200) {
    return(x * 50 / 200)
  } else if (x <= 400) {
    return(50 + (x - 200) * 50 / 200)
  } else if (x <= 800) {
    return(100 + (x - 400) * 100 / 400)
  } else if (x <= 1200) {
    return(200 + (x - 800) * 100 / 400)
  } else if (x <= 1800) {
    return(300 + (x - 1200) * 100 / 600)
  } else if (x > 1800) {
    return(400 + (x - 1800) * 100 / 600)
  } else {
    return(0)
  }
}

#Applying it to the dataframe
station_hour$NH3_SubIndex <- sapply(station_hour$NH3_24hr_avg, get_NH3_subindex)



#Applying the function to the dataframe
station_hour$CO_SubIndex <- sapply(station_hour$CO_8hr_max, get_CO_subindex)

#Creating a function for Ozone Calculation
get_O3_subindex <- function(x) {
  if (is.na(x)) {
    return(NA)
  } else if (x <= 50) {
    return(x * 50 / 50)
  } else if (x <= 100) {
    return(50 + (x - 50) * 50 / 50)
  } else if (x <= 168) {
    return(100 + (x - 100) * 100 / 68)
  } else if (x <= 208) {
    return(200 + (x - 168) * 100 / 40)
  } else if (x <= 748) {
    return(300 + (x - 208) * 100 / 539)
  } else if (x > 748) {
    return(400 + (x - 400) * 100 / 539)
  } else {
    return(0)
  }
}

#Applying it to the dataframe
station_hour$O3_SubIndex <- sapply(station_hour$O3_8hr_max, get_O3_subindex)

#FINAL AQI CALCULATION
#AQI The final AQI is the maximum Sub-Index among the available sub-indices with the condition that at least one of PM2.5 and PM10 should be available and at least three out of the seven should be available.
#There is no theoretical upper value of AQI but its rare to find values over 1000.

get_AQI_bucket <- function(x) {
  if (is.na(x)) {
    return(NA)
  } else if (x <= 50) {
    return("Good")
  } else if (x <= 100) {
    return("Satisfactory")
  } else if (x <= 200) {
    return("Moderate")
  } else if (x <= 300) {
    return("Poor")
  } else if (x <= 400) {
    return("Very Poor")
  } else if (x > 400) {
    return("Severe")
  } else {
    return(NA)
  }
}

#Adding a new column for checking how many subindexes are matched
station_hour$Checks <- (station_hour$PM2.5_SubIndex > 0) + 
  (station_hour$PM10_SubIndex > 0) + 
  (station_hour$SO2_SubIndex > 0) + 
  (station_hour$NOx_SubIndex > 0) + 
  (station_hour$NH3_SubIndex > 0) + 
  (station_hour$CO_SubIndex > 0) + 
  (station_hour$O3_SubIndex > 0)


#Calculating AQI_Calculated Column
library(dplyr)
station_hour <- station_hour %>%
  rowwise() %>%
  mutate(AQI_calculated = max(c(PM2.5_SubIndex, PM10_SubIndex, SO2_SubIndex, NOx_SubIndex,
                                NH3_SubIndex, CO_SubIndex, O3_SubIndex), na.rm = TRUE))
#Handling Special Cases
station_hour <- station_hour %>%
  mutate(AQI_calculated = ifelse(PM2.5_SubIndex + PM10_SubIndex <= 0 | Checks < 3, NA, AQI_calculated))

station_hour$AQI_bucket_calculated <- sapply(station_hour$AQI_calculated, get_AQI_bucket)

#Summarising AQI Calculated Bucket
table(station_hour[!is.na(station_hour$AQI_calculated), "AQI_bucket_calculated"])

#Aggregrating station_hour data

station_hour_summary <- station_hour %>%
  group_by(StationId, Date) %>%
  summarise(AQI_calculated = mean(AQI_calculated, na.rm = TRUE)) %>%
  ungroup()

#Merging station_hour and station_day datasets
library(data.table)
station_hour <- as.data.table(station_hour)
station_hour_summary <- station_hour[, .(AQI_calculated = mean(AQI_calculated, na.rm = TRUE)), by = .(StationId, Date)]
station_day$Date <- as.character(station_day$Date)
station_hour_summary$Date <- as.character(station_hour_summary$Date)

station_day <- merge(station_day, station_hour_summary, by = c("StationId","Date"))

#Adding AQI Bucket to station_day
station_day$AQI_calculated <- round(station_day$AQI_calculated)
station_day <- station_day[!is.na(station_day$AQI_calculated), ]


#Calculating AQI on a city level basis
city_hour$Date <- as.Date(as.POSIXct(city_hour$Datetime, format="%Y-%m-%d %H:%M:%S"))
city_hour$Date <- as.character(city_hour$Date)

station_hour1 <- merge(station_hour, stations[, c("StationId", "City")], by = "StationId")

station_hour_summary1 <- station_hour %>%
  group_by(City, Datetime) %>%
  summarise(AQI_calculated = mean(AQI_calculated, na.rm = TRUE)) %>%
  ungroup()

city_hour <- merge(city_hour, station_hour_summary1, by = c("City", "Datetime"))
city_hour$AQI_calculated <- round(city_hour$AQI_calculated)

city_hour_summary_day <- city_hour %>%
  group_by(City, Date) %>%
  summarise(AQI_calculated = mean(AQI_calculated, na.rm = TRUE)) %>%
  ungroup()

city_day <- merge(city_day, city_hour_summary_day, by = c("City", "Date"))
city_day$AQI_calculated <- round(city_day$AQI_calculated)

#RemovingNAs
city_hour <- city_hour[!is.na(city_hour$AQI_calculated),]
city_day <- city_day[!is.na(city_day$AQI_calculated),]

#Verification
# Prepare data by removing NAs and selecting relevant columns
df_check_station_hour <- na.omit(station_hour[, c("AQI", "AQI_calculated")])
df_check_station_day <- na.omit(station_day[, c("AQI", "AQI_calculated")])
df_check_city_hour <- na.omit(city_hour[, c("AQI", "AQI_calculated")])
df_check_city_day <- na.omit(city_day[, c("AQI", "AQI_calculated")])

# Define a function to calculate and print the desired metrics
print_metrics <- function(df, label) {
  total_rows <- nrow(df)
  matched_aqi <- sum(df$AQI == df$AQI_calculated)
  percent_match <- matched_aqi * 100 / total_rows
  
  cat(label, "\n")
  cat("Rows:", total_rows, "\n")
  cat("Matched AQI:", matched_aqi, "\n")
  cat("% Match:", percent_match, "\n\n")
}

# Apply the function to each dataframe
print_metrics(df_check_station_hour, "Station + Hour")
print_metrics(df_check_station_day, "Station + Day")
print_metrics(df_check_city_hour, "City + Hour")
print_metrics(df_check_city_day, "City + Day")






