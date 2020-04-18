# set working directory
setwd("~/Spatial Analysis IV/Git_Projects/spatialstats4")

# load packages

library(dplyr)
library(lubridate)
library(spatstat) 
library(splancs)
library(stringr)
library(tidyr)

# read in crimes data

crimes <- read.csv("Crimes_2018_2019.csv", stringsAsFactors = FALSE)

# check the structure and clean some variable names

str(crimes)

colnames(crimes)[colnames(crimes) == "ï..ID"] <- "ID"
colnames(crimes)[colnames(crimes) == "Description"] <- "Secondary.Type"
colnames(crimes)[colnames(crimes) == "Location"] <- "Lat.Long"
colnames(crimes)[colnames(crimes) == "Location.Description"] <- "Location"
colnames(crimes)[colnames(crimes) == "Updated.On"] <- "Update.Date"

# remove crimes with missing coordinates because don't have exact addresses to geocode 

crimes <- crimes[which(!is.na(crimes$Longitude) & !is.na(crimes$Latitude)),]

# separate Date field into Date and Time

crimes <- separate(crimes, Date,
                   c("Date", "Time"), sep = " ") 

# convert Date from class Character to Date

crimes$Date <- mdy(crimes$Date)

# convert Time from class Character to Period

crimes$Time.hm <- hm(crimes$Time)

# create new variable Shift that indicates whether crime was reported during the Day (7:00 - 15:00), 
# Evening (15:00 - 23:00), or Midnight (23:00 - 7:00) 

crimes <- mutate(crimes, Shift = case_when(
  Time.hm >= as.numeric(hms("7H 0M 0S")) & Time.hm < as.numeric(hms("15H 0M 0S")) ~ "DAY",
  Time.hm >= as.numeric(hms("15H 0M 0S")) & Time.hm < as.numeric(hms("23H 0M 0S")) ~ "EVENING",
  TRUE ~ "MIDNIGHT"
))

# create new variable Day for day of the week crime was reported

crimes$Day <- weekdays(crimes$Date) 

# create a new variable Weekend that indicates whether crime was reported during the weekend

crimes <- mutate(crimes, Weekend = case_when(
  Day == "Saturday" | Day == "Sunday" ~ 1,
  Day == "Monday" | Day == "Tuesday" | Day == "Wednesday" | Day == "Thursday" | Day == "Friday" ~ 0,
))

# create a new variable Violent that indicates whether a crime is violent

crimes <- mutate(crimes, Violent = case_when(
  Primary.Type == "CRIM SEXUAL ASSAULT" | Primary.Type == "CRIMINAL SEXUAL ASSAULT" | Primary.Type == "HOMICIDE" | Primary.Type == "ROBBERY" ~ 1,
  Primary.Type == "ASSAULT" & str_detect(Secondary.Type, "AGGRAVATED") ~ 1, 
  Primary.Type == "BATTERY" & str_detect(Secondary.Type, "AGGRAVATED") ~ 1, 
  TRUE ~ 0
))

# sum total violent crimes and stratify by 2018/2019, time of day, time of week

sum(crimes$Violent)
sum(crimes$Violent[crimes$Year == "2018"]) 
sum(crimes$Violent[crimes$Year == "2018"]) 

sum(crimes$Violent[crimes$Shift == "DAY"])
sum(crimes$Violent[crimes$Shift == "EVENING"])
sum(crimes$Violent[crimes$Shift == "MIDNIGHT"])

sum(crimes$Violent[crimes$Weekend == 1])
sum(crimes$Violent[crimes$Weekend == 0])

# write cleaned crimes data to .csv for projecting in ArcGIS

write.csv(crimes, "Clean_Crimes_2018_2019.csv")

# read in license data

outlets <- read.csv("Current_Liquor_PPA_Licenses.csv", stringsAsFactors = FALSE)

# check the structure and clean some variable names

str(outlets)

colnames(outlets)[colnames(outlets) == "ï..ID"] <- "ID"

# subset data to alcohol outlets + renewed license only 

alcohol.outlets <- subset(outlets, LICENSE.CODE == 1470 | LICENSE.CODE == 1474 | LICENSE.CODE == 1475 | LICENSE.CODE == 1471)
alcohol.outlets <- subset(alcohol.outlets, APPLICATION.TYPE == "RENEW")

# create new variable ON.PREMISE that indicates whether the outlet has a license for on-premise consumption, 
# coding Late Hour outlets as missing for now

alcohol.outlets <- mutate(alcohol.outlets, ON.PREMISE = case_when(
  LICENSE.CODE == 1470 | LICENSE.CODE == 1475  ~ 1,
  LICENSE.CODE == 1474 ~ 0,
  TRUE ~ -999
))

# check for missing coordinates

sum(is.na(alcohol.outlets$LATITUDE))

# write cleaned outlets data to .csv for projecting in ArcGIS + geocoding missing business coordinates

write.csv(alcohol.outlets, "Clean_Alcohol_Outlets.csv")


