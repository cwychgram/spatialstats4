# set working directory
setwd("~/Spatial Analysis IV/Git_Projects/spatialstats4")

# load packages

library(dplyr)
library(lubridate)
library(rgdal)
library(spatstat) 
library(splancs)
library(stringr)
library(tidyr)

# read in crimes data

crimes <- read.csv("Crimes_2018_2019.csv", stringsAsFactors = FALSE)

# check the structure and clean some variable names

str(crimes)

colnames(crimes)[colnames(crimes) == "?..ID"] <- "ID"
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

colnames(outlets)[colnames(outlets) == "?..ID"] <- "ID"

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

# write outlets data with missing coordinates to .csv for geocoding in ArcGIS 

alcohol.outlets.4geocode <- alcohol.outlets[which(is.na(alcohol.outlets$LONGITUDE) & is.na(alcohol.outlets$LATITUDE)),]

write.csv(alcohol.outlets.4geocode , "Alcohol_Outlets_4Geocode.csv")

# Geocoding notes -- downloaded TIGER shapefile for Cook County, created Address Locator, Geocoded produced 41 Matched and 45 Unmatched. 
# Most were matched by looking up the business name and tweaking the address, but 22 could not be matched, mainly because they were outlets located
# at O'Hare airport terms. Not a big deal and they were deleted

# write all outlets to .csv file so that coordinates from geocoding can be joined to outlets that already had coordinates 

write.csv(alcohol.outlets, "Clean_Alcohol_Outlets.csv")

# read in new shapefile which has all outlet coordinates (except those that could not be geocoded). This shapefile has
# many unecessary fields from the joining process in ArcGIS, so it needs to be cleaned up again

clean.alcohol.outlets <- readOGR("Clean_Alcohol_Outlets.shp", stringsAsFactors = FALSE)

# remove unecessary fields and clean up field names

clean.alcohol.outlets <- clean.alcohol.outlets[, c(3:36, 106:107)]

names(clean.alcohol.outlets)[2] <- "ACCOUNT_NUMBER"
names(clean.alcohol.outlets)[3] <- "SITE_NUMBER"
names(clean.alcohol.outlets)[5] <- "BUSINESS_NAME"
names(clean.alcohol.outlets)[6] <- "ADDRESS"
names(clean.alcohol.outlets)[7] <- "CITY"
names(clean.alcohol.outlets)[8] <- "STATE"
names(clean.alcohol.outlets)[10] <- "WARD"
names(clean.alcohol.outlets)[11] <- "PRECINCT"
names(clean.alcohol.outlets)[13] <- "POLICE_DISTRICT"
names(clean.alcohol.outlets)[14] <- "LICENSE_CODE"
names(clean.alcohol.outlets)[15] <- "LICENSE_DESCRIPTION"
names(clean.alcohol.outlets)[16] <- "BUSINESS_ACTIVITY_ID"
names(clean.alcohol.outlets)[17] <- "BUSINESS_ACTIVITY"
names(clean.alcohol.outlets)[18] <- "LICENSE_NUMBER"
names(clean.alcohol.outlets)[19] <- "APPLICATION_TYPE"
names(clean.alcohol.outlets)[24] <- "LICENSE_START_DATE"
names(clean.alcohol.outlets)[25] <- "LICENSE_EXP_DATE"
names(clean.alcohol.outlets)[35] <- "LONGITUDE"
names(clean.alcohol.outlets)[36] <- "LATITUDE"

clean.alcohol.outlets <- clean.alcohol.outlets[, -c(12, 19, 20, 21, 22, 23, 26, 28, 29, 30, 31, 32, 33)]

# Write to .csv and load into ArcGIS for projecting and calculating meter coordinates

mydir <- getwd()

write.csv(clean.alcohol.outlets, "Clean_Alcohol_Outlets_v2.csv")

# read in projected alcohol outlets. This file excludes Late Hour

alcohol <- readOGR("Alcohol_Outlets_wo_LH.shp")

# remove outlets with dupplicate coordinates

alcohol <- remove.duplicates(alcohol) # 511 removed


