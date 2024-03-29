# set working directory
setwd("~/Spatial Analysis IV/Git_Projects/spatialstats4")

# load packages

library(dplyr)
library(ggplot2)
library(lubridate)
library(maptools)
library(rgdal)
library(spatstat) 
library(sp)
library(spdplyr)
library(splancs)
library(stringr)
library(tidyr)

###################               
## DATA CLEANING ##
###################             

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
# coding Late Hour outlets as missing for now (they will be exported to separate file after projecting in ArcGIS)

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

# Geocoding -- downloaded TIGER shapefile for Cook County to create Address Locator. Geocoding produced 41 Matched and 45 Unmatched. 
# Most were matched by googling the business name and cleaning up the address. 22 could not be matched, most of them business at O'hare.
# These were removed from the dataset. 

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

#################### 
## INTENSITY MAPS ##
#################### 

# read in projected Chicago shapefile

Chicago <- readOGR("Chicago_Boundary_prj.shp")

## Provide the spatial domain ##
#  Note: I tried using the fortify function shown in lab but this resulted in the polygon tearing on the upper left (O'Hare)

# extract the spatial polygon 

x <- Chicago@polygons[[1]]@Polygons 

str(x) # The slot I want seems to be 2, not 1 (as shown in lab)

# retrieve the matrix of coordinates from the spatial polygon

coords <- slot(x[[2]], "coords") 

# inverse the order of the points so that they appear in a counterclockwise order

coords <-coords[11326:1, ] 

# get rid of the first row, which is a repeat of the final row

coords <- coords[-1, ] 

# change the scale from meters to miles

coords_mi < -coords * 0.000621371192 

# read in projected alcohol outlets. This file excludes Late Hour

alcohol <- readOGR("Alcohol_Outlets_wo_LH.shp")

# remove outlets with dupplicate coordinates

alcohol <- remove.duplicates(alcohol) # 511 removed

# subset into on-premise and off-premise consumption outlets

on_premise <- alcohol %>% filter(alcohol@data$ON_PREMISE == 1)
off_premise <- alcohol %>% filter(alcohol@data$ON_PREMISE == 0)

# create ppp objects

alcohol_ppp <- ppp(alcohol@coords[, 1] * 0.000621371192, alcohol@coords[, 2] * 0.000621371192,
                  window = owin(poly = list(x = coords_mi[, 1], y = coords_mi[, 2])))

on_premise_ppp <- ppp(on_premise@coords[, 1] * 0.000621371192, on_premise@coords[, 2] * 0.000621371192,
                      window = owin(poly = list(x = coords_mi[, 1], y = coords_mi[, 2])))

off_premise_ppp <- ppp(off_premise@coords[, 1] * 0.000621371192, off_premise@coords[, 2] * 0.000621371192,
                       window = owin(poly = list(x = coords_mi[, 1], y = coords_mi[, 2])))

# create kernel density maps using the optimal bandwidth

alcohol_kd <- density(alcohol_ppp, bw.diggle(alcohol_ppp))
plot(alcohol_kd, main = "Spatial Intensity of Alcohol Outlets")
polygon(coords_mi, lwd = 2)

on_premise_kd <- density(on_premise_ppp, bw.diggle(on_premise_ppp))
plot(on_premise_kd, main = "Spatial Intensity of On-Premise Alcohol Outlets")
polygon(coords_mi, lwd = 2)

off_premise_kd <- density(off_premise_ppp, bw.diggle(off_premise_ppp))
plot(off_premise_kd, main = "Spatial Intensity of Off-Premise Alcohol Outlets")
polygon(coords_mi, lwd = 2)

# read in projected crime data

crimes <- readOGR("Crimes_2018_2019_prj.shp")

crimes <- remove.duplicates(crimes) # this removes a LOT of points because of the street block anonymization

# subset into violent/non-violent, day/evening/midnight, weekend/weekday

violent_crimes <- crimes %>% filter(crimes@data$Violent == 1)
nonviolent_crimes <- crimes %>% filter(crimes@data$Violent == 0)

day_violent_crimes <- violent_crimes %>% filter(violent_crimes@data$Shift == "DAY")
evening_violent_crimes <- violent_crimes %>% filter(violent_crimes@data$Shift == "EVENING")
midnight_violent_crimes <- violent_crimes %>% filter(violent_crimes@data$Shift == "MIDNIGHT")

weekend_violent_crimes <- violent_crimes %>% filter(violent_crimes@data$Weekend == 1) 
weekday_violent_crimes <- violent_crimes %>% filter(violent_crimes@data$Weekend == 0) 

# create ppp objects

crimes_ppp <- ppp(crimes@coords[, 1] * 0.000621371192, crimes@coords[, 2] * 0.000621371192,
                  window = owin(poly = list(x = coords_mi[, 1], y = coords_mi[, 2])))

violent_crimes_ppp <- ppp(violent_crimes@coords[, 1] * 0.000621371192, violent_crimes@coords[, 2] * 0.000621371192,
                          window = owin(poly = list(x = coords_mi[, 1], y = coords_mi[, 2])))

nonviolent_crimes_ppp <- ppp(nonviolent_crimes@coords[, 1] * 0.000621371192, nonviolent_crimes@coords[, 2] * 0.000621371192,
                          window = owin(poly = list(x = coords_mi[, 1], y = coords_mi[, 2])))

day_violent_crimes_ppp <- ppp(day_violent_crimes@coords[, 1] * 0.000621371192, day_violent_crimes@coords[, 2] * 0.000621371192,
                                  window = owin(poly = list(x = coords_mi[, 1], y = coords_mi[, 2])))

evening_violent_crimes_ppp <- ppp(evening_violent_crimes@coords[, 1] * 0.000621371192, evening_violent_crimes@coords[, 2] * 0.000621371192,
                          window = owin(poly = list(x = coords_mi[, 1], y = coords_mi[, 2])))

midnight_violent_crimes_ppp <- ppp(midnight_violent_crimes@coords[, 1] * 0.000621371192, midnight_violent_crimes@coords[, 2] * 0.000621371192,
                                  window = owin(poly = list(x = coords_mi[, 1], y = coords_mi[, 2])))

weekday_violent_crimes_ppp <- ppp(weekday_violent_crimes@coords[, 1] * 0.000621371192, weekday_violent_crimes@coords[, 2] * 0.000621371192,
                                   window = owin(poly = list(x = coords_mi[, 1], y = coords_mi[, 2])))

weekend_violent_crimes_ppp <- ppp(weekend_violent_crimes@coords[, 1] * 0.000621371192, weekend_violent_crimes@coords[, 2] * 0.000621371192,
                                  window = owin(poly = list(x = coords_mi[, 1], y = coords_mi[, 2])))

# create kernel density maps using the optimal bandwidth

crimes_kd <- density(crimes_ppp, bw.diggle(crimes_ppp))
plot(crimes_kd, main = "Spatial Intensity of Crimes")
polygon(coords_mi, lwd = 2)

violent_crimes_kd <- density(violent_crimes_ppp, bw.diggle(violent_crimes_ppp))
plot(violent_crimes_kd, main = "Spatial Intensity of Violent Crimes")
polygon(coords_mi, lwd = 2)

nonviolent_crimes_kd <- density(nonviolent_crimes_ppp, bw.diggle(nonviolent_crimes_ppp))
plot(nonviolent_crimes_kd, main = "Spatial Intensity of Non-Violent Crimes")
polygon(coords_mi, lwd = 2)

day_violent_crimes_kd <- density(day_violent_crimes_ppp, bw.diggle(day_violent_crimes_ppp))
plot(day_violent_crimes_kd, main = "Spatial Intensity of Violent Crimes -- Day")
polygon(coords_mi, lwd = 2)

evening_violent_crimes_kd <- density(evening_violent_crimes_ppp, bw.diggle(evening_violent_crimes_ppp))
plot(evening_violent_crimes_kd, main = "Spatial Intensity of Violent Crimes -- Evening")
polygon(coords_mi, lwd = 2)

midnight_violent_crimes_kd <- density(midnight_violent_crimes_ppp, bw.diggle(midnight_violent_crimes_ppp))
plot(midnight_violent_crimes_kd, main = "Spatial Intensity of Violent Crimes -- Midnight")
polygon(coords_mi, lwd = 2)

weekday_violent_crimes_kd <- density(weekday_violent_crimes_ppp, bw.diggle(weekday_violent_crimes_ppp))
plot(weekday_violent_crimes_kd, main = "Spatial Intensity of Violent Crimes -- Weekday")
polygon(coords_mi, lwd = 2)

weekend_violent_crimes_kd <- density(weekend_violent_crimes_ppp, bw.diggle(weekend_violent_crimes_ppp))
plot(weekend_violent_crimes_kd, main = "Spatial Intensity of Violent Crimes -- Weekend")
polygon(coords_mi, lwd = 2)

################              
## K FUNCTION ##
################  

#####################              
## DIFF K FUNCTION ##
#####################  

######################              
## CROSS K FUNCTION ##
######################      

temp <- rbind(data.frame(x = alcohol_ppp$x, y = alcohol_ppp$y, type = "Alcohol Outlets"), 
              data.frame(x = crimes_ppp$x, y = crimes_ppp$y, type = "Crimes"))

temp_ppp <- ppp(temp[, 1], temp[, 2],
                window = owin(poly = list(x = coords_mi[, 1], y = coords_mi[, 2])), marks = temp$type)

K_cross <- Kcross(temp_ppp, "Alcohol Outlets", "Crimes")

plot(K_cross$r, K_cross$iso, lwd = 2, xlab = "Distance (mi)", ylab = "Estimated Cross K-Function" ,main = "", type = "l")
title("Cross K-Function")
lines(K_cross$r, K_cross$theo, lwd = 2, col = "green" ,lty = 1)
legend(locator(1), legend = c("Observed Cross K", "Independence Cross K"), lty = c(1, 1), col = c("black", "green"), lwd = 2)

lambda <- 197954/231.37 # Do I use the # of crimes before duplicates were removed or after? 
lambda # 855.57 crimes expected per sq mi

plot(K_cross$r, K_cross$iso * lambda, lwd = 2, xlab = "Distance (mi)", ylab = "Expected Number", main = "", type = "l")
lines(K_cross$r, K_cross$theo * lambda, lwd = 2, col = "blue", lty = 3)

