# Load data, clean and tidy it.

#Set working directory
#setwd("C:/Users/Owner/Documents/DataScience/avalanche")
#Load in csv files
avalInfo <- read.csv('avalanches_raw.csv')
weatInfo <- read.csv('altaGuardWeather6516.csv', na.strings = -9999)

# How to Download data 
# Avalanche data may be downloaded manually at "https://utahavalanchecenter.org/avalanches/download"
#avalInfo <- read.csv('https://utahavalanchecenter.org/avalanches/download?download=1&eid=712')
# Weather data may be downloaded manually at "http://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USC00420072/detail", "http://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/locations/CITY:US490006/detail"
# Also download at https://www.ncdc.noaa.gov/data-access
#weatInfo <- read.csv()

# Got wind data from: http://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USS0011J69S/detail
# Picked this location, because it had the largest date range, and is also in the mountains
# The location fo the station is north of the canyons, it is called Louis Meadow
wndInfo <- read.csv('windLouisMeadow739480.csv')

# Change names to be human readable
library(reshape)
weatInfo <- rename(weatInfo, c(PRCP = "Precipitation")) # in tenths of mm
weatInfo <- rename(weatInfo, c(SNWD = "Snow_Depth")) # in mm
weatInfo <- rename(weatInfo, c(SNOW = "Snowfall")) # in mm
weatInfo <- rename(weatInfo, c(TMAX = "Max_Temperature")) # in tenths of degrees C
weatInfo <- rename(weatInfo, c(TMIN = "Min_Temperature")) # in tenths of degrees C
weatInfo <- rename(weatInfo, c(TOBS = "Temperature_at_observation_time")) # in tenths of degrees C
wndInfo <- rename(wndInfo, c(AWND = "Average_Wind_Speed")) # in meters per second (pretty sure)
wndInfo <- rename(wndInfo, c(WSFI = "Max_Wind_Speed")) # in meters per second (pretty sure)
#Remove station from wndInfo
wndInfo$STATION <- NULL

#Make the "dates variable", dates
#Fix dates in month/day/year format to month-day-year(2)
avalInfo$Date <-gsub("^([0-9]{2})-([0-9]{1,2})-([0][0-9])$","\\1-\\2-20\\3",avalInfo$Date)
avalInfo$Date <-gsub("^([0-9]{2})-([0-9]{1,2})-([01][0123456])$","\\1-\\2-20\\3",avalInfo$Date)
avalInfo$Date <-gsub("^([0-9]{2})-([0-9]{1,2})-([23456789][0-9])$","\\1-\\2-19\\3",avalInfo$Date)
avalInfo$Date <- gsub("^([0-9]{2})/([0-9]{1,2})/([0-9]{4})$","\\1-\\2-\\3",avalInfo$Date)

avalInfo$Date <- as.Date( as.character(avalInfo$Date), format="%m-%d-%Y")
weatInfo$DATE <-as.Date(as.character(weatInfo$DATE), format="%Y%m%d")
wndInfo$DATE <- as.Date(as.character(wndInfo$DATE), format = "%Y%m%d")
# Check that they are dates
str(weatInfo$DATE)
str(avalInfo$Date)
str(wndInfo$DATE)

#Convert factors to numerics
avalInfo$Elevation <- as.numeric(as.character(avalInfo$Elevation))
avalInfo$Width <- as.numeric(as.character(avalInfo$Width))
avalInfo$Vertical <- as.numeric(as.character(avalInfo$Vertical))

#Fix Depth since it is in inches and feet
#Store ft values
ftvals <- grep("'", avalInfo$Depth)
#Remove ' and " so that NA's are not introduced for numeric values
avalInfo$Depth <- gsub("'","",avalInfo$Depth)
avalInfo$Depth <-gsub("\"","",avalInfo$Depth)
#Convert to numeric so I can use operators
avalInfo$Depth <- as.numeric(as.character(avalInfo$Depth))
#Multiply the feet by 12
avalInfo$Depth[ftvals] <- avalInfo$Depth[ftvals]*12

# Fix Coordinates, so that they are split into lat and long
# First introduce NA's
avalInfo$Coordinates[avalInfo$Coordinates==""] <- NA 
library(reshape2)
avalInfo$Coordinates <- colsplit(avalInfo$Coordinates,",", c("Latitude", "Longitude"))
#Replace 0's in Coordinates with NA
avalInfo$Coordinates$Latitude <- gsub("\\b0\\b","", avalInfo$Coordinates$Latitude)
avalInfo$Coordinates$Latitude <- as.numeric(as.character(avalInfo$Coordinates$Latitude))
avalInfo$Coordinates$Longitude <- as.numeric(as.character(gsub("\\b0\\b","", avalInfo$Coordinates$Longitude)))
#Change column names
avalInfo$Latitude <- avalInfo$Coordinates$Latitude
avalInfo$Longitude <- avalInfo$Coordinates$Longitude
avalInfo$Coordinates <- NULL
avalInfo$WeakLayer <- avalInfo$Weak.Layer
avalInfo$Weak.Layer <- NULL
avalInfo$BuriedFully <- avalInfo$Buried...Fully
avalInfo$BuriedPartly <- avalInfo$Buried...Partly
avalInfo$Buried...Fully <- NULL
avalInfo$Buried...Partly <- NULL
#Create data frame for caught, carried, and buried
ccb <- data.frame(date=aw_df$Date, Caught=aw_df$Caught, Carried=aw_df$Carried, Buried.Partly=aw_df$Buried...Partly, Buried.Fully=aw_df$Buried...Fully)
# Now use that data frame to gather column names into a key "TypeOfRide" variable
ccb <- melt(ccb, id.vars="date", na.rm=T)
#Change column names
colnames(ccb)[2] <- "TypeOfRide"
colnames(ccb)[3] <- "TypeOfRideN"
#Now merge ccb to aw_df
aw_df_ccb <- merge(aw_df, ccb, by.x = "Date", all.x=FALSE, by.y="date", all.y=TRUE)
#Remove unessecary columns
aw_df_ccb$Caught <- NULL
aw_df_ccb$Carried <- NULL
aw_df_ccb$Buried...Fully <- NULL
aw_df_ccb$Buried...Partly <- NULL

#Create data frame injured or killed
ik <- data.frame(date=aw_df$Date, Injured=aw_df$Injured , Killed=aw_df$Killed)
# Now use that data frame to gather column names into a key "BodilyHarm" variable
ik <- melt(ik, id.vars="date", na.rm=T)
#Change column names
colnames(ik)[2] <- "BodilyHarm"
colnames(ik)[3] <- "BodilyHarmN"
#Now merge ccb to aw_df creates to many duplicates
aw_df_ik <- merge(aw_df, ik, by.x = "Date", all.x = FALSE, by.y="date", all.y=TRUE)
#Remove unessecary columns
aw_df_ik$Killed <- NULL
aw_df_ik$Injured <- NULL
#Remove the data that has been tidied into other data sets
avalInfo$Caught <- NULL
avalInfo$Carried <- NULL
avalInfo$BuriedFully <- NULL
avalInfo$BuriedPartly <- NULL
avalInfo$Killed <- NULL
avalInfo$Injured <- NULL



# Remove variables that are not usable
weatInfo$STATION <- NULL
weatInfo$STATION_NAME <- NULL
weatInfo$MDPR <- NULL
weatInfo$MDSF <- NULL
weatInfo$DAPR<- NULL
weatInfo$DASF <- NULL
weatInfo$WT01 <- NULL
weatInfo$WT06 <- NULL
weatInfo$WT05 <- NULL
weatInfo$WT11 <- NULL
weatInfo$WT04 <- NULL
weatInfo$WT03 <- NULL


# Create ordered data (in this case I ordered it so that NE is centered, because then it looks normal)
#avalInfo$Aspect = factor(avalInfo$Aspect, levels=c("West", "Northwest", "North", "Northeast", "East", "Southeast", "South", ""), ordered = TRUE)

#Merge data sets
#Merge the avalanche and weather data sets by date
#aw_df <- merge(avalInfo, weatInfo, by.x = "Date", by.y="DATE", all.y = FALSE)
#Merge the combined data set with the wind info
#aw_df <- merge(aw_df,wndInfo, by.x = "Date", by.y = "DATE", all.x = TRUE, all.y = FALSE)

#Creat full data frame, containing all dates
fl_df <- merge(avalInfo, weatInfo, by.x = 'Date', by.y = 'DATE', all.y = TRUE, all.x = TRUE)
fl_df <- merge(fl_df, wndInfo, by.x='Date', by.y = 'DATE', all.x=TRUE)
#Fill blanks with NA
fl_df$WeakLayer[fl_df$WeakLayer ==""] <- NA
fl_df$Aspect[fl_df$Aspect==''] <- NA

#Remove all dates between not in interquartile range of when we have info for avalances
#First find out the range of dates
x <- gsub("^[0-9]{4}-([0-9][0-9])-[0-9]{2}$",'\\1',avalInfo$Date)
#Identify probabilities
library(ggplot2)
qplot(x)
length(x[which(x=='05')])/length(x)
#Remove months dates from 05-10 inclusive
fl_df <- fl_df[grep("-[01][567890]-", fl_df$Date),]


#Create data frame that contains the number of avalanches that have occure
numav <- fl_df
numav <- numav[!duplicated(test$Date),]
#Create a did avalanche column
library(plyr)
temp <- ddply(avalInfo,.(Date),nrow)
colnames(temp)[2] <- "NumberOfAvalanches"
numav <- merge(numav, temp, by.x='Date', by.y='Date', all.x = TRUE)
#Replace NA with 0 in number of avalanches column
numav$NumberOfAvalanches[is.na(numav$NumberOfAvalanches)] <- 0
#Set values with NA and number of avalanches=0 to 0
numav$Depth[numav$NumberOfAvalanches==0] <- 0
numav$Width[numav$NumberOfAvalanches==0] <- 0
numav$Vertical[numav$NumberOfAvalanches==0] <- 0
numav$Elevation[numav$NumberOfAvalanches==0] <- 0
numav$Latitude[numav$NumberOfAvalanches==0] <- 0
numav$Longitude[numav$NumberOfAvalanches==0] <- 0
