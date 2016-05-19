# Load data, clean and tidy it.

#Set working directory
setwd("C:/Users/Owner/Documents/DataScience/avalanche")
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
avalInfo$Date <- gsub("^([0-9]{2})/([0-9]{1,2})/[0-9]{2}([0-9]{2})$","\\1-\\2-\\3",avalInfo$Date)
avalInfo$Date <- as.Date( as.character(avalInfo$Date), format="%m-%d-%y")
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


#Fill in blanks with NA
avalInfo$Weak.Layer[avalInfo$Weak.Layer==""] <- NA

# Create ordered data (in this case I ordered it so that NE is centered, because then it looks normal)
#avalInfo$Aspect = factor(avalInfo$Aspect, levels=c("West", "Northwest", "North", "Northeast", "East", "Southeast", "South", ""), ordered = TRUE)

#Merge data sets
#Merge the avalanche and weather data sets by date
aw_df <- merge(avalInfo, weatInfo, by.x = "Date", by.y="DATE", all.y = FALSE)
#Merge the combined data set with the wind info
aw_df <- merge(aw_df,wndInfo, by.x = "Date", by.y = "DATE", all.x = TRUE, all.y = FALSE)
#Remove data that is missing at least 90% of its observations
aw_df$STATION <- NULL
aw_df$STATION_NAME <- NULL
aw_df$MDPR <- NULL
aw_df$MDSF <- NULL
aw_df$DAPR<- NULL
aw_df$DASF <- NULL
aw_df$WT01 <- NULL
aw_df$WT06 <- NULL
aw_df$WT05 <- NULL
aw_df$WT11 <- NULL
aw_df$WT04 <- NULL
aw_df$WT03 <- NULL

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
aw_df$Caught <- NULL
aw_df$Carried <- NULL
aw_df$Buried...Fully <- NULL
aw_df$Buried...Partly <- NULL
aw_df$Killed <- NULL
aw_df$Injured <- NULL



#Create data frame for weather when no avalanches occured
#First create the data frame for wind and weather
no_a_df <- merge(weatInfo,wndInfo, by.x = 'DATE', by.y = 'DATE')
#Remove variables with >90% NA
no_a_df$STATION <- NULL
no_a_df$STATION_NAME <- NULL
no_a_df$MDPR <- NULL
no_a_df$MDSF <- NULL
no_a_df$DAPR<- NULL
no_a_df$DASF <- NULL
no_a_df$WT01 <- NULL
no_a_df$WT06 <- NULL
no_a_df$WT05 <- NULL
no_a_df$WT11 <- NULL
no_a_df$WT04 <- NULL
no_a_df$WT03 <- NULL

#First remove months 07,08,09, because avalanche season runs oct.-june (at most) then so weather data would be inappropriate
#Need to fix so that outlier dates are not included, such as June
no_a_df <- no_a_df[grep("-0[789]-", no_a_df$DATE,invert = TRUE),]
#Now remove all the days that avalanches occured
#test <- no_a_df
#store <- c(0)
for(i in 1:length(aw_df$Date)){
  #print(paste0("the it is ", l))
  no_a_df <- subset(no_a_df, DATE != aw_df$Date[i])
  #store <- c(store, length(test$DATE))
}
# Check that dates are removed
no_a_df$DATE[which(no_a_df$Snowfall=='32')]
aw_df$Date[which(aw_df$Snowfall=='32')]
