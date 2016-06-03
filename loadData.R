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

#Set all blanks equal to NA
avalInfo$WeakLayer[avalInfo$WeakLayer ==''] <- NA
avalInfo$Aspect[avalInfo$Aspect==''] <- NA
avalInfo$Trigger[avalInfo$Trigger==''] <- NA

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



#Create avalanche and weather data frame
#Merge data sets
#Merge the avalanche and weather data sets by date
aw_df <- merge(avalInfo, weatInfo, by.x = "Date", by.y="DATE", all.y = FALSE)
#Merge the combined data set with the wind info
aw_df <- merge(aw_df,wndInfo, by.x = "Date", by.y = "DATE", all.x = TRUE, all.y = FALSE)



#Creat full data frame, containing all dates
fl_df <- merge(avalInfo, weatInfo, by.x = 'Date', by.y = 'DATE', all.y = TRUE, all.x = TRUE)
fl_df <- merge(fl_df, wndInfo, by.x='Date', by.y = 'DATE', all.x=TRUE)
#Fill blanks with NA
fl_df$WeakLayer[fl_df$WeakLayer ==""] <- NA
fl_df$Aspect[fl_df$Aspect==''] <- NA
#Remove entries with no date
fl_df <- fl_df[complete.cases(fl_df$Date),]


#Remove all dates/months for which we do no avalanche info 07-09 July-September
#First find out the range of the months in which avalanches occur
x <- gsub("^[0-9]{4}-([0-9][0-9])-[0-9]{2}$",'\\1',avalInfo$Date)
x <- as.numeric(as.character(na.omit(x)))
# Check
for(i in 7:9){
  print(length(na.omit(x[x==i]))==0)
}
#Remove months dates from 06-10 inclusive
fl_df <- fl_df[grep("^[0-9]{4}-[0][789]-[0-9]{2}$", fl_df$Date, invert=TRUE),]
#Same for years
#First find out the range of the years in which avalanches have been recorded
y <- na.omit(gsub("^([0-9]{4})-[0-9][0-9]-[0-9]{2}$",'\\1',avalInfo$Date))
y <- as.numeric(y)
#Create a vector that contains the years that need to be removed
ery <-c()
years <- c(1958:2016)
for(i in years){
  ery <- c(ery,length(y[y==i])==0)
}
#Remove all the years in ery
fl_df <- fl_df[grep(paste(years[ery],collapse = "|"), fl_df$Date, invert=TRUE),]

