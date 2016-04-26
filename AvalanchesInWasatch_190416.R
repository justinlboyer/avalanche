# Find working directory
getwd()
# List the files in the working directory
list.files()

#Set working directory
setwd('~/ML')

#Read in csv files
avalInfo <- read.csv('avalanches_raw.csv')
weatInfo <- read.csv('altaGuardWeather6516.csv', na.strings = -9999)

#Make the "dates variable", dates
avalInfo$Date <- as.Date( as.character(avalInfo$Date), format="%m-%d-%y")

weatInfo$DATE <-as.Date(as.character(weatInfo$DATE), format="%Y%m%d")
# Check that they are dates
str(weatInfo$DATE)
str(avalInfo$Date)


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


#Fix Coordinates, so that they are split into lat and long
library(reshape2)
avalInfo$Coordinates <- colsplit(avalInfo$Coordinates,",", c("Latitude", "Longitude"))
#Replace 0's in Coordinates with NA
avalInfo$Coordinates$Latitude <- gsub("\\b0\\b","", avalInfo$Coordinates$Latitude)
avalInfo$Coordinates$Latitude <- as.numeric(as.character(avalInfo$Coordinates$Latitude))
avalInfo$Coordinates$Longitude <- as.numeric(as.character(gsub("\\b0\\b","", avalInfo$Coordinates$Longitude)))

# Create ordered data (in this case I ordered it so that NE is centered, because then it looks normal)
avalInfo$Aspect = factor(avalInfo$Aspect, levels=c("West", "Northwest", "North", "Northeast", "East", "Southeast", "South", ""), ordered = TRUE)



#Merge the two data sets by date
aw_df <- merge(avalInfo, weatInfo, by.x = "Date",all.x = TRUE, by.y="DATE", all.y = FALSE)

#Create data frame for caught, carried, and buried
ccb <- data.frame(date=aw_df_full$Date, Caught=aw_df_full$Caught, Carried=aw_df_full$Carried, Buried.Partly=aw_df_full$Buried...Partly, Buried.Fully=aw_df_full$Buried...Fully)
# Now use that data frame to gather column names into a key "Exposure" variable
ccb <- melt(ccb, id.vars="date", na.rm=T)
#Change column names
colnames(ccb)[2] <- "Exposure"
colnames(ccn)[3] <- "ExposureN"
#Now merge ccb to aw_df_full
aw_df_full <- merge(aw_df, ccb, by.x = "Date", by.y="date")

#Create data frame injured or killed
ik <- data.frame(date=aw_df_full$Date, Injured=aw_df_full$Injured , Killed=aw_df_full$Killed)
# Now use that data frame to gather column names into a key "BodilyHarm" variable
ik <- melt(ik, id.vars="date", na.rm=T)
#Change column names
colnames(ik)[2] <- "BodilyHarm"
colnames(ik)[3] <- "BodilyHarmN"
#Now merge ccb to aw_df_full
aw_df_full <- merge(aw_df_full, ik, by.x = "Date", by.y="date")


#Summarize data
summary(aw_df_full)
str(aw_df_full)


#Remove data that is missing at least 90% of its observations
aw_df_full$STATION <- NULL
aw_df_full$STATION_NAME <- NULL
aw_df_full$MDPR <- NULL
aw_df_full$MDSF <- NULL
aw_df_full$DAPR<- NULL
aw_df_full$DASF <- NULL
aw_df_full$WT01 <- NULL
aw_df_full$WT06 <- NULL
aw_df_full$WT05 <- NULL
aw_df_full$WT11 <- NULL
aw_df_full$WT04 <- NULL
aw_df_full$WT03 <- NULL
aw_df_full$Caught <- NULL
aw_df_full$Carried <- NULL
aw_df_full$Buried...Fully <- NULL
aw_df_full$Buried...Partly <- NULL
aw_df_full$Killed <- NULL
aw_df_full$Injured <- NULL



#Create data frame for injured and killed

# List data headings
names(aw_df_full)

#Attach aw_df_full to make life easier
attach(aw_df_full)

#Replace blank cells with NA

#Omitting N/A from aw data frame 
#Below results in deletion of all data.  I'll need to use Amelia...
#aw_df_omitNA <- na.exclude(aw_df_full)



#Load the library to plot with
library(ggplot2)

#Plots
#Plot Date
qplot(data = aw_df_full, x=Date, bins=296)

#Plot Region
qplot(data = aw_df_full, x=Region)

#Plot Place
qplot(data = aw_df_full, x=Place)

#Plot Trigger
qplot(data = aw_df_full, x=Trigger)

#Plot Weak Layer
qplot(data = aw_df_full, x=Weak.Layer)

#Plot Depth
ggplot(data = aw_df_full, aes(x=Depth))+
  geom_histogram(breaks=seq(0,120,1)) #+  xlim(c(0,60))

#Plot Width
ggplot(data = aw_df_full, aes(x=Width))+
  geom_histogram(breaks=seq(5,1000,10)) + xlim(c(0,500))

#Plot Vertical
ggplot(data = aw_df_full, aes(x=Vertical))+
  geom_histogram(breaks=seq(10,3500,10)) + xlim(c(10,2500))

#Plot Aspect
qplot(data = aw_df_full, x=Aspect)

#Plot of Elevation
ggplot(data = aw_df_full, aes(x=Elevation)) +
  geom_histogram( breaks=seq(7700,11200, 100)) +
  xlim(c(7700,11200))

#Plot Latitude
ggplot(data = aw_df_full, aes(x=Coordinates$Latitude))+
  geom_histogram(breaks=seq(40.38,42.8,0.01)) +
  xlim(c(40.38,42.08))

#Plot Longitude
ggplot(data = aw_df_full, aes(x=Coordinates$Longitude))+
  geom_histogram(breaks=seq(-112.25,-111.0,0.005))+
  xlim(c(-112.25,-111.0))

#Plot how many were caught/carried/Buried
ggplot(data = aw_df_full, aes(x=Depth))+
  geom_freqpoly(breaks=seq(1,120,14), aes(color=Swept))


# Frequency Plot with multiple lines
ggplot(data = avalInfo, aes(x=Elevation)) +
geom_freqpoly(aes(color=Aspect))

#Frequency Plot based on proportions
ggplot(aes(x=avalInfo$Elevation, y=(..count../sum(..count..))), data=avalInfo) +
  geom_freqpoly(aes(color=Aspect))+
  labs(y="Percentage of Avalanches")



#Box Plots

ggplot(avalInfo, aes(x=NoNA)) +
  geom_boxplot()


#Create a plot (histogram in this case) the facets makes a bunch of plots
qplot(data=avalInfo, x=Elevation, binwidth=150 )+
  facet_grid(Aspect~Elevation)

#Limit plot width (doesn't work, because x is factor see code below to change factors to numerics then rerun)
qplot(data=avalInfo, x=Elevation, binwidth=100)+
  #geom_histogram()+
  facet_grid(Aspect~Elevation, scales="free_x")+
  scale_x_continuous(limits=c(7700,11200))
#facet_wrap(~Elevation)

