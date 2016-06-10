#Subset the data based on the number of avalanches that occured by date
#This script subsets the data so that probabilities may be assesed
#Load data and clean it
#source(file='loadData.R')
source(file="subsetDates.R" )


#Create data frame that contains the number of avalanches that have occured
numav <- no_out_dates_df
library(plyr)
#library(dplyr)
temp <- ddply(avalInfo,.(Date),nrow)
colnames(temp)[2] <- "NumberOfAvalanches"
#Check that all the dates got encoded
length(avalInfo$Date)==sum(temp$NumberOfAvalanches)
#Now remove the dates deemed outliers (Any dates pre 2004)
temp <- temp[grep("200[4-9]-|201[0-6]",temp$Date),]
#Remove any NAs in Date
temp <- temp[!is.na(temp$Date),]


#Create data frame with single dates
numav <- ddply(numav, .(Date), summarise, Depth.mean=mean(Depth),Width.mean=mean(Width),Vertical.mean=mean(Vertical),Elevation.mean=mean(Elevation),Lat.mean=mean(Latitude),Long.mean=mean(Longitude),Preci.mean=mean(Precipitation),SnowDepth.mean=mean(Snow_Depth),Snowfall.mean=mean(Snowfall),Max_Temperature.mean=mean(Max_Temperature),Min_Temperature.mean=mean(Min_Temperature),TempObs.mean=mean(Temperature_at_observation_time), AvgWindSpeed.mean=mean(Average_Wind_Speed),MaxWindSpeed.mean=mean(Max_Wind_Speed))
numav <- merge(numav, temp, by.x = 'Date', by.y = 'Date', all.x = TRUE, all.y = TRUE)
#Remove the temp df
rm(temp)
#Replace NA with 0 in number of avalanches column
numav$NumberOfAvalanches[is.na(numav$NumberOfAvalanches)] <- 0
#Set values with NA and number of avalanches=0 to 0
numav$Depth.mean[numav$NumberOfAvalanches==0] <- 0
numav$Width.mean[numav$NumberOfAvalanches==0] <- 0
numav$Vertical.mean[numav$NumberOfAvalanches==0] <- 0
numav$Elevation.mean[numav$NumberOfAvalanches==0] <- 0
numav$Lat.mean[numav$NumberOfAvalanches==0] <- 0
numav$Long.mean[numav$NumberOfAvalanches==0] <- 0



#Remove values that are no longer needed: i, store
#rm(i, store)

