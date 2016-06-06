#Subset the data based on the number of avalanches that occured by date
#This script subsets the data so that probabilities may be assesed
#Load data and clean it
#source(file='loadData.R')
source(file="subsetDates.R" )


#Create data frame that contains the number of avalanches that have occured
numav <- no_out_dates_df
library(plyr)
library(dplyr)
temp <- ddply(avalInfo,.(Date),nrow)
colnames(temp)[2] <- "NumberOfAvalanches"
#Check that all the dates got encoded
length(avalInfo$Date)==sum(temp$NumberOfAvalanches)

#Create data frame with single dates
numav <- ddply(numav, .(Date), summarise, Depth.mean=mean(Depth),Width.mean=mean(Width),Vertical.mean=mean(Vertical),Elevation.mean=mean(Elevation),Lat.mean=mean(Latitude),Long.mean=mean(Longitude),Preci.mean=mean(Precipitation),SnowDepth.mean=mean(Snow_Depth),Snowfall.mean=mean(Snowfall),Max_Temperature.mean=mean(Max_Temperature),Min_Temperature.mean=mean(Min_Temperature),TempObs.mean=mean(Temperature_at_observation_time), AvgWindSpeed.mean=mean(Average_Wind_Speed),MaxWindSpeed.mean=mean(Max_Wind_Speed))
numav <- merge(numav, temp, by.x = 'Date', by.y = 'Date', all.x = TRUE, all.y = TRUE)
#Replace NA with 0 in number of avalanches column
numav$NumberOfAvalanches[is.na(numav$NumberOfAvalanches)] <- 0
#Set values with NA and number of avalanches=0 to 0
numav$Depth[numav$NumberOfAvalanches==0] <- 0
numav$Width[numav$NumberOfAvalanches==0] <- 0
numav$Vertical[numav$NumberOfAvalanches==0] <- 0
numav$Elevation[numav$NumberOfAvalanches==0] <- 0
numav$Latitude[numav$NumberOfAvalanches==0] <- 0
numav$Longitude[numav$NumberOfAvalanches==0] <- 0

#Check that there are enough avalanches
store =0
for (i in 1:100){
  store = store + sum(numav$NumberOfAvalanches[numav$NumberOfAvalanches==i])
}
print(store>=length(avalInfo[avalInfo$Region=='Sal',]))

