#Subset the data based on the number of avalanches that occured by date
#This script subsets the data so that probabilities may be assesed
#Load data and clean it
#source(file='loadData.R')
source(file="subsetDates.R")


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

#Remove NAs in Aspect
#numav <-numav[!is.na(numav$Aspect),]



#Create data frame with single dates (updated to include aspect)
numav <- ddply(numav, .(Date), summarise, Depth.mean=mean(Depth),Width.mean=mean(Width),Vertical.mean=mean(Vertical),Elevation.mean=mean(Elevation),Lat.mean=mean(Latitude),Long.mean=mean(Longitude),Preci.mean=mean(Precipitation),SnowDepth.mean=mean(Snow_Depth),Snowfall.mean=mean(Snowfall),Max_Temperature.mean=mean(Max_Temperature),Min_Temperature.mean=mean(Min_Temperature),TempObs.mean=mean(Temperature_at_observation_time), AvgWindSpeed.mean=mean(Average_Wind_Speed),MaxWindSpeed.mean=mean(Max_Wind_Speed),Aspect.count = sample(Aspect, size = 1)) #ifelse(length(names(table(Aspect)[which(table(numav$Aspect)==max(table(Aspect)))]))>1, "Northeast",names(table(Aspect)[which(table(numav$Aspect)==max(table(Aspect)))]))
numav <- merge(numav, temp, by.x = 'Date', by.y = 'Date', all.x = TRUE, all.y = TRUE)
#Remove the temp df
rm(temp)
#Replace NA with 0 in number of avalanches column
numav$NumberOfAvalanches[is.na(numav$NumberOfAvalanches)] <- 0
#Set values with NA and number of avalanches=0 to 0
numav$Depth.mean[numav$NumberOfAvalanches==0] <- 0
numav$Width.mean[numav$NumberOfAvalanches==0] <- 0
numav$Vertical.mean[numav$NumberOfAvalanches==0] <- 0
numav$Lat.mean[numav$NumberOfAvalanches==0] <- 0
numav$Long.mean[numav$NumberOfAvalanches==0] <- 0

#Randomize elevations for 0 avalanches
samelev <- sample(nrow(numav[numav$Elevation.mean!=0,]), nrow(numav[numav$NumberOfAvalanches==0,]), replace=FALSE)
numav$Elevation.mean[numav$NumberOfAvalanches==0] <- numav$Elevation.mean[samelev]


#Set NA's in Aspect to random aspect, so that aspect is usable
Ns <- length(numav$Aspect.count[is.na(numav$Aspect.count)])
numav$Aspect.count[is.na(numav$Aspect.count)] <- sample(levels(numav$Aspect.count)[2:9],Ns, replace = TRUE)
Bls <- length(numav$Aspect.count[numav$Aspect.count==""])
numav$Aspect.count[numav$Aspect.count==""] <- sample(levels(numav$Aspect.count)[2:9],Bls, replace = TRUE)

#Center data
#First save all means
DepthMean <- mean(numav$Depth.mean, na.rm=TRUE)
WidthMean <- mean(numav$Width.mean, na.rm=TRUE)
ElevMean <- mean(numav$Elevation.mean, na.rm=TRUE)
PrecMean <- mean(numav$Preci.mean, na.rm=TRUE)
SnowdMean <- mean(numav$SnowDepth.mean, na.rm=TRUE)
SnowfMean <- mean(numav$Snowfall.mean, na.rm=TRUE)
MxTMean <- mean(numav$Max_Temperature.mean, na.rm=TRUE)
MnTMean <- mean(numav$Min_Temperature.mean, na.rm=TRUE)
MxWMean <- mean(numav$MaxWindSpeed.mean, na.rm=TRUE)
NoAMean <- mean(numav$NumberOfAvalanches, na.rm=TRUE)
#Create a vector and save it
daMeans <- c(DepthMean,WidthMean,ElevMean,PrecMean,SnowdMean,SnowfMean,MxTMean,MnTMean,MxWMean,NoAMean)
save(daMeans, file = "TheMeans.R")

numav$Depth.mean <- scale(numav$Depth.mean, center = T, scale = F)
numav$Width.mean <- scale(numav$Width.mean, center = T, scale = F)
numav$Elevation.mean <- scale(numav$Elevation.mean, center = T, scale = F)
numav$Preci.mean <- scale(numav$Preci.mean, center = T, scale = F)
numav$SnowDepth.mean <- scale(numav$SnowDepth.mean, center = T, scale = F)
numav$Snowfall.mean <- scale(numav$Snowfall.mean, center = T, scale = F)
numav$Max_Temperature.mean <- scale(numav$Max_Temperature.mean, center = T, scale = F)
numav$Min_Temperature.mean <- scale(numav$Min_Temperature.mean, center = T, scale = F)
numav$MaxWindSpeed.mean <- scale(numav$MaxWindSpeed.mean, center = T, scale = F)
#numav$NumberOfAvalanches <- scale(numav$NumberOfAvalanches, center = T, scale = F)

#Remove values that are no longer needed: i, store
rm(Bls, Ns)

