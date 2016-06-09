#This script subsets the data so that probabilities may be assesed
#Load data and clean it
source(file="loadData.R" )



#Create data frame for caught, carried, and buried
ccb <- data.frame(date=aw_df$Date, Caught=aw_df$Caught, Carried=aw_df$Carried, BuriedPartly=aw_df$BuriedPartly, BuriedFully=aw_df$BuriedFully)
# Now use that data frame to gather column names into a key "TypeOfRide" variable
library(reshape2)
ccb <- melt(ccb, id.vars="date", na.rm=T)
#Change column names
colnames(ccb)[2] <- "TypeOfRide"
colnames(ccb)[3] <- "TypeOfRideN"
#Now merge ccb to aw_df
ccb <- merge(aw_df, ccb, by.x = "Date", all.x=FALSE, by.y="date", all.y=TRUE)
#Remove any rows not containg type of ride information
ccb <- ccb[!is.na(ccb$TypeOfRideN),]
#Remove unessecary columns
ccb$Caught <- NULL
ccb$Carried <- NULL
ccb$Buried...Fully <- NULL
ccb$Buried...Partly <- NULL


