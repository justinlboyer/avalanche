#This script subsets the data so that probabilities may be assesed
#Load data and clean it
source(file="loadData.R" )



#Create data frame injured or killed
ik <- data.frame(date=aw_df$Date, Injured=aw_df$Injured , Killed=aw_df$Killed)
# Now use that data frame to gather column names into a key "BodilyHarm" variable
library(reshape2)
ik <- melt(ik, id.vars="date", na.rm=T)
#Change column names
colnames(ik)[2] <- "BodilyHarm"
colnames(ik)[3] <- "BodilyHarmN"
#Now merge ik to aw_df creates too many duplicates
ik <- merge(aw_df, ik, by.x = "Date", all.x = FALSE, by.y="date", all.y=TRUE)
#Remove all non harmed observations
ik <- ik[!is.na(ik$BodilyHarmN),]
#Remove unessecary columns
ik$Killed <- NULL
ik$Injured <- NULL
