#Subset the data based on the number of avalanches that occured by date
#This script subsets the data so that probabilities may be assesed
#Load data and clean it
#source(file='loadData.R')
source(file="subsetDates.R")

#Create df that contains all conditions in which avalanches occur
binumav <- no_out_dates_df
library(dplyr)
binumav <- mutate(binumav, Slid = ifelse(is.na(Region)==1,0,1))

#Subset dates into month and day(TBD)

#Make list of variables to keep
keeps <-c("Region","Place","Aspect","Elevation","Precipitation","Snow_Depth","Snowfall","Max_Temperature","Min_Temperature","Max_Wind_Speed","Slid")
binumav <- binumav[keeps]

#Impute NA's with a random selction of values
#Set NA's in Aspect to random aspect, so that aspect is usable
#Aspect
Ns <- length(binumav$Aspect[is.na(binumav$Aspect)])
binumav$Aspect[is.na(binumav$Aspect)] <- sample(levels(binumav$Aspect)[2:9],Ns, replace = TRUE)
Bls <- length(binumav$Aspect[binumav$Aspect==""])
binumav$Aspect[binumav$Aspect==""] <- sample(levels(binumav$Aspect)[2:9],Bls, replace = TRUE)
#Region
binumav$Region[is.na(binumav$Region)] <- sample(levels(binumav$Region),length(binumav$Region[is.na(binumav$Region)]),replace = TRUE)
#Place
binumav$Place[is.na(binumav$Place)] <- sample(levels(binumav$Place),length(binumav$Place[is.na(binumav$Place)]),replace = TRUE)
#Elevation
binumav$Elevation[is.na(binumav$Elevation)] <- sample(seq(5100,12400,200),length(binumav$Elevation[is.na(binumav$Elevation)]),replace = TRUE)

#Remove values that are no longer needed: i, store
rm(Bls, Ns, keeps)

detach("package:dplyr",unload = TRUE)

