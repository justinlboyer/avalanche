#This script will calculate probabilities of the data
#Source the necessary files
source(file="subsetNumAv_Dates.R" )

#Look at correlations
cor(numav[2:16], use = 'pairwise.complete.obs')
#There are some obvious ones but what we can read from this is because lat,long and elevation have decent correlation, this implies certain places avalanche more frequently
#Also precip,snowfall, and maxtemp seem to be most strongly associated with a rise in avalanche activity
#Lets look at the probability that something will avalanche given a precip,snowfall,maxtemp set
#First find ranges
range(numav$Preci.mean, na.rm = TRUE)
range(numav$Snowfall.mean, na.rm = TRUE)
range(numav$Max_Temperature.mean, na.rm = TRUE)
#Calculate probabilites for precipiation
pPre <- matrix(ncol=2,nrow = length(seq(0,5.5,0.5)))
s <- 1
for(i in seq(0,5.5,0.5)){
  pPre[s,1] <- i+0.5
  pPre[s,2]<-sum(numav$NumberOfAvalanches[numav$Preci.mean>i & numav$Preci.mean <= i+0.5], na.rm = TRUE)/sum(numav$NumberOfAvalanches, na.rm = TRUE)
  s <- s+1
}

#Calculate prob of aval given range of snowfall
pSnow <- matrix(ncol=2,nrow = length(seq(0,32)))
for(i in seq(0,32)){
  pSnow[i+1,1] <- i+1
  pSnow[i+1,2]<-sum(numav$NumberOfAvalanches[numav$Snowfall.mean>i & numav$Snowfall.mean <= i+1], na.rm = TRUE)/sum(numav$NumberOfAvalanches, na.rm = TRUE)
}

#Calculate prob of aval given range of max temp
pMxTemp <- matrix(ncol=2,nrow = length(seq(1,71,5)))
s <-1
for(i in seq(1,71,5)){
  pMxTemp[s,1] <- i+5
  pMxTemp[s,2]<-sum(numav$NumberOfAvalanches[numav$Max_Temperature.mean>i & numav$Max_Temperature.mean <= i+5], na.rm = TRUE)/sum(numav$NumberOfAvalanches, na.rm = TRUE)
  s <- s+1
}
#Crazy 70% of all avalanches occurr between 16 and 36 degrees, so if the max temp is between 60 and 90, dangerous conditions might exist

                                                                                   