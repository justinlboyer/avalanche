#This script subsets the data so that probabilities may be assesed
#Load data and clean it
source(file="loadData.R" )


#Remove all dates/months for which we do no avalanche info
#First find out the range of the months in which avalanches occur
x <- gsub("^[0-9]{4}-([0-9][0-9])-[0-9]{2}$",'\\1',avalInfo$Date)
x <- as.numeric(as.character(na.omit(x)))
#Identify outlier months
library(outliers)
#check different arrangements for outlier months
poss.outlier <- c()
for(i in 6:12){
  poss.outlier <- c(outlier(x %% i),poss.outlier)
}
#Check possible outliers, measure against percentage of data
percen <- c()
for(i in unique(poss.outlier)){
  percen <- c(percen,length(x[x==i])/length(x))
}
#Now we can see that may (0.855% of the data), june (0.132% of the data) and october (0.132% of the data) are outliers, so remove those months
#Remove months dates from 06-10 inclusive
outlier_dates_df <- fl_df[grep("-[01][67890]-", fl_df$Date),]
#Same for years
#First find out the range of the years in which avalanches have been recorded
y <- na.omit(gsub("^([0-9]{4})-[0-9][0-9]-[0-9]{2}$",'\\1',avalInfo$Date))
y <- as.numeric(y)
#Check possible outliers, measure against percentage of data
percen <- c()
for(i in unique(poss.outlier)){
  percen <- c(percen,length(x[x==i])/length(x))
}
#Create a vector that contains the years that need to be removed
outlier.years <- unique(boxplot.stats(y)$out)

#Remove all the years in ery
outlier_dates_df <- outlier_dates_df[grep(paste(outlier.years,collapse = "|"), outlier_dates_df$Date, invert=TRUE),]

