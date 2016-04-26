# Find working directory
getwd()
# List the files in the working directory
list.files()

#Set working directory
setwd('~/ML')

#Read in csv files
avalInfo <- read.csv('AvalanchesInSLC_Raw.csv')
weatInfo <- read.csv('altaGuardWeather6516.csv', na.strings = -9999)

#Look at a subset of data
subset(  avalInfo, Aspect=='Northeast')

#Another way to view a subset of data then save it
avalInfo[avalInfo$Aspect=='Northeast',]

#Look at data type
class(avalInfo$Region)

#Make the "dates variable", dates
avalInfo$Date <- as.Date( as.character(avalInfo$Date), format="%m-%d-%y")


weatInfo$DATE <-as.Date(as.character(weatInfo$DATE), format="%Y%m%d")
# Check that they are dates
str(weatInfo$DATE)
str(avalInfo$Date)


#Merge the two data sets by date
aw_df_full <- merge(avalInfo, weatInfo, by.x = "Date", by.y="DATE")


#View counts of variables and stuff
table(avalInfo$Aspect)

#Summarize data
summary(aw_df_full)

#Look at various names for all the variables factors
levels(avalInfo$Aspect)

#Make the dates variable dates
tmpDate <- as.Date(avalInfo$Date, format="%m-%d-%Y")
avalInfo$Date <- tmpDate

#Load the library to plot with
library(ggplot2)
#Create a plot (histogram in this case) the facets makes a bunch of plots
qplot(data=avalInfo, x=Elevation, binwidth=150 )+
  facet_grid(Aspect~Elevation)
# Create ordered data (in this case I ordered it so that NE is cenetered)
avalInfo$Aspect = factor(avalInfo$Aspect, levels=c("West", "Northwest", "North", "Northeast", "East", "Southeast", "South", ""), ordered = TRUE)
avalInfo$Elevation = factor(avalInfo$Elevation, levels=c("", "11200","11100", "11000", "10900", "10800", "10700", "10600", "10500", "10400", "10300", "10200", "10100", "10000", "9900", "9800", "9700", "9600", "9500", "9400", "9300", "9200", "9100", "9000", "8900", "8800", "8700", "8600", "8500", "8400", "8300", "8200", "8100", "8000", "7900", "7800", "7700", "7600", "7500", "7400", "7300", "7200", "7100", "7000", "6900", "6800", "6700", "6100", "5600", "5100"), ordered = TRUE)

#Limit plot width (doesn't work, because x is factor see code below to change factors to numerics then rerun)
qplot(data=avalInfo, x=Elevation, binwidth=100)+
  #geom_histogram()+
  facet_grid(Aspect~Elevation, scales="free_x")+
  scale_x_continuous(limits=c(7700,11200))
  #facet_wrap(~Elevation)

#Plot with breaks
ggplot(data = avalInfo, aes(x=Elevation)) +
  geom_histogram( breaks=seq(7700,11200, 100)) +
  xlim(c(7700,11200))
  
# Frequency Plot with multiple lines
ggplot(data = avalInfo, aes(x=Elevation)) +
geom_freqpoly(aes(color=Aspect))

#FRequency Plot based on proportions
ggplot(aes(x=avalInfo$Elevation, y=(..count../sum(..count..))), data=avalInfo) +
  geom_freqpoly(aes(color=Aspect))+
  labs(y="Percentage of Avalanches")

# List data headings
names(avalInfo)

#Replace blank cells with NA

#Omitting N/A from Elevation data
NoNA <- subset(avalInfo, !is.na(avalInfo$Elevation))

#Convert factors to numerics
avalInfo$Elevation <- as.numeric(as.character(avalInfo$Elevation))




#Box Plots

ggplot(avalInfo, aes(x=NoNA)) +
  geom_boxplot()

