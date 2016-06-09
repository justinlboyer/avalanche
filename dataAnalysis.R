# Data analysis
#Load data
source(file="loadData.R" )


#Look at boxplots of avalanche weather and non avalanche weather
# Precipitation is a big deal
boxplot(fl_df$Precipitation, fl_df$Precipitation[!is.na(fl_df$Place)])
summary(fl_df$Precipitation)
summary(fl_df$Precipitation[!is.na(fl_df$Place)])
# The probability an avalanche occurs when it snows more than 0.21mm is:
length(fl_df$Precipitation[which(fl_df$Precipitation[!is.na(fl_df$Place)]>=quantile(fl_df$Precipitation[!is.na(fl_df$Place)], 0.5, na.rm = TRUE))])/length(fl_df$Precipitation[!is.na(fl_df$Place)])
#Compared to the probability it snows more than 0.21mm during the avalanche months
length(fl_df$Precipitation[which(fl_df$Precipitation >= quantile(fl_df$Precipitation[!is.na(fl_df$Place)], 0.5, na.rm = TRUE))])/length(fl_df$Precipitation)

boxplot(fl_df$Snowfall, fl_df$Snowfall)
boxplot(fl_df$Snow_Depth, fl_df$Snow_Depth)
boxplot(fl_df$Max_Temperature, fl_df$Max_Temperature)
boxplot(fl_df$Min_Temperature, fl_df$Min_Temperature)
boxplot(fl_df$Temperature_at_observation_time, fl_df$Temperature_at_observation_time)
boxplot(fl_df$Average_Wind_Speed, fl_df$Average_Wind_Speed)
boxplot(fl_df$Max_Wind_Speed, fl_df$Max_Wind_Speed)

#Quick look at things that might be associated
#As min temp increases so does the liklihood of bodily injury
summary(lm(fl_df_ik$BodilyHarmN~fl_df_ik$TMIN))
#The ones below don't have much statistical significance
summary(lm(fl_df_ik$BodilyHarmN~fl_df_ik$TMAX))
summary(lm(fl_df_ik$BodilyHarmN~fl_df_ik$PRCP))
summary(lm(fl_df_ik$BodilyHarmN~fl_df_ik$Elevation))
summary(lm(fl_df_ik$BodilyHarmN~fl_df_ik$SNWD))
summary(lm(fl_df_ik$BodilyHarmN~fl_df_ik$SNOW))
summary(lm(fl_df_ik$BodilyHarmN~fl_df_ik$TMAX))
summary(lm(fl_df_ik$BodilyHarmN~fl_df_ik$TOBS))

#As min/max/observed temp increases going for a ride decreases, this data might be skewed because of the difference in the magnitudes of the numbers
summary(lm(TypeOfRideN~TMIN, data = fl_df_ccb))
summary(lm(TypeOfRideN~TMAX, data = fl_df_ccb))
summary(lm(TypeOfRideN~TOBS, data = fl_df_ccb))
#As precipitation/Snowfall/Depth of snow increases going for a ride increases
summary(lm(TypeOfRideN~PRCP, data = fl_df_ccb))
summary(lm(TypeOfRideN~SNOW, data = fl_df_ccb))
summary(lm(TypeOfRideN~SNWD, data = fl_df_ccb))
summary(lm(TypeOfRideN~TMIN, data = fl_df_ccb))
summary(lm(TypeOfRideN~TMIN, data = fl_df_ccb))
summary(lm(TypeOfRideN~TMAX, data = fl_df_ccb))
summary(lm(TypeOfRideN~TOBS, data = fl_df_ccb))
#As precipitation/Snowfall/Depth of snow increases going for a ride increases
summary(lm(TypeOfRideN~PRCP, data = fl_df_ccb))
summary(lm(TypeOfRideN~SNOW, data = fl_df_ccb))
#The association below has an ok residual error compared to the others
summary(lm(TypeOfRideN~SNWD, data = fl_df_ccb))

#As width of the avalanche increase, so does depth
summary(lm(Depth~Width, data = avalInfo))
#As elevation increases depth/width/vertical of avalanche increases
summary(lm(Depth~Elevation, data = avalInfo))
summary(lm(Depth~Width, data = avalInfo))
summary(lm(Vertical~Elevation, data = avalInfo))

#As Precipitation increases so does depth/width/vertical
summary(lm(Depth~PRCP, data = fl_df))
summary(lm(Width~PRCP, data = fl_df)) #With good significance levels and rejection of null
summary(lm(Vertical~PRCP, data = fl_df)) #With good significance levels and rejection of null
#As Snow depth increases so does depth/width/vertical/elevation
summary(lm(Depth~SNWd, data = fl_df))
summary(lm(Width~SNWD, data = fl_df)) #With good significance levels and rejection of null
summary(lm(Vertical~SNWD, data = fl_df)) #With good significance levels and rejection of null
summary(lm(Elevation~SNWD, data = fl_df)) #With good significance levels and rejection of null
#As Snowfall increases so does depth/width/vertical/elevation
summary(lm(Depth~SNOW, data = fl_df)) 
summary(lm(Width~SNOW, data = fl_df)) #With good significance levels and rejection of null
summary(lm(Vertical~SNOW, data = fl_df)) #Can't reject null 
summary(lm(Elevation~SNOW, data = fl_df)) #Can't reject null 
#As min temp increases so does depth/width/vertical/elevation



#Load the library to plot with
library(ggplot2)
#Plots
#Plot Date
qplot(data = fl_df, x=Date, bins=296)
#Plot Region
qplot(data = fl_df, x=Region)
#Plot Place
qplot(data = fl_df, x=Place)
#Plot Trigger
qplot(data = fl_df, x=Trigger)
#qplot(data = avalInfo, x=Trigger)
#Plot Weak Layer
qplot(data = fl_df, x=Weak.Layer)
#qplot(data = avalInfo, x=Weak.Layer)
#Plot Depth
ggplot(data = fl_df, aes(x=Depth))+
  geom_histogram(breaks=seq(0,120,1)) #+  xlim(c(0,60))
#Plot Width
ggplot(data = fl_df, aes(x=Width))+
  geom_histogram(breaks=seq(5,1000,10)) + xlim(c(0,500))
#Plot Vertical
ggplot(data = fl_df, aes(x=Vertical))+
  geom_histogram(breaks=seq(10,3500,10)) + xlim(c(10,2500))
#Plot Aspect
qplot(data = fl_df, x=Aspect)
#Plot of Elevation
ggplot(data = fl_df, aes(x=Elevation)) +
  geom_histogram( breaks=seq(7700,11200, 100)) +
  xlim(c(7700,11200))
#Plot Latitude
ggplot(data = fl_df, aes(x=Coordinates$Latitude))+
  geom_histogram(breaks=seq(40.38,42.8,0.01)) +
  xlim(c(40.38,42.08))
#Plot Longitude
ggplot(data = fl_df, aes(x=Coordinates$Longitude))+
  geom_histogram(breaks=seq(-112.25,-111.0,0.005))+
  xlim(c(-112.25,-111.0))
#Plot how many were caught/carried/Buried
qplot(data = fl_df_ccb, x=TypeOfRide)
#Plot how many were injured or killed
qplot(data=fl_df_ik, x=BodilyHarm)
#Plot Precipitation
ggplot(data=fl_df, aes(x=Precipitation))+
  geom_histogram(breaks=seq(0, 3, 0.05)) #+ xlim(c(0.0001,3))
#Plot Snow depth
ggplot(fl_df, aes(x=Snow_Depth))+
  geom_histogram(breaks=seq(0,168,1))
#Plot Snowfall
ggplot(fl_df, aes(x=Snowfall)) +
  geom_histogram(breaks=seq(0,32,0.2)) #+xlim(.01,22)
#Plot max temperature
ggplot(fl_df, aes(x=Max_Temperature))+
  geom_histogram(breaks=seq(1,61,1))
#Plot minimum temperature
ggplot(fl_df, aes(x=Min_Temperature))+
  geom_histogram(breaks=seq(-13,40,1))
#Plot temperature at time of observation
ggplot(fl_df, aes(x=Temperature_at_observation_time))+
  geom_histogram()
# Plot average wind speed
ggplot(fl_df, aes(x=Average_Wind_Speed))+
  geom_histogram(breaks=seq(0, 6, .1))
# Plot Highest wind speed
ggplot(fl_df, aes(x=Max_Wind_Speed))+
  geom_histogram(breaks=seq(0,31.6,.1))

