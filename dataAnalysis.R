# Data analysis
#Load data
source(file="loadData.R" )


#Quick look at things that might be associated
#As min temp increases so does the liklihood of bodily injury
summary(lm(aw_df_ik$BodilyHarmN~aw_df_ik$TMIN))
#The ones below don't have much statistical significance
summary(lm(aw_df_ik$BodilyHarmN~aw_df_ik$TMAX))
summary(lm(aw_df_ik$BodilyHarmN~aw_df_ik$PRCP))
summary(lm(aw_df_ik$BodilyHarmN~aw_df_ik$Elevation))
summary(lm(aw_df_ik$BodilyHarmN~aw_df_ik$SNWD))
summary(lm(aw_df_ik$BodilyHarmN~aw_df_ik$SNOW))
summary(lm(aw_df_ik$BodilyHarmN~aw_df_ik$TMAX))
summary(lm(aw_df_ik$BodilyHarmN~aw_df_ik$TOBS))

#As min/max/observed temp increases going for a ride decreases, this data might be skewed because of the difference in the magnitudes of the numbers
summary(lm(TypeOfRideN~TMIN, data = aw_df_ccb))
summary(lm(TypeOfRideN~TMAX, data = aw_df_ccb))
summary(lm(TypeOfRideN~TOBS, data = aw_df_ccb))
#As precipitation/Snowfall/Depth of snow increases going for a ride increases
summary(lm(TypeOfRideN~PRCP, data = aw_df_ccb))
summary(lm(TypeOfRideN~SNOW, data = aw_df_ccb))
summary(lm(TypeOfRideN~SNWD, data = aw_df_ccb))
summary(lm(TypeOfRideN~TMIN, data = aw_df_ccb))
summary(lm(TypeOfRideN~TMIN, data = aw_df_ccb))
summary(lm(TypeOfRideN~TMAX, data = aw_df_ccb))
summary(lm(TypeOfRideN~TOBS, data = aw_df_ccb))
#As precipitation/Snowfall/Depth of snow increases going for a ride increases
summary(lm(TypeOfRideN~PRCP, data = aw_df_ccb))
summary(lm(TypeOfRideN~SNOW, data = aw_df_ccb))
#The association below has an ok residual error compared to the others
summary(lm(TypeOfRideN~SNWD, data = aw_df_ccb))

#As width of the avalanche increase, so does depth
summary(lm(Depth~Width, data = avalInfo))
#As elevation increases depth/width/vertical of avalanche increases
summary(lm(Depth~Elevation, data = avalInfo))
summary(lm(Depth~Width, data = avalInfo))
summary(lm(Vertical~Elevation, data = avalInfo))

#As Precipitation increases so does depth/width/vertical
summary(lm(Depth~PRCP, data = aw_df))
summary(lm(Width~PRCP, data = aw_df)) #With good significance levels and rejection of null
summary(lm(Vertical~PRCP, data = aw_df)) #With good significance levels and rejection of null
#As Snow depth increases so does depth/width/vertical/elevation
summary(lm(Depth~SNWd, data = aw_df))
summary(lm(Width~SNWD, data = aw_df)) #With good significance levels and rejection of null
summary(lm(Vertical~SNWD, data = aw_df)) #With good significance levels and rejection of null
summary(lm(Elevation~SNWD, data = aw_df)) #With good significance levels and rejection of null
#As Snowfall increases so does depth/width/vertical/elevation
summary(lm(Depth~SNOW, data = aw_df)) 
summary(lm(Width~SNOW, data = aw_df)) #With good significance levels and rejection of null
summary(lm(Vertical~SNOW, data = aw_df)) #Can't reject null 
summary(lm(Elevation~SNOW, data = aw_df)) #Can't reject null 
#As min temp increases so does depth/width/vertical/elevation



#Load the library to plot with
library(ggplot2)
#Plots
#Plot Date
qplot(data = aw_df, x=Date, bins=296)
#Plot Region
qplot(data = aw_df, x=Region)
#Plot Place
qplot(data = aw_df, x=Place)
#Plot Trigger
qplot(data = aw_df, x=Trigger)
#qplot(data = avalInfo, x=Trigger)
#Plot Weak Layer
qplot(data = aw_df, x=Weak.Layer)
#qplot(data = avalInfo, x=Weak.Layer)
#Plot Depth
ggplot(data = aw_df, aes(x=Depth))+
  geom_histogram(breaks=seq(0,120,1)) #+  xlim(c(0,60))
#Plot Width
ggplot(data = aw_df, aes(x=Width))+
  geom_histogram(breaks=seq(5,1000,10)) + xlim(c(0,500))
#Plot Vertical
ggplot(data = aw_df, aes(x=Vertical))+
  geom_histogram(breaks=seq(10,3500,10)) + xlim(c(10,2500))
#Plot Aspect
qplot(data = aw_df, x=Aspect)
#Plot of Elevation
ggplot(data = aw_df, aes(x=Elevation)) +
  geom_histogram( breaks=seq(7700,11200, 100)) +
  xlim(c(7700,11200))
#Plot Latitude
ggplot(data = aw_df, aes(x=Coordinates$Latitude))+
  geom_histogram(breaks=seq(40.38,42.8,0.01)) +
  xlim(c(40.38,42.08))
#Plot Longitude
ggplot(data = aw_df, aes(x=Coordinates$Longitude))+
  geom_histogram(breaks=seq(-112.25,-111.0,0.005))+
  xlim(c(-112.25,-111.0))
#Plot how many were caught/carried/Buried
qplot(data = aw_df_ccb, x=TypeOfRide)
#Plot how many were injured or killed
qplot(data=aw_df_ik, x=BodilyHarm)
#Plot Precipitation
ggplot(data=aw_df, aes(x=PRCP))+
  geom_histogram(breaks=seq(0, 3, 0.05)) #+ xlim(c(0.0001,3))
#Plot Snow depth
ggplot(aw_df, aes(x=SNWD))+
  geom_histogram(breaks=seq(0,168,1))
#Plot Snowfall
ggplot(aw_df, aes(x=SNOW)) +
  geom_histogram(breaks=seq(0,32,0.2)) #+xlim(.01,22)
#Plot max temperature
ggplot(aw_df, aes(x=TMAX))+
  geom_histogram(breaks=seq(1,61,1))
#Plot minimum temperature
ggplot(aw_df, aes(x=TMIN))+
  geom_histogram(breaks=seq(-13,40,1))
#Plot temperature at time of observation
ggplot(aw_df, aes(x=TOBS))+
  geom_histogram()
