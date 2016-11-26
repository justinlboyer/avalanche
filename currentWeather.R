#This script pulls in the current weather conditions at the Alta Guard Station

# My mesowest token: 1b407db951f84b7dabd5eb0f5e408faf
currentWeather <- function() {
  yourl <-
    url(
      "http://api.synopticlabs.org/v2/stations/timeseries?&token=1b407db951f84b7dabd5eb0f5e408faf&units=english&recent=1440&output=csv&stid=AGD&status=active&vars=air_temp,snow_depth,precip_accum,snow_interval&varoperator=and"
    )
  currAGD_df <-
    read.csv(
      yourl,
      header = TRUE,
      sep = ",",
      comment.char = "#",
      skip = 8,
      col.names = c(
        "Station_Name",
        "Date_time",
        "air_temp",
        "snow_depth",
        "precip_accum",
        "snow_interval"
      )
    )
  # #Change factors to numeric
  # currAGD_df$air_temp <- as.numeric(currAGD_df$air_temp)
  # currAGD_df$snow_depth <- as.numeric(currAGD_df$snow_depth)
  # currAGD_df$precip_accum <- as.numeric(currAGD_df$precip_accum)
  # currAGD_df$snow_interval <-as.numeric(currAGD_df$snow_interval)
  
  
  currMinTemp <- min(currAGD_df$air_temp)
  currMaxTemp <- max(currAGD_df$air_temp)
  currSNWD <- median(currAGD_df$snow_depth)
  currPrecip <- median(currAGD_df$precip_accum)
  currSNFL <- median(currAGD_df$snow_interval)
  return(c(currMinTemp, currMaxTemp, currSNWD, currPrecip, currSNFL))
}
