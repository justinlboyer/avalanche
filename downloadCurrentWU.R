#This script downloads the current weather conditions from weather underground

library(httr)

smple1 <- GET("http://api.wunderground.com/api/a31cc0a0ae79091f/geolookup/q/UT/Alta.json")