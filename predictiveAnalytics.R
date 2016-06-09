#Try to use predictive analytics only on Salt lake data
#Load data
source(file="loadData.R")


#Salt lake data
sl_fl_df <- subset(numav, numav$Region =='Salt Lake' | numav$Region=='(Other)' )#| is.na(numav$Region) )
#Remove categorical variables, create quantitiative data frame
sl_qu_df <- sl_fl_df[,c('Date','Depth','Width','Vertical','Elevation', 'Latitude', 'Longitude', 'Precipitation', 'Snow_Depth', 'Snowfall', 'Max_Temperature','Min_Temperature', 'Temperature_at_observation_time', 'Average_Wind_Speed','NumberOfAvalanches')]
#Set up train and test data
testidx <- which(1:nrow(sl_qu_df) %% 4 == 0)
av_train <- sl_qu_df[-testidx,]
av_test <- sl_qu_df[testidx,]
#Set up analysis
model <- lm(NumberOfAvalanches~., data = av_train)
#Use model to predic the output of test data
prediction <- predict(model, newdata = av_test)
#Check for the correlation with actual result
cor(prediction,av_test$NumberOfAvalanches, use = 'complete.obs')


#Try classification
