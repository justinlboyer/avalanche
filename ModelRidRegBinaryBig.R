# This script trains a ridge regression model with the hopes of prediciting what situations are avalanche prone

#Load necessary data frames
source(file="subsetNumAv_Dates.R")
#Clear unecessary data
rm(avalInfo, aw_df, fl_df, no_out_dates_df, weatInfo, wndInfo)

#Create new data frame to use
binNumav <- numav
#Remove all values that cannot be used to predict
binNumav$Date <- NULL
binNumav$Depth.mean <- NULL
binNumav$Width.mean <- NULL
binNumav$Vertical.mean <- NULL
binNumav <- na.omit(binNumav)
#Build df with a random assortment of l NoA 0s
# l <- 310
# test<-sample(nrow(binNumav[binNumav$NumberOfAvalanches==0,]), l, replace=FALSE)
# bkupBNMV <- binNumav
# binNumav <- binNumav[binNumav$NumberOfAvalanches != 0,]
# binNumav <- rbind(binNumav,bkupBNMV[test,])
# rm(test, bkupBNMV)

#Set up binary on number of avalanches
binNumav$NumberOfAvalanches[round(binNumav$NumberOfAvalanches)!=0] <- 1
#If using scale in subsetNumAv_Dates need line below
#binNumav$NumberOfAvalanches[round(binNumav$NumberOfAvalanches)!=-round(NoAMean)] <- 1



#Split data into train and validate
perTr <- 0.9 #Percent of data to split into training set
train.index <-sample(nrow(binNumav), nrow(binNumav)*perTr, replace=FALSE)
tr <- binNumav[train.index,]
val <- binNumav[-train.index,]

#Modeling using ridge regression ~~Need to read about~~
library(glmnet)
#Modeling using ridge regression BUT with variables which are available on day to day
x.tr2 <- model.matrix(NumberOfAvalanches ~ Preci.mean + SnowDepth.mean + Snowfall.mean + Max_Temperature.mean + Min_Temperature.mean + MaxWindSpeed.mean + Elevation.mean + Aspect.count, data = tr)[,-1]
y.tr2 <- tr$NumberOfAvalanches

x.val2 <- model.matrix(NumberOfAvalanches ~ Preci.mean + SnowDepth.mean + Snowfall.mean + Max_Temperature.mean + Min_Temperature.mean + MaxWindSpeed.mean + Elevation.mean + Aspect.count, data = val)[,-1]
y.val2 <- val$NumberOfAvalanches

#CV to obtain best lambda
set.seed(10)
rr.cv2 <- cv.glmnet(x.tr2, y.tr2, alpha=0)
plot(rr.cv2)

rr.bestlam <- rr.cv2$lambda.min
rr.goodlam <- rr.cv2$lambda.1se

# predict validation set using best lambda and calculate RMSE
rr.fit2 <- glmnet(x.tr2, y.tr2, alpha = 0)
plot(rr.fit2, xvar = "lambda", label=TRUE)

rr.pred2 <- predict(rr.fit2, s=rr.bestlam, newx = x.val2)
#Squash all values between 0 and 1
rr.pred2[rr.pred2 <0] <- 0
rr.pred2 <- tanh(rr.pred2)
val$NumberOfAvalanches[val$NumberOfAvalanches!=0]<-1

sqrt(mean(rr.pred2-val$NumberOfAvalanches)^2)


#Squash all values between 0 and 1
rr.pred2[rr.pred2 <0] <- 0
rr.pred2 <- tanh(rr.pred2)

#Check accuracy
print(confusionMatrix(round(rr.pred2,0), val$NumberOfAvalanches))

misClassError <- mean(round(rr.pred2,0) != round(val$NumberOfAvalanches,0))
print(paste('Accuracy', 1-misClassError))

## Model is pretty good so save it and use in shiny app
save(rr.fit2, file = "some0rr.fit_PSSTTWEA_test.rda")
save(rr.bestlam, file = "some0rr.bestlam_PSSTTWEA_test.R")
#save(rr.fit2, file = "some0rr.fit_PSSTTWEA.rda")
#save(rr.bestlam, file = "some0rr.bestlam_PSSTTWEA.R")
