#This file loops over various samples to create an estimate of the accuracy of the ridge regression model

#library(dplyr)

#~~~~ Try again but with defined function
train.index <- sample(nrow(binNumav), nrow(binNumav)*perTr, replace=FALSE)
print(summary(train.index))
print(rrfun(train.index, binNumav))
rm(train.index)
#








# 
# #Load necessary data frames
# source(file="subsetNumAv_Dates.R")
# #Clear unecessary data
# rm(avalInfo, aw_df, fl_df, no_out_dates_df, weatInfo, wndInfo)
# 
# #Create new data frame to use
# binNumav <- numav
# #Remove all values that cannot be used to predict
# binNumav$Date <- NULL
# binNumav$Depth.mean <- NULL
# binNumav$Width.mean <- NULL
# binNumav$Vertical.mean <- NULL
# binNumav <- na.omit(binNumav)
# 
# #Loop l times
# l <- 10
# out <- numeric(l)
# for (i in 1:l){
# #Create train and validate sets
#   perTr <- 0.9
#   for(s in 1:10){train.index <-sample(nrow(binNumav), nrow(binNumav)*perTr, replace=FALSE)}#train.index <- sample(nrow(binNumav), nrow(binNumav)*perTr, replace=FALSE)
#   print(summary(train.index))
#   tr <- binNumav[train.index,]
#   val <- binNumav[-train.index,]
#   #Construct model
#   x.tr2 <- model.matrix(NumberOfAvalanches ~ Preci.mean + SnowDepth.mean + Snowfall.mean + Max_Temperature.mean + Min_Temperature.mean + MaxWindSpeed.mean, data = tr)[,-1]
#   y.tr2 <- tr$NumberOfAvalanches
#   x.val2 <- model.matrix(NumberOfAvalanches ~ Preci.mean + SnowDepth.mean + Snowfall.mean + Max_Temperature.mean + Min_Temperature.mean + MaxWindSpeed.mean, data = val)[,-1]
#   y.val2 <- val$NumberOfAvalanches
#   #CV to obtain best lambda
#   set.seed(10)#
#   rr.cv2 <- cv.glmnet(x.tr2, y.tr2, alpha=0)
#   #plot(rr.cv2)
#   rr.bestlam <- rr.cv2$lambda.min
#   rr.goodlam <- rr.cv2$lambda.1se
#   # predict validation set using best lambda and calculate RMSE
#   rr.fit2 <- glmnet(x.tr2, y.tr2, alpha = 0)
#   #plot(rr.fit2, xvar = "lambda", label=TRUE)
#   rr.pred2 <- predict(rr.fit2, s=rr.bestlam, newx = x.val2)
#   #sqrt(mean(rr.pred2-y.val2)^2)
#   #=0.2433208
#   #Check accuracy
#   print(i)
#   #out[i,1] <- tr[1,1]
#   out[i] <- 1-(mean(trunc(rr.pred2) != val$NumberOfAvalanches))
#   rm(train.index, tr, val, x.tr2, y.tr2, rr.fit2)
# }
# 
# print(summary(out))