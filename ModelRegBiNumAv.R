# This script trains a logistic regression model with the hopes of prediciting what situations are avalanche prone

#Load necessary data frames
source(file="subsetNumAvBinary.R")
#Clear unecessary data
rm(avalInfo, aw_df, fl_df, no_out_dates_df, weatInfo, wndInfo)

binumav <- na.omit(binumav)

#Visualize data
# library(ggplot2)
# ggplot(binumav, aes(y=Slid, x=Preci.mean)) + geom_point() + facet_wrap(~Max_Temperature.mean)
# ggplot(binumav, aes(y=Slid, x=Snowfall.mean)) + geom_point() + facet_wrap(~Max_Temperature.mean)
# ggplot(binumav, aes(y=Slid, x=Max_Temperature.mean)) + geom_point()
# ggplot(binumav, aes(y=Slid, x=TempObs.mean)) + geom_point()

#Perhaps build model  off anything with r2>3%
#So Preci,Snowfall,MaxTemp,TempObs

#Split data into train and validate
perTr <- 0.9 #Percent of data to split into training set
train.index <-sample(nrow(binumav), nrow(binumav)*perTr, replace=FALSE)
tr <- binumav[train.index,]
val <- binumav[-train.index,]

library(glmnet)
#Modeling using ridge regression BUT with variables which are available on day to day
x.tr2 <- model.matrix(Slid ~ Precipitation + Snow_Depth + Snowfall + Max_Temperature + Min_Temperature, data = tr)[,-1]
y.tr2 <- tr$Slid

x.val2 <- model.matrix(Slid ~ Precipitation + Snow_Depth + Snowfall + Max_Temperature + Min_Temperature, data = val)[,-1]
y.val2 <- val$Slid

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
sqrt(mean(rr.pred2-y.val2)^2)

#Check accuracy
misClassError <- mean(round(rr.pred2) != val$Slid)
print(paste('Accuracy', 1-misClassError))
library(caret)
rracc <- confusionMatrix(round(rr.pred2,0), val$Slid)


## Model is pretty good so save it and use in shiny app
save(rr.fit2, file = "ensembleSingleRR.rda")
save(rr.bestlam, file = "ensembleSingleRRBestLam.R")


#Check RMSE with just intercept
sqrt(mean((mean(tr$Slid)-val$Slid)^2))
interceptFit <- mean(tr$Slid)
save(interceptFit,file =  "interceptFit.R")

#Test some simple linear models
lm.fit1 <- lm(Slid ~ Precipitation + Snow_Depth + Snowfall + Max_Temperature + Min_Temperature, data = tr)
summary(lm.fit1)

lm.pred1 <- predict(lm.fit1, newdata = val)
sqrt(mean((lm.pred1-val$Slid)^2))

## Check accuracy
lmacc <- confusionMatrix(round(lm.pred1,0), val$Slid)

#Save linear model
save(lm.fit1, file = "ensembleLinear.rda")





#Now lasso
set.seed(10)
las.cv <- cv.glmnet(x.tr2, y.tr2, alpha= 1)
plot(las.cv)

las.bestlam <- las.cv$lambda.min
las.goodlam <- las.cv$lambda.1se

#predic validation set using best lamda and calc RMSe
las.fit <- glmnet(x.tr2, y.tr2, alpha= 1)
plot(las.fit, xvar ="lambda", label = TRUE)

las.pred <- predict(las.fit, s = las.bestlam, newx = x.val2)
sqrt(mean((las.pred - y.val2)^2))

## Check accuracy
lassacc <- confusionMatrix(round(las.pred,0), val$Slid)

#Save Lasso
save(las.fit, file = "ensembleLasso.rda")
save(las.bestlam, file = "ensembleLasBestLam.R")




#Try logistic
mdl <-glm(tr$Slid~Precipitation + Snow_Depth + Snowfall + Max_Temperature + Min_Temperature,family = binomial(link = "logit"), data=tr)
summary(mdl)
sqrt(mean(predict(mdl,newdata = val)-val$Slid)^2)
logpred <- predict(mdl,newdata = val)
logpred <- tanh(logpred)
logpred[logpred < 0] <- 0
## Check accuracy
logacc <- confusionMatrix(round(logpred,0), val$Slid)

#Save logistic model
save(mdl, file = "ensembleLogistic.rda")


#Save all the analysis on accuracy
save(logacc, file = "LogisticAccuracy.R")
save(lmacc, file = "LinearModelAccuracy.R")
save(lassacc, file = "LassoAccuracy.R")
save(rracc, file = "RidgeAccuracy.R")

