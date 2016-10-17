# This script trains a logistic regression model with the hopes of prediciting what situations are avalanche prone

#Load necessary data frames
source(file="subsetNumAvBinary.R")
#Clear unecessary data
rm(avalInfo, aw_df, fl_df, no_out_dates_df, weatInfo, wndInfo)

binumav <- na.omit(binumav)

#Visualize data
library(ggplot2)
ggplot(binumav, aes(y=Slid, x=Preci.mean)) + geom_point() + facet_wrap(~Max_Temperature.mean)
ggplot(binumav, aes(y=Slid, x=Snowfall.mean)) + geom_point() + facet_wrap(~Max_Temperature.mean)
ggplot(binumav, aes(y=Slid, x=Max_Temperature.mean)) + geom_point()
ggplot(binumav, aes(y=Slid, x=TempObs.mean)) + geom_point()

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
print(confusionMatrix(round(rr.pred2,0), val$Slid))


## Model is pretty good so save it and use in shiny app
save(rr.fit2, file = "bi_rr.fit_PSSTTWEA.rda")
save(rr.bestlam, file = "bi_rr.bestlam_PSSTTWEA.R")




###########################Previous code, for finding best model



#Check RMSE with just intercept
sqrt(mean((mean(tr$Slid)-val$Slid)^2))

#Test some simple linear models
lm.fit1 <- lm(Slid ~ Precipitation + Snowfall + Max_Temperature + Max_Wind_Speed, data = tr)
summary(lm.fit1)

lm.pred1 <- predict(lm.fit1, newdata = val)
sqrt(mean((lm.pred1-val$Slid)^2))


#Modeling using ridge regression 
library(glmnet)
x.tr <- model.matrix(Slid ~ ., data = tr)[,-1]
y.tr <- tr$Slid

x.val <- model.matrix(Slid ~ ., data = val)[,-1]
y.val <- val$Slid

#CV to obtain best lambda
set.seed(10)
rr.cv <- cv.glmnet(x.tr, y.tr, alpha=0)
plot(rr.cv)

rr.bestlam <- rr.cv$lambda.min
rr.goodlam <- rr.cv$lambda.1se

# predict validation set using best lambda and calculate RMSE
rr.fit <- glmnet(x.tr, y.tr, alpha = 0)
plot(rr.fit, xvar = "lambda", label=TRUE)

rr.pred <- predict(rr.fit, s=rr.bestlam, newx = x.val)
sqrt(mean(rr.pred-y.val)^2)



#Now lasso
set.seed(10)
las.cv <- cv.glmnet(x.tr, y.tr, alpha= 1)
plot(las.cv)

las.bestlam <- las.cv$lambda.min
las.goodlam <- las.cv$lambda.1se

#predic validation set using best lamda and calc RMSe
las.fit <- glmnet(x.tr, y.tr, alpha= 1)
plot(las.fit, xvar ="lambda", label = TRUE)

las.pred <- predict(las.fit, s = las.bestlam, newx = x.val)
sqrt(mean((las.pred - y.val)^2))


#Try logistic
mdl <-glm(tr$Slid~Precipitation + Snow_Depth + Snowfall + Max_Temperature + Min_Temperature + Max_Wind_Speed + Elevation + Aspect,family = quasibinomial(link = "logit"), data=tr)
summary(mdl)
sqrt(mean(predict(mdl,newdata = val)-val$Slid)^2)


## So Ridge Regression is best by far!!!