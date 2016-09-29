# This script trains a logistic regression model with the hopes of prediciting what situations are avalanche prone

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

bkupBNMV <- binNumav

#Build df with a random assortment of 903 NoA 0s
test<-sample(nrow(binNumav[binNumav$NumberOfAvalanches==0,]), 903, replace=FALSE)
binNumav <-binNumav[binNumav$NumberOfAvalanches != 0,]
binNumav <- rbind(binNumav,bkupBNMV[test,])
rm(test, bkupBNMV)



#Set all number avalanaches to 0 or 1
#binNumav$NumberOfAvalanches <- ifelse(binNumav$NumberOfAvalanches == 0, 0,1)

#Visualize data
# library(ggplot2)
# ggplot(binNumav, aes(y=NumberOfAvalanches, x=Preci.mean)) + geom_point() + facet_wrap(~Max_Temperature.mean)
# ggplot(binNumav, aes(y=NumberOfAvalanches, x=Snowfall.mean)) + geom_point() + facet_wrap(~Max_Temperature.mean)
# ggplot(binNumav, aes(y=NumberOfAvalanches, x=Max_Temperature.mean)) + geom_point()
# ggplot(binNumav, aes(y=NumberOfAvalanches, x=TempObs.mean)) + geom_point()

#Perhaps build model  off anything with r2>3%
#So Preci,Snowfall,MaxTemp,TempObs

#Split data into train and validate
perTr <- 0.9 #Percent of data to split into training set
train.index <-sample(nrow(binNumav), nrow(binNumav)*perTr, replace=FALSE)
tr <- binNumav[train.index,]
val <- binNumav[-train.index,]

#Check RMSE with just intercept
sqrt(mean((mean(tr$NumberOfAvalanches)-val$NumberOfAvalanches)^2))
#=2.311731

#Test some simple linear models
lm.fit1 <- lm(NumberOfAvalanches ~ Preci.mean + Snowfall.mean + Max_Temperature.mean + TempObs.mean, data = tr)
summary(lm.fit1)

lm.pred1 <- predict(lm.fit1, newdata = val)
sqrt(mean((lm.pred1-val$NumberOfAvalanches)^2))
#=2.0753

#Try all varis
lm.fit2 <- lm(NumberOfAvalanches ~ ., data = tr)
summary(lm.fit2)

lm.pred2 <- predict(lm.fit2, newdata = val)
sqrt(mean((lm.pred2-val$NumberOfAvalanches)^2))
#=2.055108


#Modeling using ridge regression ~~Need to read about~~
library(glmnet)
x.tr <- model.matrix(NumberOfAvalanches ~ ., data = tr)[,-1]
y.tr <- tr$NumberOfAvalanches

x.val <- model.matrix(NumberOfAvalanches ~ ., data = val)[,-1]
y.val <- val$NumberOfAvalanches

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
#=0.3778392


#Modeling using ridge regression BUT with variables which are available on day to day
x.tr2 <- model.matrix(NumberOfAvalanches ~ Preci.mean + SnowDepth.mean + Snowfall.mean + Max_Temperature.mean + Min_Temperature.mean + MaxWindSpeed.mean + Aspect.count, data = tr)[,-1]
y.tr2 <- tr$NumberOfAvalanches

x.val2 <- model.matrix(NumberOfAvalanches ~ Preci.mean + SnowDepth.mean + Snowfall.mean + Max_Temperature.mean + Min_Temperature.mean + MaxWindSpeed.mean + Aspect.count, data = val)[,-1]
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
sqrt(mean(rr.pred2-y.val2)^2)
#=0.388666

#Check accuracy
misClassError <- mean(trunc(rr.pred2) != val$NumberOfAvalanches)
print(paste('Accuracy', 1-misClassError))

## Model is pretty good so save it and use in shiny app
save(rr.fit2, file = "mostZrr.fit_PSSTTWA.rda")
save(rr.bestlam, file = "mostZrr.bestlam.R")

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
#=2.094529 Not better than linear

#Try logistic
# Make dependent value binary for purposes of logistic regression
tr$NumberOfAvalanches <- ifelse(tr$NumberOfAvalanches==0, 0, 1)
val$NumberOfAvalanches <- ifelse(val$NumberOfAvalanches==0, 0, 1)

mdl <-glm(tr$NumberOfAvalanches~.,family = quasibinomial(link = "logit"), data=tr)
summary(mdl)


## So Ridge Regression is best by far!!!

