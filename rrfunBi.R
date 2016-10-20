#This script creates a function to test ridge regression

rrfunBi <- function(ti, df){

#Create train and validate sets
  #train.index <- sample(nrow(df), nrow(df)*perTr, replace=FALSE)
  tr <- df[ti,]
  val <- df[-ti,]
  #Construct model
  x.tr2 <- model.matrix(Slid ~ Precipitation + Snow_Depth + Snowfall + Max_Temperature + Min_Temperature, data = tr)[,-1]
  y.tr2 <- tr$Slid
  x.val2 <- model.matrix(Slid ~ Precipitation + Snow_Depth + Snowfall + Max_Temperature + Min_Temperature, data = val)[,-1]
  y.val2 <- val$Slid
  #CV to obtain best lambda
  set.seed(10)#
  rr.cv2 <- cv.glmnet(x.tr2, y.tr2, alpha=0)
  rr.bestlam <- rr.cv2$lambda.min
  rr.goodlam <- rr.cv2$lambda.1se
  # predict validation set using best lambda and calculate RMSE
  rr.fit2 <- glmnet(x.tr2, y.tr2, alpha = 0)
  rr.pred2 <- predict(rr.fit2, s=rr.bestlam, newx = x.val2)
  #Force all output values between 0 and 1
  rr.pred2[rr.pred2 <0] <- 0
  rr.pred2 <- tanh(rr.pred2)
  #Check accuracy
  acc <- confusionMatrix(round(rr.pred2), val$Slid)
  stuff <- list(rr.fit2,rr.bestlam,acc)
  return(stuff)
  }
