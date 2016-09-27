#This script creates a function to test ridge regression

rrfun <- function(ti, df){
  #Remove all values that cannot be used to predict
  df$Date <- NULL
  df$Depth.mean <- NULL
  df$Width.mean <- NULL
  df$Vertical.mean <- NULL
  df <- na.omit(df)
#Create train and validate sets
  #train.index <- sample(nrow(df), nrow(df)*perTr, replace=FALSE)
  tr <- df[ti,]
  val <- df[-ti,]
  #Construct model
  x.tr2 <- model.matrix(NumberOfAvalanches ~ Preci.mean + SnowDepth.mean + Snowfall.mean + Max_Temperature.mean + Min_Temperature.mean + MaxWindSpeed.mean, data = tr)[,-1]
  y.tr2 <- tr$NumberOfAvalanches
  x.val2 <- model.matrix(NumberOfAvalanches ~ Preci.mean + SnowDepth.mean + Snowfall.mean + Max_Temperature.mean + Min_Temperature.mean + MaxWindSpeed.mean, data = val)[,-1]
  y.val2 <- val$NumberOfAvalanches
  #CV to obtain best lambda
  set.seed(10)#
  rr.cv2 <- cv.glmnet(x.tr2, y.tr2, alpha=0)
  #plot(rr.cv2)
  rr.bestlam <- rr.cv2$lambda.min
  rr.goodlam <- rr.cv2$lambda.1se
  # predict validation set using best lambda and calculate RMSE
  rr.fit2 <- glmnet(x.tr2, y.tr2, alpha = 0)
  #plot(rr.fit2, xvar = "lambda", label=TRUE)
  rr.pred2 <- predict(rr.fit2, s=rr.bestlam, newx = x.val2)
  #sqrt(mean(rr.pred2-y.val2)^2)
  #=0.2433208
  #Check accuracy
  #out[i,1] <- tr[1,1]
  out <- 1-(mean(trunc(rr.pred2) != val$NumberOfAvalanches))
  #rm(rr.pred2, val, x.tr2, x.val2, perTr, rr.bestlam, rr.cv2,rr.fit2,rr.goodlam,s,train.index, y.tr2,y.val2)
  return(out)
  }
