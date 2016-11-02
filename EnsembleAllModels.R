#This script loads all the models then produces a result
load("ensembleBestLam.R")
load("ensembleRRFit.rda")
load("ensembleLasBestLam.R")
load("ensembleLasso.rda")
load("ensembleLinear.rda")
load("ensembleLogistic.rda")
#load("interceptFit.R")


#Get input
x.inDF <- data.frame(x.val2)
colnames(x.inDF) <- c("Precipitation","Snow_Depth","Snowfall","Max_Temperature","Min_Temperature")
x.in <- x.val2

#Initialize Prediction vector
pred <- matrix(nrow = length(val$Slid),ncol = 13)

#Predict logistic
pred[,1] <- predict(mdl,newdata = x.inDF)

#Predict Lasso
pred[,2] <- predict(las.fit, s = las.bestlam, newx = x.in)

#Predict Linear
pred[,3] <- predict.lm(lm.fit1, newdata = x.inDF)

#Predict Intercept
#pred[,4] <- interceptFit

#Predict Using 10 RR models
for (l in 4:13) {
  pred[,l] <- predict(fit[[l-3]],s=bestlam[[l-3]],newx = x.in)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Restrict values between 0 and 1
pred[pred<0] <- 0
pred <- tanh(pred)

#Output result
result <- numeric(length(val$Slid))
for (i in 1:length(val$Slid)) {
  result[i] <- Mode(round(pred[i,]))
}

#Check accuracy of overall model
overallAcc <- confusionMatrix(round(result,0), val$Slid)
#save(overallAcc, file = "EnsembleForecastAccuracy.R")
