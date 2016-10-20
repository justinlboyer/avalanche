#This script loads all the models then produces a result
load("ensembleBestLam.R")
load("ensembleRRFit.rda")
load("ensembleLasBestLam.R")
load("ensembleLasso.rda")
load("ensembleLinear.rda")
load("ensembleLogistic.rda")


#Get input
x.inDF <- data.frame(t(x.val2[14,]))
x.in <- t(x.val2[14,])

#Initialize Prediction vector
pred <- numeric(13)

#Predict logistic
pred[1] <- predict(mdl,newdata = x.inDF)

#Predict Lasso
pred[2] <- predict(las.fit, s = las.bestlam, newx = x.in)

#Predict Linear
pred[3] <- predict.lm(lm.fit1, newdata = x.inDF)

#Predict Using 10 RR models
for (l in 4:13) {
  pred[l] <- predict(fit[[l-3]],s=bestlam[[l-3]],newx = x.in)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Restrict values between 0 and 1
pred[pred<0] <- 0
pred <- tanh(pred)

#Output result
result <- Mode(round(pred))
#Output probability
Prob <- max(table(round(pred)))/length(pred)
