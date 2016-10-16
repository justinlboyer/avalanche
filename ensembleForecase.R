# This script produces many models then uses them to predict the probability of avalanche
library(caret)
library(glmnet)
#Load files and prep them
source(file="subsetNumAvBinary.R")
source(file="rrfunBi.R")
#Clear unecessary data
rm(avalInfo, aw_df, fl_df, no_out_dates_df, weatInfo, wndInfo,keeps)

binumav <- na.omit(binumav)

i <- 25 #Size of ensemble
fit <- list()
bestlam <- numeric(i)
acc <- list()
train.matrix <- matrix(, nrow = i, ncol = nrow(binumav)*.9)
for (i in 1:i) {
  train.matrix[i,] <- sample(nrow(binumav), nrow(binumav)*.9, replace=TRUE) 
}
for (j in 1:i) {
  fit[[j]]<-rrfunBi(train.matrix[j,], binumav)[[1]]
  bestlam[j]<-rrfunBi(train.matrix[j,], binumav)[[2]]
  acc[[j]]<-rrfunBi(train.matrix[j,], binumav)[[3]]
}
# Now use each model to provide a prediction
av <- numeric(i)
for (l in 1:i) {
  av[l] <- predict(fit[[l]],s=bestlam[[l]],newx = t(test[8,]))
}
print(max(table(round(av)))/length(av))
table(av)




print(summary(out))
print(sd(out))
hist(out)