# This script produces many models then uses them to predict the probability of avalanche
#Load files and prep them
source(file="subsetNumAvBinary.R")
source(file="rrfunBi.R")
library(caret)
library(glmnet)
#Clear unecessary data
rm(avalInfo, aw_df, fl_df, no_out_dates_df, weatInfo, wndInfo)

binumav <- na.omit(binumav)
#Keep only salt lake region
binumav <- binumav[binumav$Region=="Salt Lake",]
#Remove unused columns
binumav$Region <- NULL
binumav$Place <- NULL

i <- 5 #Size of ensemble
fit <- list()
bestlam <- numeric(i)
acc <- list()

perTr <- 0.6 # Percent of data to train
train.matrix <- matrix(, nrow = i, ncol = nrow(binumav)*perTr)
for (i in 1:i) {
  train.matrix[i,] <- sample(nrow(binumav), nrow(binumav)*perTr, replace=TRUE) 
}
for (j in 1:i) {
  fit[[j]]<-rrfunBi(train.matrix[j,], binumav)[[1]]
  bestlam[j]<-rrfunBi(train.matrix[j,], binumav)[[2]]
  print(bestlam[j])
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