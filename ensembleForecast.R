# This script produces many models then uses them to predict the probability of avalanche
#Load files and prep them
source(file="subsetNumAvBinary.R")
source(file="rrfunBi.R")
library(caret)
library(glmnet)
#Clear unecessary data
rm(avalInfo, aw_df, fl_df, no_out_dates_df, weatInfo, wndInfo)

#Keep only Wasatch region
binumav <- binumav[binumav$Region=="Salt Lake"|binumav$Region=="Logan"|binumav$Region=="Ogden"|binumav$Region=="Provo",]

#Remove unused columns (data imputed or with too many NA's)
binumav$Region <- NULL
binumav$Place <- NULL
binumav$Aspect <- NULL
binumav$Elevation <- NULL
binumav$Max_Wind_Speed <- NULL

binumav <- na.omit(binumav)


i <- 10 #Size of ensemble
fit <- list()
bestlam <- numeric(i)
acc <- list()

perTr <- 0.5 # Percent of data to train
train.matrix <- matrix(, nrow = i, ncol = nrow(binumav)*perTr)
for (i in 1:i) {
  train.matrix[i,] <- sample(nrow(binumav), nrow(binumav)*perTr, replace=TRUE) 
}
for (j in 1:i) {
  fit[[j]]<-rrfunBi(train.matrix[j,], binumav)[[1]]
  bestlam[j]<-rrfunBi(train.matrix[j,], binumav)[[2]]
  acc[[j]]<-rrfunBi(train.matrix[j,], binumav)[[3]]
}
# Now use each model to provide a prediction
av <- numeric(i)
for (l in 1:i) {
  av[l] <- predict(fit[[l]],s=bestlam[[l]],newx = t(x.val2[19,]))
}
table(av)
print(max(table(round(av)))/length(av))

#Save ridge regression ensemble
#save(fit, file = "ensembleRRFit.rda")
#save(bestlam, file = "ensembleBestLam.R")



print(summary(out))
print(sd(out))
hist(out)