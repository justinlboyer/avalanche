#This file loops over various samples to create an estimate of the accuracy of the ridge regression model

out <- numeric(100)
train.matrix <- matrix(, nrow = 100, ncol = 1083)
for (i in 1:100) {
    train.matrix[i,] <- sample(nrow(binNumav), nrow(binNumav)*.9, replace=TRUE) 
}
for (j in 1:100) {
  out[j]<-rrfun(train.matrix[j,], binNumav)
}
