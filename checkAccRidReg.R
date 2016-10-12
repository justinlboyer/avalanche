#This file loops over various samples to create an estimate of the accuracy of the ridge regression model
i <- 200
out <- numeric(i)
train.matrix <- matrix(, nrow = i, ncol = nrow(binNumav)*.9)
for (i in 1:i) {
    train.matrix[i,] <- sample(nrow(binNumav), nrow(binNumav)*.9, replace=TRUE) 
}
for (j in 1:i) {
  out[j]<-rrfunAspEl(train.matrix[j,], binNumav)
}
print(summary(out))
print(sd(out))
hist(out)


#Check specificity and sensitivity
#Recode so all non zero avalanches are 1
# ssdf <- binNumav
# ssdf$NumberOfAvalanches[ssdf$NumberOfAvalanches != 0]<-1
# perTr <- 0.8 #Percent of data to split into training set
# train.index <-sample(nrow(ssdf), nrow(ssdf)*perTr, replace=FALSE)
# tr <- ssdf[train.index,]
# val <- ssdf[-train.index,]
# #Load model
# load("some0rr.fit_PSSTTWEA.rda")
# load("some0rr.bestlam_PSSTTWEA.R")
# #plot(rr.fit2, xvar = "lambda", label=TRUE)
# rr.pred2 <- predict(rr.fit2, s=rr.bestlam, newx = x.val2)
# rr.pred2[rr.pred2 <0] <- 0
# rr.pred2 <- tanh(rr.pred2)
# val$NumberOfAvalanches[val$NumberOfAvalanches!=0]<-1
# library(caret)
#sensitivity(rr.pred2,val$NumberOfAvalanches)
confusionMatrix(round(rr.pred2), val$NumberOfAvalanches)

