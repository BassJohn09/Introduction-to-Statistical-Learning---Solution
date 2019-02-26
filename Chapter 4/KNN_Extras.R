#KNN
require(class)
library(caret)

standardized.X <- scale(Kickstarter.train[,c(11,15,16)])
train.X <- cbind(standardized.X,Kickstarter.train[,c(4,12)])
dmy <- dummyVars(" ~ .", data = train.X)
train.X <- data.frame(predict(dmy, newdata = train.X))

standardized.X <- scale(Kickstarter.test[,c(11,15,16)])
test.X <- cbind(standardized.X,Kickstarter.test[,c(4,12)])
dmy <- dummyVars(" ~ .", data = test.X)
test.X <- data.frame(predict(dmy, newdata = test.X))

train.Y <- Kickstarter.train$state
test.Y <- Kickstarter.test$state
knn.pred <- knn(train.X,test.X,train.Y, k = 2) #closest in Euclidean distance
table(knn.pred,test.Y)
