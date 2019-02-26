# K-Nearest Neighbors

require(ISLR)
library(class) # class where k-nearest neighbors
?knn #Notice it does not take a formula

Xfeatures <- cbind(Smarket$Lag1,Smarket$Lag2,Smarket$Year)

train <- Smarket$Year < 2005
TestSet <- Xfeatures[!train,]
TrainSet <- Xfeatures[train,]
Direction <- Smarket$Direction[train]

knn.pred <- knn(TrainSet,TestSet,Direction, k = 1) #closest in Euclidean distance
table(knn.pred,Smarket$Direction[!train])
mean(Smarket$Direction[!train] == knn.pred) #Random chance

