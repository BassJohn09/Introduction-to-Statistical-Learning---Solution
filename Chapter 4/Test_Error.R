
# Scenarios

# KNN Classification  
require(ISLR)
library(class)

#Since data has different magnitudes, scaling is necessary 
standard.X <- scale(Caravan[,-86])
standard.Y <- Caravan[,86]

# K- Nearest Neighbors
n <- 1000
test.vector <- sample(1:nrow(Caravan),n, replace = FALSE)
training.X <- standard.X[-c(test.vector),]
test.X <- standard.X[test.vector,]
training.Y <- standard.Y[-c(test.vector)]
test.Y <- standard.Y[test.vector]

KNN.pred <- knn(training.X,test.X,training.Y,k=3)
mean(test.Y != KNN.pred)
mean(test.Y == KNN.pred)

KNN.cross <- table(KNN.pred,test.Y)

Error.knn <- (KNN.cross[1,2]+KNN.cross[2,1])/n

# Logistic Regression
glm.fit <- glm(Purchase ~ .,data = Caravan, family = "binomial",subset = -test.vector) 
summary(glm.fit) 
glm.probs <- predict(glm.fit,newdata = Caravan[test.vector,],type = "response")
glm.probs[1:5] # Close to 0.5 
glm.pred <- ifelse(glm.probs > 0.5,"Yes","No")
logit.cross <- table(glm.pred,test.Y) 

Error.logreg <- (logit.cross[1,2]+logit.cross[2,1])/n
