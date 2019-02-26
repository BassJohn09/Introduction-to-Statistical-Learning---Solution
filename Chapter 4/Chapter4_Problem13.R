# Chapter 4 - Problem 13 

# Using the Boston data set, fit classification models in order to predict
# whether a given suburb has a crime rate above or below the median.
# Explore logistic regression, LDA, and KNN models using various subsets
# of the predictors. Describe your findings.

require(ISLR)

crim01 <- ifelse(Boston$crim >median(Boston$crim),1,0) 
Boston$crim01 <- crim01
cor(Boston)
boxplot(medv~crim01,data = Boston)

#Important Variables: zn+indus+nox+age+dis+rad+tax+lstat+medv
#Non Important: chas+rm+black+ptratio

#Indus - Nox - age are somewhat correlated
#Tax and Rad are highly correlated
#lstat and medv are correlated

# Function to get data

# (a) Logistic regresion

test.selection <- sample(1:nrow(Boston),76,replace = TRUE) # 15% of data used for test
test.set <- Boston[test.selection,]
training.set <- Boston[-test.selection,]
logit.model <- glm(crim01 ~ indus+rad+age+medv,data = training.set)
summary(logit.model)

logit.prob <- predict(logit.model,newdata = test.set, type = "response")
logit.pred <- rep(0,nrow(test.set))
logit.pred[logit.prob > 0.5] <- 1

logit.cm <- table(logit.pred,test.set$crim01)
logit.error <- mean(logit.pred != test.set$crim01)

# (b) LDA

library(MASS)
lda.model <- lda(crim01 ~ nox+rad+age+medv,data = training.set)
lda.model

lda.pred <- predict(lda.model,newdata = test.set,type = "response")
lda.cm <- table(lda.pred$class,test.set$crim01)
lda.error <- mean(lda.pred$class != test.set$crim01)

# (c) QDA
qda.model <- qda(crim01 ~ nox+rad+age+medv,data = training.set)
qda.model

qda.pred <- predict(qda.model,newdata = test.set,type = "response")
qda.cm <- table(qda.pred$class,test.set$crim01)
qda.error <- mean(qda.pred$class != test.set$crim01)

# (d) KNN 

training.set.knn <- cbind(training.set$nox,training.set$rad,
                          training.set$age,training.set$medv)
test.set.knn <- cbind(test.set$nox,test.set$rad,
                      test.set$age,test.set$medv)
training.response.knn <- as.factor(training.set$crim01)
test.response.knn <- as.factor(test.set$crim01)

library(class)

knn.pred <- knn(training.set.knn,test.set.knn,training.response.knn, k=8)

knn.cm <- table(knn.pred,test.response.knn)
knn.error <- mean(knn.pred != test.response.knn)
