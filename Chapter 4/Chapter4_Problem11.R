# Chapter 4 - Problem 11

# (a) Create a binary variable, mpg01, that contains a 1 if mpg contains
# a value above its median, and a 0 if mpg contains a value below
# its median. You can compute the median using the median()
# function. Note you may find it helpful to use the data.frame()
# function to create a single data set containing both mpg01 and
# the other Auto variables.

require(ISLR)
library(magrittr)
library(ggplot2)

mpg01 <- ifelse(Auto$mpg >= median(Auto$mpg), 1,0)
Auto$mpg01 <- mpg01

# (b) Explore the data graphically in order to investigate the association
# between mpg01 and the other features. Which of the other
# features seem most likely to be useful in predicting mpg01? Scatterplots
# and boxplots may be useful tools to answer this question.
# Describe your findings.

pairs(mpg01 ~ horsepower+weight+displacement+year,data = Auto)
pairs(mpg01 ~ cylinders+acceleration+origin,data = Auto)

boxplot(Auto$mpg01,Auto$acceleration)
boxplot(Auto$mpg01,Auto$year)

Auto.meanVals <- aggregate(Auto$horsepower, list(Auto$mpg01,Auto$origin),mean)
colnames(Auto.meanVals) <- c("mpg01","Origin","Horsepower")

Auto.meanVals$mpg01 <- as.factor(Auto.meanVals$mpg01)

ggplot(Auto.meanVals,aes(x = Horsepower, y = Origin,group = mpg01))+
  geom_line(aes(linetype=mpg01, color= mpg01))+
  geom_point()+
  theme(legend.position="top")

# mpg01 is related to horsepower + accelaration+origin

# (c) Split the data into a training set and a test set.

test.selection <- base::sample(1:nrow(Auto),size = round(nrow(Auto)*0.1,0),replace = TRUE)
test.set <- Auto[test.selection,]
training.set <- Auto[-test.selection,]

# (d) Perform LDA on the training data in order to predict mpg01
# using the variables that seemed most associated with mpg01 in
# (b). What is the test error of the model obtained?

library(MASS)
lda.model <- lda(mpg01 ~ horsepower+acceleration+origin+year,data = training.set)
lda.model

lda.pred <- predict(lda.model,newdata = test.set)
lda.cm <- table(lda.pred$class,test.set$mpg01)
mean(lda.pred$class != test.set$mpg01)

# Low error - 0.07692!!

# (e) Perform QDA on the training data in order to predict mpg01
# using the variables that seemed most associated with mpg01 in
# (b). What is the test error of the model obtained?

qda.model <- qda(mpg01 ~ horsepower+acceleration+origin+year,data = training.set)
qda.model

qda.pred <- predict(qda.model,newdata = test.set)
qda.cm <- table(qda.pred$class,test.set$mpg01)
mean(qda.pred$class != test.set$mpg01)

# Slightly worse error - 0.1025641

# Perform logistic regression on the training data in order to predict
# mpg01 using the variables that seemed most associated with
# mpg01 in (b). What is the test error of the model obtained?

logit.model <- glm(mpg01 ~ horsepower+acceleration+year+origin,
                   data = training.set,family = "binomial")
summary(logit.model)

logit.probs <- predict(logit.model,newdata = test.set,type = "response")
logit.pred <- rep(0,length(logit.probs))
logit.pred[logit.probs >.5] <- 1
logit.cm <- table(logit.pred,test.set$mpg01)
mean(logit.pred != test.set$mpg01)

#Same as LDA - 0.07692

# (g) Perform KNN on the training data, with several values of K, in
# order to predict mpg01. Use only the variables that seemed most
# associated with mpg01 in (b). What test errors do you obtain?
#   Which value of K seems to perform the best on this data set?

training.set.knn <- cbind(training.set$horsepower,training.set$acceleration,
                          training.set$year,training.set$origin) 
test.set.knn <- cbind(test.set$horsepower,test.set$acceleration,
                       test.set$year,test.set$origin)
training.class.knn <- as.factor(training.set$mpg01)
test.class.knn <- as.factor(test.set$mpg01)

library(class)
knn.pred <- knn(training.set.knn,test.set.knn,training.class.knn,k=15)

knn.cm <- table(knn.pred,test.class.knn)
mean(knn.pred != test.class.knn)
