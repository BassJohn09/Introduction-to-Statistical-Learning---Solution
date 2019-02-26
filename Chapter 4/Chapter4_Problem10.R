# Chapter 4 - Problem 10
# This question should be answered using the Weekly data set, which
# is part of the ISLR package. This data is similar in nature to the
# Smarket data from this chapter's lab, except that it contains 1089
# weekly returns for 21 years, from the beginning of 1990 to the end of
# 2010.

require(ISLR)
library(magrittr)
library(dplyr)
library(class)

MeanError <- function(predicted,observed,threshold){
  
  glm.pred <- rep("Down" ,length(predicted))
  glm.pred[predicted > threshold]= "Up"
  confusion.table <- table(glm.pred,observed)
  meanError <- mean(glm.pred != observed) #Mean Value error
  return(meanError)
}

# (a). Produce some numerical and graphical summaries of the Weekly
# data. Do there appear to be any patterns?

summary(Weekly)
pairs(Weekly)

Up <- Weekly[Weekly$Direction == "Up",]
summary(Up)
boxplot(Up$Lag1,Up$Direction)
cor(Weekly[,-9])

#Non linear relation between Year and Volume
#Possible collinearity between lags
#No clear distinction between the direction and the variables
#There might be outliers in the data, Max value way above mean values
# Volume and year are colinear

# (b). Use the full data set to perform a logistic regression with
# Direction as the response and the five lag variables plus Volume
# as predictors. Use the summary function to print the results. Do
# any of the predictors appear to be statistically significant? If so,
# which ones?

glm.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data = Weekly,family = "binomial")
summary(glm.fit)

#Only Lag2 seems to be significant in the model

# (c) Compute the confusion matrix and overall fraction of correct
# predictions. Explain what the confusion matrix is telling you
# about the types of mistakes made by logistic regression.

glm.probs <- predict(glm.fit,type = "response")
glm.pred= rep("Down" ,length(glm.probs))
glm.pred[glm.probs >.5]= "Up"

confusion.table <- table(glm.pred,Weekly$Direction)

MeanErrorValue <- mean(glm.pred != Weekly$Direction) #Mean Value error

caret::varImp(glm.fit) #Importance of a Variable. USEFUL

# The most important variables are the first two lags
# Logistic method is slightly better than random classification

# (d) Now fit the logistic regression model using a training data period
# from 1990 to 2008, with Lag2 as the only predictor. Compute the
# confusion matrix and the overall fraction of correct predictions
# for the held out data (that is, the data from 2009 and 2010).

train <- (Weekly$Year < 2009)
test.set <- Weekly[!train,]
glm.fit2 <- glm(Direction ~ Lag2, data = Weekly,family = "binomial",
                subset = train)
summary(glm.fit2)

#Lag2 is a significant factor to explain the direction of the data

glm.probs2 <- predict(glm.fit2,newdata = test.set,type = "response")

Errors <- c(1)
sequence <- seq(0.05,0.95,0.001)
for (i in sequence){
  ME <- MeanError(glm.probs2,test.set$Direction,i)
  Errors <- c(ME,Errors)
}
Errors <- Errors[-c(length(Errors))]

plot(sequence,Errors)
threshold <- sequence[match(min(Errors),Errors)] #Best Threshold

glm.pred2 <- rep("Down" ,length(glm.probs2))
glm.pred2[glm.probs2 > threshold]= "Up"
logit.cm <- table(glm.pred2,test.set$Direction)

# It predicts reasonably well the weeks when the stock rises 
# but no when it decreases. False positives when increasing is a concern.


# (e) Repeat (d) using LDA.

library(MASS)
lda.model <- lda(Direction ~ Lag2,data = Weekly,subset = Year < 2009)
lda.model

lda.pred <- predict(lda.model,newdata = test.set) #Predict data - list
lda.cm <- table(lda.pred$class,test.set$Direction) #Confusion Matrix
mean(lda.pred$class != test.set$Direction)

# (f) Repeat (d) using QDA.
qda.model <- qda(Direction ~ Lag2,data = Weekly,subset = train)
qda.model

qda.pred <- predict(qda.model,newdata = test.set) #Predict data

#qda.pred.adj <- ifelse(qda.pred$posterior[, 2] > .70, "Up", "Down") 

qda.cm <- table(qda.pred$class,test.set$Direction) #Confusion Matrix
mean(qda.pred$class != test.set$Direction)

# (h) Repeat (d) using KNN with K = 1.

Direction <- Weekly$Direction[train] #Factor

TrainSet <- subset(Weekly,Year <= 2008, select = Lag2) #Matrix or Data Frame
TestSet <- subset(Weekly,Year > 2008, select = Lag2) #Matrix or Data Frame
Test.results <- Weekly$Direction[!train] #Factor

knn.pred <- knn(TrainSet,TestSet,Direction,k=1) 
knn.cm <- table(Test.results,knn.pred)
mean(knn.pred != test.set$Direction)


#Mutate function: adds new variables, preserves existing
# Summarise: Takes data frame and summarises information based on given criteria
# in this case the criteria is the mean value of the columns qda.pred different to direction

test.set %>% 
  mutate(qda.pred = (qda.pred$class),
         lda.pred = (lda.pred$class),
         logit.pred = (glm.pred2),
         knn.pred = (knn.pred)) %>%
  summarise(qda.Error = mean(qda.pred != Direction),
            lda.Error = mean(lda.pred != Direction),
            logit.Error = mean(logit.pred != Direction),
            knn.Error = mean(knn.pred != Direction))

list(QDA_model = qda.cm %>% prop.table() %>% round(3),
     LDA_model = lda.cm %>% prop.table() %>% round(3),
     KNN_model = knn.cm %>% prop.table() %>% round(3),
     LOGIT_model = logit.cm %>% prop.table() %>% round(3))

# Higher Error KNN > QDA > Logit > LDA
# LDA and Logit are better methods to analyze data
# KNN model better at predicting when the market falls than any other method

# (i) Experiment with different combinations of predictors, including
# possible transformations and interactions, for each of the
# methods. Report the variables, method, and associated confusion
# matrix that appears to provide the best results on the held
# out data. Note that you should also experiment with values for
# K in the KNN classifier.

Weekly_mod <- Weekly[-c(978,977,981,980,533,607,976,979),]
train_mod <- Weekly_mod$Year < 2009
test.set.mod <- Weekly[!train_mod,]
glm.fit3 <- glm(Direction ~ Lag1+Lag2,
               data = Weekly_mod,family = "binomial",subset = train_mod)
summary(glm.fit3)

glm.probs3 <- predict(glm.fit3,newdata = test.set.mod,type = "response")

Errors <- c(1)
sequence <- seq(0.05,0.95,0.001)
for (i in sequence){
  ME <- MeanError(glm.probs3,test.set.mod$Direction,i)
  Errors <- c(ME,Errors)
}
Errors <- Errors[-c(length(Errors))]

plot(sequence,Errors)
threshold <- sequence[match(min(Errors),Errors)] #Best Threshold

glm.pred3 <- rep("Down" ,length(glm.probs3))
glm.pred3[glm.probs3 > threshold]= "Up"
logit.cm <- table(glm.pred3,test.set$Direction)
mean(glm.pred3 != test.set.mod$Direction)


#Knn with 10 classes
knn10.pred <- knn(TrainSet,TestSet,Direction,k=10) 
knn10.cm <- table(Test.results,knn10.pred)
mean(knn10.pred != test.set$Direction)

#Best model is still LDA for just the Lag2 variable