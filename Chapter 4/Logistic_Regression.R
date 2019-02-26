require(ISLR) #Similar to library
names(Smarket)
summary(Smarket)

#Predicting direction as binary response using logistic function
pairs(Smarket, col = Smarket$Direction) # Use color to show qualitative data

glm.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data = Smarket, family = "binomial") 

#Using family as binomial indicates logistic regression and logit functions

summary(glm.fit) 

# Stock Market: Not surprise that there is no significance, highly correlated variables
# Null deviance: Model wih only mean values
# residual deviance: adding errors to the model

glm.probs <- predict(glm.fit,type = "response")
glm.probs[1:5] # Close to 0.5 
glm.pred <- ifelse(glm.probs > 0.5,"Up","Down")

table(glm.pred,Smarket$Direction) # Diagonal = predicted values
mean(glm.pred == Smarket$Direction) # Possible overfit since no factor was significant

#Training and test set

TrainSet <- Smarket[Smarket$Year < 2005,]

glm.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data = TrainSet, family = "binomial") 

TestSet <- Smarket[Smarket$Year >= 2005,]
glm.probs <- predict(glm.fit,newdata = TestSet,type = "response")
glm.pred <- ifelse(glm.probs > 0.5,"Up","Down")

direction <- TestSet$Direction

table(glm.pred,direction) # Diagonal = predicted values
mean(glm.pred == direction) # Possible overfit since no factor was significant

#Fit a smaller model
glm.fit3 <- glm(Direction ~ Year+Lag1+Lag2,
               data = Smarket, family = "binomial") 
summary(glm.fit3) 
glm.probs3 <- predict(glm.fit3,type = "response")
glm.pred3 <- ifelse(glm.probs3 > 0.5,"Up","Down")
table(glm.pred3,Smarket$Direction) # Diagonal = predicted values
mean(glm.pred3 == Smarket$Direction) # Possible overfit since no factor was significant

