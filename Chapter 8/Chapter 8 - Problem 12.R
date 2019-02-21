Kickstarter <- read.csv("ks-projects-201801.csv", 
                        header = TRUE)

Kickstarter$Days <- as.numeric(as.Date(Kickstarter$deadline) - as.Date(Kickstarter$launched)) #Timeframe of each project in days 

Year <- as.numeric(format(as.Date(Kickstarter$launched),"%Y"))

#Removing outliers whose launched date is 1970:01:01
Kickstarter.mod <- Kickstarter[!Year == 1970,]

#RichMF <- Kickstarter.mod[Kickstarter.mod$usd_goal_real > 1000000,]

rm(Kickstarter)
rm(Year)

# Simple Data Exploration

library(dplyr)

categories <- table(Kickstarter.mod$main_category)
categories <- categories[order(-categories)]
barplot(categories, xlab = "Main Categories", ylab = "Frequency")

Status <- table(Kickstarter.mod$state)
barplot(Status, xlab = "Status of the projects", ylab = "Frequency")

#Objective: Analyze failed vs successful projects in the Technology industry

Kickstarter.mod <- Kickstarter.mod %>% 
  filter(state %in% c("failed","successful")) 

Kickstarter.mod$state <- ifelse(Kickstarter.mod$state == "successful",1,0)
Kickstarter.mod$state <- as.factor(Kickstarter.mod$state)

Technology <- Kickstarter.mod[Kickstarter.mod$main_category == "Technology",]
cat.tech <- table(droplevels(Technology$category))
cat.tech <- cat.tech[order(-cat.tech)]
barplot(cat.tech, xlab = "Categories", ylab = "Frequency", main = "Technology Projects")

require(randomForest)
set.seed(1000)
Technology <- droplevels(Technology)
train <- sample(1:nrow(Technology),ceiling(nrow(Technology)*2/3))
Technology.train <- Technology[train,]
Technology.test <- Technology[-train,]

rm(Kickstarter.mod)

ntrees <- seq(from = 500,to = 3000, by = 500)
test.error.bagging <- rep(NA,length(ntrees))  

#Bagging

for (i in 1:length(ntrees)){
  Technology.bagging <- randomForest(state~category+country+Days+backers+usd_goal_real,
                                     data = Technology.train, ntree = ntrees[i],mtry = 5)
  Technology.pred.bagging <- predict(Technology.rf,Technology.test,type = "class")
  table.error.bagging <- table(Technology.pred.bagging, Technology.test$state)  
  test.error.bagging[i] <- table.error.bagging[2,1]/(table.error.bagging[2,1]+table.error.bagging[2,2])
}

importance(Technology.bagging)
rm(Technology.bagging)

#Random Forest

for (i in 1:length(ntrees)){
  Technology.rf <- randomForest(state~category+country+Days+backers+usd_goal_real,
                                data = Technology.train, ntree = ntrees[i],mtry = 2)
  Technology.pred.rf <- predict(Technology.rf,Technology.test,type = "class")
  table.error.rf <- table(Technology.pred.rf, Technology.test$state) 
  test.error.rf[i] <- table.error.rf[2,1]/(table.error.rf[2,1]+table.error.rf[2,2])
}

importance(Technology.rf)
rm(Technology.rf)

#Boosting
require(gbm)
for (i in 1:length(ntrees)){
  Technology.boost <- gbm(state~country+category+Days+backers+usd_goal_real,
                    data = Technology.train, distribution = "multinomial", n.trees = ntrees[i],
                    shrinkage = 0.01)
  
  Technology.prob.boost <- predict(Technology.boost,newdata = Technology.test,type = "response",n.trees = ntrees[i])
  Technology.pred.boost <- ifelse(Technology.prob.boost[,1,1] >= 0.5,0,1) #1 Column is fail status
  table.error.boost <- table(Technology.pred.boost, Technology.test$state) 
  test.error.boost[i] <- table.error.boost[2,1]/(table.error.boost[2,1]+table.error.boost[2,2])
}

rm(Technology.boost)


#Logistic Regression
glm.fit <- glm(state~country+category+Days+backers+usd_goal_real,
               data = Technology.train, family = "binomial") 

summary(glm.fit) 

glm.probs <- predict(glm.fit,Technology.test,type = "response")
glm.pred <- ifelse(glm.probs > 0.5,1,0)
table.error.log <-  table(glm.pred,Technology.test$state)
test.error.log <- table.error.log[2,1]/(table.error.log[2,1]+table.error.log[2,2])

test.error <- cbind(test.error.bagging,test.error.rf,test.error.boost)

matplot(ntrees,test.error, xlab = "Number of Trees", "ylab" = "Misclassification Error", 
     type = "b", pch = 20, lty = 2, col = c("red","black","blue"), ylim = c(0.09,0.16))

abline(h = test.error.log, col = "green")

par(mar=c(1,1,0,0))
legend("top",legend = c("Bagging","Random Forest","Boosting","Logistic Regression"),
       col =  c("red","black","blue","green"),horiz = TRUE,lty = 2,pch = 20, bty = "n")


#THe logistic regression is better and much easier to understand



