# Chapter 3 - Training

library(MASS)
library("ISLR", lib.loc="J:/Program Files/R-3.5.1/library")
library (nortest)
library(car)

Housing <- Boston
names(Housing)
plot(medv ~ lstat, Housing)
fit <- lm(medv ~ lstat,Housing)
summary(fit)
abline(fit,col = "red") #Fit linear model in a plot
names(fit)

plot(hatvalues (fit )) #Plot Hatvalues of the regression

#Confidence intervals for coefficients - 95%
confint(fit)

#predict fit
predict(fit,newdata = data.frame(lstat = c(5,10,15)),interval = "confidence",
        level = 0.99)

#Linear model
res <- fit[["residuals"]] #Residuals
fitted <- fit[["fitted.values"]] #fitted.values

hist(res) #Histogram of Residuals
ad.test(res) #Anderson-Darling Test
plot(fitted,res) #

ncvTest(fit) #Non-Constant variance test

#Multiple Lienar Regression
fit2 <- lm(medv ~ lstat+age,Housing)
summary(fit2)

fit3 <- lm(medv ~ .,Boston) #All the other variables as predictors
summary(fit3)

par (mfrow=c(2,2))
plot(fit3) #Plots residuals

# Curvature in Residuals vs fitted values show Non-linear Behavior
fit4 <- update(fit3,~.-age-indus) 
# 1. nothing on the left - same response
# 2. dot notation - every variable
summary(fit4)

#Calculate Variance Inflation factor - Library car
vif(fit3)

#Nonlinear terms and interactions
fit5 <- lm(medv ~ lstat*age,Housing)
summary(fit5)

#Note that using the dot notation, both main effects and their interactions were calculated

#Quadratic terms
fit6 <- lm(medv ~ lstat+ I(lstat^2),Housing) #Identity function to protect quadratic terms
summary(fit6)
par (mfrow = c(1,1))

plot(Housing$medv ~ Housing$lstat)
points(Housing$lstat,fitted(fit6),col = "red",pch = 20)

#Anova test to quantify the best model
anova(fit,fit6) # Null hypothesis: Both models equally predict the data

#Fitting polynomials
fit7 <- lm(medv ~ poly(lstat,4),Housing) # 4 is the polynomial degree
points(Housing$lstat,fitted(fit7),col = "blue",pch = 20) #pch plotting characters

#polynomial fit is too wiggly - Overfitting problems

#Identifying plotting characters
plot(1:20,2:21,pch = 1:20,cex = 2)

#Qualitative factors
fix(Carseats)
names(Carseats) #Children car seats
summary(Carseats) #Notice that for Qualitative data, it produces counts

fit10 <- lm(Sales ~ .+Income:Advertising+Age:Price,Carseats)
summary(fit10)

contrasts(Carseats$ShelveLoc) #Coding for Qualitative data

regplot <- function(x,y,...){ #... argument = Construct, unnamed arguments can be passed and used
  fit =lm(y~x)
  plot(x,y,...)
  abline(fit,col = "red")
}

regplot(Carseats$Price,Carseats$Sales,xlab = "Price",ylab = "Sales",col = "blue")
