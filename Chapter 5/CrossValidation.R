# Cross Validation method in R

require(ISLR)
require(boot)

#CV.GLM: Cross validation for generalized linear models

plot(mpg~horsepower,data = Auto)

#Leave-one out Cross Validation (LOOCV)
model <- glm(mpg~horsepower,data = Auto)
CVmodel <- cv.glm(data = Auto, glmfit = model) 
CVerror <- CVmodel$delta

# Predictive error does not use standard formula

loocv <- function(fit){
  h <- influence(fit)$h
  residuals <- residuals(fit)
  error <- mean((residuals/(1-h))^2) # element-wise operation
  return(error)
}

loocv(model)

cv.error <- rep(0,5)
degree <- 1:5

for (d in degree){
  polyfit <- glm(mpg ~ poly(horsepower,d),data = Auto)
  cv.error[d] <- loocv(polyfit)
}

plot(degree,cv.error,type = 'b')

# 10 fold cross validation

CV.error.10fold <- rep(0,5)

for (d in degree){
  model <- glm(mpg~poly(horsepower,d),data = Auto)
  CV.error.10fold[d] <- cv.glm(data = Auto, glmfit = model, K = 10)$delta[1]   
}

lines(degree,CV.error.10fold,type = 'b', col = "red")
