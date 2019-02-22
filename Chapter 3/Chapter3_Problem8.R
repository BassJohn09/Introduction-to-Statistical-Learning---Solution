# Chapter 3
# Problem 8

library(ISLR)

model <- lm(mpg ~ horsepower,Auto)
summary(model)

# Is there a relationship between the predictor and the response?
# Yes, there is a relationship. P value < 0.05

# How strong is the relationship between the predictor and the response?
# Very Strong, Very low p-value

# Is the relationship between the predictor and the response positive or negative?
# Negative, B1 = -0.157845

# What is the predicted mpg associated with a horsepower of 98? 
# What are the associated 95% confidence and prediction intervals?

Prediction <- predict(model,newdata = data.frame(horsepower = c(98)),
                      interval = c("prediction"))
CI <- predict(model,newdata = data.frame(horsepower = c(98)),
              interval = c("confidence"))

print(Prediction)
print(CI)

# Plot the response and the predictor. Use the abline() function 
# to display the least squares regression line.

plot(Auto$horsepower,Auto$mpg,pch = 1,col = "red")
abline(model,col = "blue")

# Line does not capture the true behavior of the variable
# For range (50.75) and above 170 of Horsepower, overprediction
# For range (75,100) of Horsepower, underprediciton
# Nonlinearity behavior of the variable
# Non normality behavior of the response variable
# Signs of non-homocedasticity
# THere are high leverage points  
plot(model)
