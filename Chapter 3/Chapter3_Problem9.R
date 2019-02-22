library(ISLR)
library(car)
library(nortest)
library(MASS)

# Chapter 3 - Problem 9
# a. Produce a scatterplot matrix which includes all of the variables in the data set.
pairs(Auto)

# b. Compute the matrix of correlations between the variables using 
#    the function cor(). You will need to exclude the name variable,
#    cor() which is qualitative.
QuanAuto <- Auto
QuanAuto$name <- NULL
Correlation <- cor(QuanAuto)

#c. Use the lm() function to perform a multiple linear regression
#   with mpg as the response and all other variables except name as
#   the predictors. Use the summary() function to print the results.
#   Comment on the output. For instance:

# i. Is there a relationship between the predictors and the response?
model1 <- lm(mpg ~ .,QuanAuto)
summary(model1)

# There is a relationship between the mpg and the following predictors:
# (displacement,weight,year,origin). 

# The other predictors does not seem to have a significant effect on the mpg

# ii. Which predictors appear to have a statistically significant
#     relationship to the response?

Significant <- summary(model1)$coeff[,4] < 0.05
SigVariables <- names(QuanAuto)[Significant == TRUE] 

# iii. What does the coefficient for the year variable suggest?
Year <- summary(model1)$coeff[7,1] #Year Coefficient

# More recent cars are correlated to better mpg in cars, 
# which can be explained by technological improvements. 

# d. Use the plot() function to produce diagnostic plots of the linear
# regression fit. Comment on any problems you see with the fit.
# Do the residual plots suggest any unusually large outliers? Does
# the leverage plot identify any observations with unusually high
# leverage?

par(mfrow = c(2,2))
plot(model1)
outlierTest(model1)

# Nonlinearity observed
# Nonnormality observed
# High leverage point (14)
# There is evidence of outliers

# e. Use the * and : symbols to fit linear regression models with
# interaction effects. Do any interactions appear to be statistically significant?

model2 <- lm(mpg ~ .+weight:horsepower +
                   acceleration:horsepower + displacement:year,QuanAuto)
summary(model2)

# f. Try a few different transformations of the variables, such as log(X),
# ???X, X2. Comment on your findings.

QuanAuto$mpg <- log(QuanAuto$mpg)
pairs(QuanAuto)

model3 <- lm(mpg ~ .+weight:horsepower+acceleration:horsepower 
                    +displacement:year,QuanAuto)
summary(model3)
