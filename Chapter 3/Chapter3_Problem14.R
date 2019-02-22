# Chapter 3 - Problem 14

# (a) The last line corresponds to creating a linear model in which y is
# a function of x1 and x2. Write out the form of the linear model.
# What are the regression coefficients?

set.seed (1)
x1 <- runif(100)
x2 <- 0.5* x1+rnorm(100) /10
y <- 2+2*x1 +0.3*x2+rnorm(100)

# y = 2 + 2*x1+0.3*x3+eps

# (b) What is the correlation between x1 and x2? Create a scatterplot
# displaying the relationship between the variables.

cor(x1,x2) # High correlation between x1 and x2
plot(x1,x2) # Linear dependence between x1 and x2

# (c) Using this data, fit a least squares regression to predict y using
# x1 and x2. Describe the results obtained. What are ??0, ??1, and
# ??2? How do these relate to the true ??0, ??1, and ??2? Can you
# reject the null hypothesis H0 : ??1 = 0? How about the null
# hypothesis H0 : ??2 = 0?

model <- lm(y ~ x1+x2)
summary(model)

# B0 is close to the true value, B1 and B2 were wrongly calculated
# B2 = 0 hypothesis cannot be rejected.

# (d) Now fit a least squares regression to predict y using only x1.
# Comment on your results. Can you reject the null hypothesis
# H0 : ??1 = 0?

model2 <- lm(y~x1)
summary(model2)

# B1 = 0 hypothesis is rejected.

# (e) Now fit a least squares regression to predict y using only x2.
# Comment on your results. Can you reject the null hypothesis
# H0 : ??2 = 0?

model3 <- lm(y~x2)
summary(model3)

# B2 = 0 hypothesis now is rejected.

# (f) Do the results obtained in (c)-(e) contradict each other? Explain
# your answer.

# When analyzing by itself, the factor x2 was significant but when 
# analyzing the overall model is was no longer significant. collinearity problem
# between x1 and x2.

# (g) Now suppose we obtain one additional observation, which was
# unfortunately mismeasured.

x1 <- c(x1 , 0.1)
x2 <- c(x2 , 0.8)
y <- c(y,6)

# Re-fit the linear models from (c) to (e) using this new data. What
# effect does this new observation have on the each of the models?
# In each model, is this observation an outlier? A high-leverage
# point? Both? Explain your answers.

model_a <- lm(y ~ x1+x2)
summary(model_a)
model_b <- lm(y ~ x1)
summary(model_b)
model_c <- lm(y ~ x2)
summary(model_c)

# on the complete model, the x1 factor is not longer significant, 
# x2 became significant due to this new datapoint.
par(mfrow = c(2,2))
plot(model_a)

# According to the Residuals vs Leverage graph, new data is an high leverage point

library(car)
library(ggplot2)
outlierTest(model_b) 

# These point is also an outlier, there is another outlier on point 82
# according to the Bonferroni test


