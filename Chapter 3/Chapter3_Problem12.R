# Chapter 3 - Problem 12

# (b) Generate an example in R with n = 100 observations in which
# the coefficient estimate for the regression of X onto Y is different
# from the coefficient estimate for the regression of Y onto X.

set.seed(1)
x <- rnorm(100)
y <- rnorm(100)

model <- lm(y ~ x+0)
summary(model)

model2 <- lm(x ~ y+0)
summary(model2)

# (c) Generate an example in R with n = 100 observations in which
# the coefficient estimate for the regression of X onto Y is the
# same as the coefficient estimate for the regression of Y onto X.


