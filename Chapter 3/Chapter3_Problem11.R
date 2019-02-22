# Chapter 3 - Problem 11
# (a) Perform a simple linear regression of y onto x, without an intercept.
# Report the coefficient estimate ^??, the standard error of
# this coefficient estimate, and the t-statistic and p-value associated
# with the null hypothesis H0 : ?? = 0. Comment on these
# results.

set.seed (1)
x <- rnorm (100)
y <- 2*x+rnorm (100)

model <- lm(y~x+0)
ValuesXY <- summary(model)$coefficient
print(ValuesXY)

# (b) Now perform a simple linear regression of x onto y without an
# intercept, and report the coefficient estimate, its standard error,
# and the corresponding t-statistic and p-values associated with
# the null hypothesis H0 : ?? = 0. Comment on these results.

model2 <- lm(x~y+0)
ValuesYX <- summary(model2)$coefficient
print(ValuesYX)

# (c) What is the relationship between the results obtained in (a) and
# (b)?

#Same t-value, same P-value, different coefficient and estimated error
# (d) Show algebraically, and confirm numerically
# in R, that the t-statistic can be written as

n <- length(x)
tstat <- sum(x*y)*sqrt(n-1)/sqrt(sum(x^2)*sum(y^2)-sum(x*y)^2)

# (e) Using the results from (d), argue that the t-statistic for the regression
# of y onto x is the same as the t-statistic for the regression of x onto y.

# Same statistics for x onto y that y onto x

# (f) In R, show that when regression is performed with an intercept,
# the t-statistic for H0 : ??1 = 0 is the same for the regression of y
# onto x as it is for the regression of x onto y.

model3 <- lm(y ~ x)
summary(model3)

model4 <- lm(x ~ y)
summary(model4)

# Same t-statistic for x-y and y-x = 18.56
