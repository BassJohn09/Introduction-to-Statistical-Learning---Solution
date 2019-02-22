
library(ISLR)
library(car)
# Chapter 3 - Problem 10
# (a) Fit a multiple regression model to predict Sales using Price, Urban, and US. 

model1 <- lm(Sales ~ Price+Urban+US,Carseats)
summary(model1)

#(b) Provide an interpretation of each coefficient in the model. Be careful
#    - some of the variables in the model are qualitative!

# Sales go up as price goes down
# Urban location has no significant effect on sales
# If the Store is in the US, Sales significantly increases

# (c) Write out the model in equation form, being careful to handle
#    the qualitative variables properly.

# Sales = 13.04 - 0.054*Price - 0.02*UrbanYes + 1.2*USYes

# (d) For which of the predictors can you reject the null hypothesis
# H0 : ??j = 0?

model2 <- lm(Sales ~.,Carseats)
summary(model2)

# Significant Predictors: CompPrice, Income, Advertising, Price, ShelveLocGood, ShelveLocMedium, Age

# (e) On the basis of your response to the previous question, fit a
#     smaller model that only uses the predictors for which there is
#     evidence of association with the outcome.

model3 <- lm(Sales ~ CompPrice+Income+Advertising+Price+ShelveLoc+Age,Carseats)
summary(model3)

# (f) How well do the models in (a) and (e) fit the data?

# Last model fits better the data according to the R^2 (0.8697 vs 0.2335)

# (g) Using the model from (e), obtain 95% confidence intervals for
#     the coefficient(s).

confint(model3)

# (h) Is there evidence of outliers or high leverage observations in the
#     model from (e)?

par(mfrow = c(2,2))
plot(model3)
outlierTest(model3)

# 358 is a potential outlier
# 298 is a potential outlier