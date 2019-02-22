# Chapter 3 - Problem 13

# (a) Using the rnorm() function, create a vector, x, containing 100
# observations drawn from a N(0, 1) distribution. This represents
# a feature, X.
set.seed(1)
x <- rnorm(100)

# (b) Using the rnorm() function, create a vector, eps, containing 100
# observations drawn from a N(0, 0.25) distribution i.e. a normal
# distribution with mean zero and variance 0.25.

eps <- rnorm(100,sd = 0.25)

# (c) Using x and eps, generate a vector y according to the model
# What is the length of the vector y? What are the values of ??0
# and ??1 in this linear model?

y <- -1 +0.5*x+eps

n <- length(y) #size is 100

# B0 = -1 , B1 = 0.5

# (d) Create a scatterplot displaying the relationship between x and
# y. Comment on what you observe.

plot(x,y) # Clear linear dependance
          # It seems to have one outlier

# (e) Fit a least squares linear model to predict y using x. Comment
# on the model obtained. How do ^ ??0 and ^ ??1 compare to ??0 and ??1?

model <- lm(y ~ x)
summary(model)

# Both intercept and slope is very similar to the true intercept and slope

abline(model,col = "red")
abline(a = -1,b = 0.5,col = "blue")

legend(x = "topleft", legend=c("Linear model", "True model"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

# Now fit a polynomial regression model that predicts y using x
# and x2. Is there evidence that the quadratic term improves the
# model fit? Explain your answer.

model2 <- lm(y~x+I(x^2))
summary(model2)

# The model is not improved by adding the quadratic term.
# term is not significant to the model.

eps_Noisy <- rnorm(100,sd = 1.5)
eps_Less <- rnorm(100,sd = 0.15)

y_noisy <- -1+0.5*x+eps_Noisy
y_less <- -1+0.5*x+eps_Less

model_noisy <- lm(y_noisy ~ x)
model_less <- lm(y_less ~ x)

summary(model_noisy)
summary(model_less)

confint(model)
confint(model_less)
confint(model_noisy)
