# Chapter 3 - Problem 15
library(MASS)

# (a) For each predictor, fit a simple linear regression model to predict
# the response. Describe your results. In which of the models is
# there a statistically significant association between the predictor
# and the response? Create some plots to back up your assertions.

Boston <- Boston
Val <- ncol(Boston)-1
M_Coef <- matrix(nrow = Val,ncol = 3,
          dimnames = list(NULL, c("Factor","Coefficient","PValue")))

for (i in 1:Val){
  crim <- Boston[,1]
  value <- Boston[,i+1]
  model <- lm(crim ~ value)
  PValue <- summary(model)$coeff[2,4]
  Name <- names(Boston[i+1])
  M_Coef[i,1] <- Name
  M_Coef[i,2] <- model$coefficient[2]
  M_Coef[i,3] <- PValue
}

#Significant values
M_Coef <- as.data.frame(M_Coef)
M_Coef[,2] <- as.numeric(as.character(M_Coef[,2]))
M_Coef[,3] <- as.numeric(as.character(M_Coef[,3]))
Sign_facts <- M_Coef[M_Coef$PValue <= 0.05,]
names_facts <- as.character(Sign_facts$Factor)
names_facts[13] <- "crim"
Significant <- subset(Boston,select = names_facts)

pairs(crim ~ nox+rm+dis+black,data = Significant)
pairs(crim ~ age+zn+indus+lstat,data = Significant)
pairs(crim ~ rad+tax+ptratio+medv,data = Significant)

# (b) Fit a multiple regression model to predict the response using
# all of the predictors. Describe your results. For which predictors
# can we reject the null hypothesis H0 : ??j = 0?

multi <- lm(crim ~ .,Boston)
summary(multi)
Sign_multi <- summary(multi)$coeff[,4] < 0.05
SigVariables <- Boston[Sign_multi == TRUE]
CoefMultivariable <- multi$coefficients[2:14]

plot(M_Coef$Coefficient,CoefMultivariable) # Significant difference between the coefficients

par(mfrow = c(2,2))
plot(multi)

M_Coef_poly <- matrix(nrow = Val,ncol = 5,
                 dimnames = list(NULL, c("Factor","B1","B2","PValue_B1","PValue_B2")))

for (i in 1:2){
  crim <- Boston[,1]
  value <- Boston[,i+1]
  model <- lm(crim ~ poly(value,3))
  PValue_B1 <- summary(model)$coeff[2,4]
  PValue_B2 <- summary(model)$coeff[3,4]
  Name <- names(Boston[i+1])
  M_Coef_poly[i,1] <- Name
  M_Coef_poly[i,2] <- model$coefficient[2]
  M_Coef_poly[i,3] <- model$coefficient[3]
  M_Coef_poly[i,4] <- PValue_B1
  M_Coef_poly[i,5] <- PValue_B2
}
