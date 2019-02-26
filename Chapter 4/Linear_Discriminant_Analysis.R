require(ISLR)
require(MASS)

#Linear Discriminant Analysis
lda.fit <- lda(Direction ~ Lag1+Lag2,data = Smarket,subset = Year < 2005)
lda.fit

#Prior Probabilities = Probabilities on the dataset
#Groups means: Means for each class
#Coefficients of linear discriminants - LDA. Linear function

plot(lda.fit) # No difference between both histograms

#Subset
Smarket.2005 <- subset(Smarket,Year == 2005) #test data
lda.pred <- predict(lda.fit,newdata = Smarket.2005) #Predict data - list
data.frame(lda.pred)[1:5,]

table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class == Smarket.2005$Direction)

