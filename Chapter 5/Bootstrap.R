# Bootstrap method in R
require(boot)
# Easy statistics for complicated situations

alpha <- function(x,y){
 var.x <- var(x)
 var.y <- var(y)
 covariance <- cor(x,y)
 est.alpha <- (var.y-covariance)/(var.x+var.y-2*covariance)
 return(est.alpha)
}

est.alpha <- alpha(Portfolio$X,Portfolio$Y)

#What is the standard error for alpha
# Use a wrapper to calculate

alpha.fm <- function(data,index){
  with(data[index,],alpha(X,Y))
}

# WITH function allows the use of the variable within the dataframe

alpha.fm(Portfolio,1:3)

set.seed(1)
alpha.fm(Portfolio,sample(1:100,100, replace=TRUE))

#Bootstrap function

boot.out <- boot(Portfolio,alpha.fm,R = 1000)
boot.out # statistic for the bootstrap method. statistic, bias and standard error
plot(boot.out) # Histogram + QQplot

