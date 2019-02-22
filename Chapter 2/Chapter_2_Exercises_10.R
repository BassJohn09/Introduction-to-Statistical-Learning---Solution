# Chapter 2 - Exercises part 3

library(MASS)

dataset = Boston

dataset$chas = as.factor(dataset$chas)

plot(y= dataset$crim,x = dataset$indus)
identify(dataset$crim,dataset$indus,dataset$indus)

pairs(~crim+age+dis,Boston)
plot(y= dataset$crim,x = dataset$dis)

pairs(~crim+rad+tax,Boston)
plot(y= dataset$zn,x = dataset$tax)

pairs(~crim+ptratio+black,Boston)
plot(y= dataset$crim,x = dataset$black)

pairs(~crim+medv+lstat,Boston)
plot(y= dataset$crim,x = dataset$lstat)

CharlesRiver = subset(Boston, chas == 1)
median(dataset$ptratio)
median(CharlesRiver$ptratio)

hist(Boston$crim[Boston$crim < 80 & Boston$crim > 40], breaks = 15)


#______________________________________

MinMedV=subset(dataset,dataset$medv == min(dataset$medv))
summary(MinMedV)

MaxRooms=subset(dataset,dataset$rm >= 8)
summary(MaxRooms)

range(dataset$crim)
