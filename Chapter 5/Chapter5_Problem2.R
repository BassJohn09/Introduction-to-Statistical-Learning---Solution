# Chapter 5, Problem 2

#probability of sampling with replacement
prob.sampling <- function(n){ prob <- 1-(1-1/n)^n}

par(mfrow = c(1,2))
n <- 1:100000
plot(n,prob.sampling(n),xlab = "Integer",ylab = "Probability",)
n2 <- 1:50
plot(n2,prob.sampling(n2),xlab = "Integer",ylab = "Probability")
title("Probability that jth observation is in the bootstrap sample", outer = TRUE, cex.main = 1.5)


store=rep (NA , 10000)
for (i in 1:10000) {
  store[i]=sum(sample (1:100 , rep =TRUE)==4) >0
}
mean(store)