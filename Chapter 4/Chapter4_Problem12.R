# Chapter 4 - Problem 12
# (a) Write a function, Power(), that prints out the result of raising 2
# to the 3rd power. In other words, your function should compute
# 23 and print out the results.

Power <- function(){
  x = 2^3
  print(x)
}
Power()

# (b) Create a new function, Power2(), that allows you to pass any
# two numbers, x and a, and prints out the value of x^a. You can
# do this by beginning your function with the line

Power2 <- function(x,y){
  print(x^y)
}

Power2(3,8)

# (c) Using the Power2() function that you just wrote, compute 10^3,
# 8^17, and 131^3.

Power2(10,3)
Power2(8,17)
Power2(131,3)

# (d) Now create a new function, Power3(), that actually returns the
# result x^a as an R object, rather than simply printing it to the
# screen. That is, if you store the value x^a in an object called
# result within your function, then you can simply return() this
# return(result) 

Power3 <- function(x,y){
  return(x^y)
}

# (e) Now using the Power3() function, create a plot of f(x) = x2.
# The x-axis should display a range of integers from 1 to 10, and
# the y-axis should display x2. Label the axes appropriately, and
# use an appropriate title for the figure. Consider displaying either
# the x-axis, the y-axis, or both on the log-scale. You can do this
# by using log=''x'', log=''y'', or log=''xy'' as arguments to
# the plot() function.

x <- c(1:10)
y <- Power3(x,2)

plot(x,y,log="y")

# Create a function, PlotPower(), that allows you to create a plot
# of x against x^a for a fixed a and for a range of values of x. For
# instance, if you call > PlotPower (1:10 ,3)
# then a plot should be created with an x-axis taking on values
# 1, 2, . . . , 10, and a y-axis taking on values 13, 23, . . . , 103.

PlotPower <- function(x,a){
  y <- x^a
  plot(x,y)
}

PlotPower(x,3)
