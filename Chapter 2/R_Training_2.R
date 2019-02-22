#R training part 2 - graphics

x = rnorm(50,0,1)
y = rnorm(50,50,10)

#generic plot function - there are better functions in R - check ggplot library
#asp = y/x ratio for the data

pdf ("Test.pdf")
plot(x,y, xlab = "normal random data mean 0, sd = 1", ylab = "normal random data mean 50, sd = 10", main = "RAND", asp = 0.05)
dev.off()

#function dev.off is a command to identify when we are done creating a figure, especifically shut dows a graphic device
#function graphics.off shuts down all graphical devices

#sequences
Seq1 = 1:100

#function seq creates sequences of numbers
Seq2 = seq(1,100,2)

#contour - creates contour plot

ejeX = seq(-pi,pi,length = 100)
ejeY = ejeX

#outer - outer product of x and y 
# x,y normally vectors or arrays
# f = vectorized funciton to applly when z is a function of x and y

ejeZ = outer(ejeX, ejeY,function(x,y)cos(x)*sin(y)) 

#contour - graphs the contour map
contour(ejeX,ejeY,ejeZ,nlevels = 15)

#add to the contour graph
contour(ejeX,ejeY,ejeZ,nlevels = 15,add = T)
fa = ejeZ-t(ejeZ)/2
contour(ejeX,ejeY,fa,nlevels = 15)

#image() is a colored coded contour plot, contructed similarly as the contour map
image(ejeX,ejeY,ejeZ,col = heat.colors(1000))

#persp() 3d plot where theta and phi are the angles at which the plot is viewed

persp(ejeX,ejeY,ejeZ,theta = 100, phi = 50)

#par = command used to add more figures in one plot
par(mfrow = c(2,1))