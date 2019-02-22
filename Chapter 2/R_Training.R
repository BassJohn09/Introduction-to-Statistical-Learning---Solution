
#Concatenate, vector
x = c(1,3,2,5)
y = c(4,5,8,10)

#length - size of a vector/matrix
length(x)

#Sum of vectors
z = x+y
z = x^2 #element-wise exponentiation
w = x/y #element-wise division

#ls function = list of objects saved so far
#rm funciton = delete objects/variables

rm(z)
ls()

#to remove all objects at once - if necessary 
# rm(list = ls())

# for help on a specific function, add ? to the beginning of the functin
# ?seq

#There are no scalars in R, only vectors

#Matrix creates matrices with a given set of values
#as.matrix = attemps to transform the data into a matrix
#is.matrix = checks whether an object is or is not a matrix
# Numbers are taken in column order - IMPORTANT

M <- matrix(data = c(1, 2, 3, 6, 7, 8), nrow = 2, ncol =3)

#Marix operation => sqrt for roots and ^ to power 

S = sqrt(M)
P = M^2
subset = M[2, 3]
complete_subset = M[-4] #Eliminates index and return the subset as vector

#If you do not want to lose the Matrix
Matrix.subset <- M[,1,drop=FALSE]

#Dimensions of a matrix
dim(M)

#rnorm = normal distribution random generator

M_alea = matrix(rnorm(6,0,1),nrow = 3,ncol = 2)


#cor = get correlation for two variables

cor(x,y)

#to get same random generated variables, we will define the seed of the pseudo random generator
set.seed(5000)
Alea2 = rnorm(50)

#statistical function
#mean calculates the average value of the set
#var calculates the variance of the set
#sd calculates the standard deviation of the set, also obtained as sqrt(var(X))

Media = mean(Alea2)
Varianza = var(Alea2)
Desviacion = sd(Alea2)



