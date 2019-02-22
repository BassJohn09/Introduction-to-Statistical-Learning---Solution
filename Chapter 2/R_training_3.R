#Indexing

Ma = matrix(1:20,5,4)

X = Ma[2,3]

#select multiple row or columns

Y = Ma[c(1,3),c(2,4)]
Z = Ma[1:3,2:4]
W = Ma[1:2,]

#empty indexes are viewed as the complete range
#The use of minus signs implies taking all indexes except the ones mentioned

Z2 = Ma[-4,-1] #check that Z and Z2 are equal but written differently

#dim = get dimensions of a matrix

dim(Y)

#_______________________________________________________________________________#
# Importing Data
#read.table() generic function to read data as data frame object
#write.table() generic function to export data

dataset = read.table("Auto.Data",header = TRUE, na.strings = "?")
fix(dataset) #fixes the data os it can be seen (& edited) as a spreadsheet

#in dataset with missing values, there are ways to deal with these kind of error, one is simply omit them
dataset = na.omit(dataset)
dim(dataset)

# names - used to get the names of all the variables

Nombres = names(dataset)

#make the variables of the dataframe directly available
attach(dataset)

#transform data into categorical values
cylinders = as.factor(cylinders)
plot(x = cylinders, y = mpg, col = "red", varwidth = T, horizontal = F)

# #histogram
# hist(mpg,col = "green",breaks = 20)
# 
# pairs(dataset)
# pairs(~ mpg + displacement+horsepower+acceleration,dataset)
# 
# #Identify : get value for particualr points in the plot
# plot(horsepower,acceleration)
# puntos = identify(horsepower,acceleration,name)
# 
# #Summary : get statistical summary of a dataframe
# summary(dataset)
# 
# #Search(), checks the list of workspaces available
