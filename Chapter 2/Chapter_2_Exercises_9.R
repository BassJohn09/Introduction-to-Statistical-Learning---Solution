
#Chapter 2 - Exercises part 2
dataset = read.table("Auto.Data",header = TRUE, na.strings = "?")

#in dataset with missing values, there are ways to deal with these kind of error, one is simply omit them
dataset = na.omit(dataset)

# three qualitative data, 5 quantitative data

sapply(dataset[,-c(2,7,8,9)],range) #sapply applies function over a list or vector
sapply(dataset[,-c(2,7,8,9)],mean) 
sapply(dataset[,-c(2,7,8,9)],sd) 

NewData = dataset[-c(10:85),]

sapply(NewData[,-c(2,7,8,9)],range) 
sapply(NewData[,-c(2,7,8,9)],mean) 
sapply(NewData[,-c(2,7,8,9)],sd) 

pairs(~ mpg + cylinders+displacement+horsepower+acceleration,dataset)

dataset$cylinders = as.factor(dataset$cylinders)
dataset$year = as.factor(dataset$year)
dataset$origin = as.factor(dataset$origin)

plot(y = dataset$mpg,x = dataset$cylinders)

pairs(~ mpg + weight+year+origin,dataset)

plot(y = dataset$mpg,x = dataset$origin)
