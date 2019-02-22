# Chapter 2 - Exercises point 9

college = read.csv('College.csv')

rownames(college) = college[,1]

college = college[,-1]

# Show the variable as a dataframe
fix(college)

# Constructs a summary of important metrics of the features from the data.
summary(college)

PairData = college[,1:10] 

# Images of the pair data between the columns 
pairs(PairData,college)

college$Private = as.factor(college$Private)
plot(y = college$Outstate,x = college$Private,xlab = "OutState",ylab = "Private",col = "3",horizontal = T)

#Binning variables

Elite = rep("No",nrow(college)) #creates a vector of "No" with the same number of rows as college
Elite[college$Top10perc>50] = "Yes" #Yes is the number of students of top 10% from high school exceeds 50% 
Elite = as.factor(Elite) #categorical variable 
college = data.frame(college,Elite) #creates a new data frame where the variable Elite is added to the dataframe college

summary(Elite)

# number of Elite universites is 78

plot(y = college$Outstate,x = college$Elite,xlab = "Outstate", ylab = "Elite Universities", col = "Blue", horizontal = T)
par(mfrow=c(1,2)) #graphical parameter to break the main plot into subplots
hist(college$Apps,col = "red",breaks = 30,xlab = "Applications") 
hist(college$Accept,col = "red",breaks = 30,xlab = "Accepted") 

#Expends vs apps
AppsBelow = college$Apps
ExpendsBelow = college$Expend
AppsBelow[college$Apps > 5000] = 0

ZeroIndex = which(AppsBelow == 0) #identify indices of zero elements of AppsBelow
ExpendsBelow <- ExpendsBelow[-ZeroIndex]
AppsBelow <- AppsBelow[AppsBelow != 0]

plot(y = ExpendsBelow,x = AppsBelow)

plot(college$Outstate, college$Grad.Rate)
# High tuition correlates to high graduation rate.
plot(college$Accept / college$Apps, college$S.F.Ratio)
# Colleges with low acceptance rate tend to have low S:F ratio.
plot(college$Top10perc, college$Grad.Rate)
# Colleges with the most students from top 10% perc don't necessarily have
# the highest graduation rate. Also, rate > 100 is erroneous!

