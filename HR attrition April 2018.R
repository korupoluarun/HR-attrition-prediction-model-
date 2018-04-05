a <- install.packages (c("caret", "ggplot2", "lattice"))
library(caret)     
library(ggplot2)  # allows plotting colorful visualizations
library(lattice)
library(gmodels)  #allows cross tabulations
library(caTools) #allows splitting datasets
library(rpart.plot)#allows plotting decision trees


#set working directory and path
getwd()
setwd("/home/arun/Downloads/R")


#assign dataset to dataframe
df <- read.csv("hr.csv")
dim(df)
str(df)
names(df)

#summary statisitcs
summary(df)


#analyse attition in various categorical attributes using bar graphs
a <- ggplot(df, aes(Gender, fill=Attrition)) + geom_bar()    #Gender 
b <- ggplot(df, aes(YearsInCurrentRole, ..count..,fill = factor(Attrition))) +geom_bar() #Years in current role
c <- ggplot(df, aes(OverTime, fill= Attrition)) + geom_bar() #overtime
d <- ggplot(df, aes(MaritalStatus, fill=Attrition)) + geom_bar() #Marital status

#lets examine the monthly income

e <- boxplot(df$MonthlyIncome ~ df$Gender)
summary(df$MonthlyIncome)

#Employee profiles
boxplot(df$Age ~ df$Gender)
boxplot(df$Age ~ df$Attrition)


#two way cross tabulation gives proportion values

prop.table(table(df$Attrition))
prop.table(table(df$Gender))
prop.table(table(df$JobSatisfaction))


CrossTable(df$OverTime, df$Attrition)
CrossTable(df$OverTime, df$Gender)
CrossTable(df$JobSatisfaction, df$Attrition)
CrossTable(df$JobRole,df$Attrition)

#Excluding some variables
#Identifyig variables with less predictive power and eliminating them
nearZeroVar(df)
df <- df[, -c(9,22,27)]

#remove variable   EmployeeNumber
match("EmployeeNumber",names(df))
df <- df [,-9]
dim(df) # removed 4 different variables

#Identify categorical variables which are factors

which(is.factor(str(df)))

#Split data
set.seed(235)
split <- sample.split(df, SplitRatio = 0.80)
train <- subset(df, split == "TRUE")
test <- subset(df, split == "FALSE")

dim(train)
dim(test)


# Classification and regression tree analysis. you need the "rpart" package

library(rpart)
library(rpart.plot)

# train model

dtmodel <- rpart(Attrition ~ .,data=train, method = "class" )
prp(dtmodel)

# test model 

library(caret)
tmodel <- predict(dtmodel, newdata=test, type="class")
tmodel

result <- table(test$Attrition, tmodel)

result

dim(test)
accuracy <-  (301+21)/379
accuracy

#test the model on training dataset itself
tmodel2 <- predict(dtmodel, newdata=train, type="class")
result2 <- table(train$Attrition, tmodel2)
result2
dim(train)
accuracy2 <- (880+99)/1091
accuracy2














