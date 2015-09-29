# Objective: Build a model that predicts whoch passengers survive and which don't
library(ggplot2)
library(dplyr)
library(kknn)
# Load training data
train <- read.csv("train.csv")
train$survived[train$survived==0]="died"
train$survived[train$survived==1]="survived"
train$survived <- as.factor(train$survived)

test  <- read.csv("test.csv")
names(test)  <- tolower(names(test))
names(train) <- tolower(names(train))
# add age groups 
train$agegroup <- cut(train$age, c(seq(0,100,10)), labels = c("0-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100"))
test$agegroup <- cut(test$age, c(seq(0,100,10)), labels = c("0-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100"))

# Data info
# VARIABLE DESCRIPTIONS:
#   survival        Survival
# (0 = No; 1 = Yes)
# pclass          Passenger Class
# (1 = 1st; 2 = 2nd; 3 = 3rd)
# name            Name
# sex             Sex
# age             Age
# sibsp           Number of Siblings/Spouses Aboard
# parch           Number of Parents/Children Aboard
# ticket          Ticket Number
# fare            Passenger Fare
# cabin           Cabin
# embarked        Port of Embarkation
# (C = Cherbourg; Q = Queenstown; S = Southampton)

# Explore data 
names(train)
str(train)



# Look at correlation between gender, age, pclass, and port with survival
# More females survive
summarise(group_by(train, sex), mean(survived))
# More children survive, and more people in the 31-40 age group, less people in the 61-80 ages survive
summarise(group_by(train, agegroup), mean(survived))
# Age and gender in combination: no gender diff for children 
summarise(group_by(train, agegroup, sex), mean(survived))

# More passengers from 1st and 2nd class survive
summarise(group_by(train, pclass), mean(survived))
# More passengers from Cherbourg survived
summarise(group_by(train, embarked), mean(survived))


# Ideas for algorithms: logistic regression, knn
titanic.kknn <- kknn(survived~sex+agegroup+pclass, train=train, test=test, distance = 1, kernel ="triangular")
str(titanic.kknn)
summary(titanic.kknn)
# returns fewer values than I have rows in test data. Why?



# iris example
data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, prob = rep(1/m, m)) 
iris.learn <- iris[-val,]   # train
iris.valid <- iris[val,1:4]    # test
iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1, kernel ="triangular")


