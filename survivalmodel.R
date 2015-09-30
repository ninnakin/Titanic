# Objective: Build a model that predicts whoch passengers survive and which don't
library(ggplot2)
library(dplyr)
library(kknn)
library(rpart)
library(rpart.plot)
# Load training data
train <- read.csv("train.csv")
train <- cleandata(train)
train$survived <- as.factor(train$survived)
levels(train$survived) <- c("Died", "Survived")

test  <- read.csv("test.csv")
test <- cleandata(test)

# add age groups 
train$agegroup <- cut(train$age, c(seq(0,80,10)), labels = c("0-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80"))
test$agegroup <- cut(test$age, c(seq(0,80,10)), labels = c("0-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80"))

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
summary(train)

##################################################################################
## Exploratory analysis 
##################################################################################

# Look at correlation between gender, age, pclass, and port with survival
# More females survive
summarise(group_by(train, sex), mean(survived))

qplot(sex, data=train, fill=survived)
# Age 
# More children survive, and more people in the 31-40 age group, less people in the 61-80 ages survive
summarise(group_by(train, agegroup), mean(survived))

hist(train$age[which(train$survived == "0")], main= "Passenger Age Histogram", xlab="Age", ylab="Count", col =4,
     breaks=seq(0,80,by=2))
hist(train$age[which(train$survived == "1")], col =2, add = T, breaks=seq(0,80,by=2))

qplot(age, data=train, fill=survived, colour=1, na.exclude=TRUE, binwidth=2)

mosaicplot(train$agegroup ~ train$survived, main="Passenger Survival by embarkment port",
           color=c("lightcoral", "cyan"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1)


# Age and gender in combination: no gender diff for children 
tmp1 <- summarise(group_by(train, agegroup, pclass, sex), survsum = sum(survived))
tmp2 <- summarise(group_by(train, agegroup, pclass, sex), survmean = mean(survived))
simplemod  <- merge(surv1, surv2,  by = intersect(names(surv1), names(surv2)))

## SVD
train$classnumeric<-as.numeric(gsub("([0-9]+).*$", "\\1", train$pclass))
# include all numeric variables 
svd1 = svd(scale(na.omit(train[, -c(1, 2, 3, 4, 9, 10)])))
par(mfrow = c(1, 2))
plot(svd1$u[, 1], col = train$survived, pch = 19)
plot(svd1$u[, 2], col = train$survived, pch = 19)
# doesn't give any more information

## k-means
kClust <- kmeans(na.omit(train[, -c(1, 2, 3, 4, 9, 10)]), centers = 10, nstart = 100)
table(kClust$cluster, na.omit(train)$survived)
# also does not work, does not distinguish betwee survivors and non-survivors 


## Very simple model: set survived = 1 for bins where mean(survived>0.5)
simplemod$survival=0;
simplemod$survival[simplemod$survmean>0.5]=1;
train$survivalmod=0;

for(i in 1:48){
  if(is.na(simplemod[i,]$agegroup)){
    train$survivalmod[train$pclass==simplemod[i,]$pclass & train$sex==simplemod[i,]$sex]=simplemod[i,]$survival}
  else{
    train$survivalmod[train$pclass==simplemod[i,]$pclass & train$sex==simplemod[i,]$sex & train$agegroup==simplemod[i,]$agegroup]=simplemod[i,]$survival}
} 

for(i in 1:48){
  if(is.na(simplemod[i,]$agegroup)){
    test$survivalmod[test$pclass==simplemod[i,]$pclass & test$sex==simplemod[i,]$sex]=simplemod[i,]$survival}
  else{
    test$survivalmod[test$pclass==simplemod[i,]$pclass & test$sex==simplemod[i,]$sex & test$agegroup==simplemod[i,]$agegroup]=simplemod[i,]$survival}
} 

# Export solution 
submissiondata <- cbind(test$passengerid, as.numeric(test$survivalmod))
colnames(submissiondata) <- c("PassengerId","Survived")
write.table(submissiondata, file="Solutions/decisiontree_anova.csv", col.names=TRUE, row.names=FALSE, sep=",")


# More passengers from 1st and 2nd class survive
summarise(group_by(train, pclass), mean(survived))
qplot(pclass, data=train, fill=survived, colour=1, na.exclude=TRUE, binwidth=1)
mosaicplot(train$pclass ~ train$survived, main="Passenger Survival by Class",
           color=c("lightcoral", "cyan"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)


# More passengers from Cherbourg survived
summarise(group_by(train, embarked), mean(survived))
qplot(embarked, data=train, fill=survived, colour=1, na.exclude=TRUE, binwidth=1)

mosaicplot(train$embarked ~ train$survived, main="Passenger Survival by embarkment port",
           color=c("lightcoral", "cyan"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)


##################################################################################
## Random forest classification
##################################################################################

# Random forest using the rpart package
fitclass <- rpart(survived ~ age + sex + pclass + embarked + fare + sibsp, data=train, method="class")
summary(fitclass)

# Plot decision trees
prp(fitclass, type = 3, extra = 3, fallen.leaves = TRUE)

# Use decision tree classification to predict survival for test group
test$survived <- predict(fitclass, test, type = "class")
levels(test$survived) <- c(0, 1)
test$survived <- as.character(test$survived)

# Export solution 
submissiondata <- cbind(test$passengerid, as.numeric(test$survived))
colnames(submissiondata) <- c("PassengerId","Survived")
write.table(submissiondata, file="Solutions/decisiontree_anova.csv", col.names=TRUE, row.names=FALSE, sep=",")

# Result: Not great
# Next step: ?








# some plots for the predicted survival rate
par(mfrow = c(1,2), xpd = NA)
mosaicplot(train$sex ~ train$survived, main="Passenger Survival by embarkment port",
           color=c("lightcoral", "cyan"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)
mosaicplot(test$sex ~ test$survived, main="Passenger Survival by embarkment port",
           color=c("lightcoral", "cyan"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)






# Ideas for algorithms: logistic regression, knn
titanic.kknn <- kknn(survived~sex+agegroup+pclass, train=train, test=test, distance = 1, kernel ="triangular")
str(titanic.kknn)
summary(titanic.kknn)
# returns fewer values than I have rows in test data. Why?



