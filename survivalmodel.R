# Objective: Build a model that predicts whoch passengers survive and which don't
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

# Libraries 
library(ggplot2)
library(dplyr)
library(kknn)
library(rpart)
library(rpart.plot)
library(randomForest)
library(party)

# Load data
train <- read.csv("train.csv", na.strings = "")
test  <- read.csv("test.csv", na.strings = "")

# Process data 

# Look at passenger name 
# Check what titles there are int the data (both train and test) 
train <- cleandata(train)
test <- cleandata(test)
test$survived <- NA
combi <- rbind(train, test)

# Extract titles
# title seem to be in the second word
n  <- sapply(strsplit(combi$name,","), function(x){x[2]})
combi$title <- sapply(strsplit(n," "), function(x){x[2]})

# one title is just "the" what does it stand for? 
# change title to "Countess."
combi$title[combi$title=="the"] <- "Countess."
# some other titles are rare, lets bin them together
combi$title[combi$title=="Mme."]<- "Mlle."
combi$title[combi$title=="Ms."]<- "Miss."
combi$title[combi$title %in% c("Don.", "Major.", "Col.", "Capt.","Jonkheer.")] <- "Sir."
combi$title[combi$title %in% c("Dona.", "Countess.")] <- "Lady."
combi$title <- factor(combi$title)

# Combine parch and sibsp to one variable called family size
combi$familysize <- combi$sibsp + combi$parch + 1
combi$familysize[combi$familysize<=2] <- "small"

# Identify families through surname and family size 
combi$surname <- sapply(strsplit(combi$name,","), function(x){x[1]})
combi$familyid <- paste0(combi$familysize,combi$surname)
combi$familysize <- combi$sibsp + combi$parch + 1
combi$familysize <- as.numeric(combi$familysize)

# some familys are categorized as not small even though there is only one person in them 
# look at these
famIDs <- data.frame(table(combi$familyid))
famIDs <- famIDs[famIDs$Freq<3, ]
combi$familyid [combi$familyid  %in% famIDs$Var1] <- 'Small'
combi$familyid  <- factor(combi$familyid )

rm(famIDs)
rm(n)

# Embarked: set missing values to most common port
combi$embarked[which(is.na(combi$embarked))]="S"
combi$fare[which(is.na(combi$fare))]<-median(combi$fare, na.rm=TRUE)

# Reduce factors for familyid for use in random forest
combi$familyid2 <- as.character(combi$familyid)
combi$familyid2[combi$familysize <= 3] <- 'Small'
combi$familyid2 <- factor(combi$familyid2)

# Estimate age using decision tree
# Idea: Construct a decision tree using the passengers with known age
# Use this to estimate the age of the remaining passengers
fitage <- rpart(age ~ sex + pclass + embarked + fare + title + sibsp + parch + familysize, data=combi[!is.na(combi$age),], method="anova")
combi$age[is.na(combi$age)] <- round(predict(fitage, combi[is.na(combi$age),]),0)

# get cabin section and number
# Some passengers have several cabins. In this case just choose the first one (I assume they are located close to each other)
combi$cabin <- as.character(combi$cabin)
singlecabin <- sapply(strsplit(combi$cabin, " "), function(x){x[1]})
combi$cabinnumber  <- as.numeric(sub("[^0-9]", "", singlecabin))
combi$cabinsection <- gsub("[^A-Z]", "", singlecabin)
combi$cabinsection <- as.factor(combi$cabinsection)
  
# Estimate msising cabin sections using random forest
#fitcabin <- rpart(cabinsection ~ age + sex + pclass + embarked + fare + title + sibsp + parch + familysize, data=combi[!is.na(combi$cabinsection),], method="class")
#combi$cabinsection[is.na(combi$cabinsection)] <- predict(fitcabin, combi[is.na(combi$cabinsection),], type="class")

# Cabin is mostly specified for passengers in first class
# because of this I will only estimate for 1st class passengers

unknown <- which(is.na(combi$cabinsection) & combi$pclass=="1st Class")
fitcabin <- rpart(cabinsection ~ age + sex + pclass + embarked + fare + title + sibsp + parch + familysize, data=combi[which(!is.na(combi$cabinsection) & combi$pclass=="1st Class"),], method="class")
combi$cabinsection[unknown] <- predict(fitcabin, combi[unknown,], type="class")
levels(combi$cabinsection)<-c(levels(combi$cabinsection), c("2","3"))
combi$cabinsection[which(is.na(combi$cabinsection) & combi$pclass=="2nd Class")] <- "2"
combi$cabinsection[which(is.na(combi$cabinsection) & combi$pclass=="3rd Class")] <- "3"


# Split back into train and test data
train <- combi[1:891,]
test <- combi[892:1309,]


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
submitdata(test$passengerid, test$survivalmod, "manualbins.csv")

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
fitclass0 <- rpart(survived ~ age + sex + pclass + embarked + fare + sibsp, parch, data=train, method="class")
summary(fitclass)

# Plot decision trees
prp(fitclass0, type = 3, extra = 3, fallen.leaves = TRUE)

# Use decision tree classification to predict survival for test group
test$survived <- predict(fitclass, test, type = "class")
levels(test$survived) <- c(0, 1)

# Export solution 
submitdata(test$passengerid, as.character(test$survived), "decisiontree_class.csv")

# Result: Not great
# Next step: ?
# Check out Titanic tutorial by Trevor Stephens

# make a new tree, bit change parameters of method to 
# allow splitting of smaller buckets and higher complexity
morecomplex <- rpart.control(minsplit = 2, cp = 0)

fitclass2 <- rpart(survived ~ age + sex + pclass + embarked + fare + sibsp + parch, data=train, method="class", control = morecomplex)
summary(fitclass2)

prp(fitclass2, type = 3, extra = 3, fallen.leaves = TRUE)

# Submit this new model 
test$survived <- predict(fitclass, test, type = "class")
levels(test$survived) <- c(0, 1)
submitdata(test$passengerid, as.character(test$survived), "decisiontree_complex.csv")

# To be able to test performance without submitting a solution, I divide my train data set into 2 parts
# 80% for training and 20% for testing
train2 <- sample_frac(train, 0.8, replace = FALSE)
test2  <- subset(train, !(train$passengerid %in% train2$passengerid))


# Redo original decision tree with the new sets
fitclass <- rpart(survived ~ age + sex + pclass + embarked + fare + sibsp, data=train2, method="class")
prp(fitclass, type = 3, extra = 3, fallen.leaves = TRUE)

predicted <- predict(fitclass, test2, type = "class")
mean(predicted == test2$survived) # 0.84 (better than when comparing to original test set)

# Redo complex decision tree with the new sets
fitclass <- rpart(survived ~ age + sex + pclass + embarked + fare + sibsp, data=train2, method="class", control = morecomplex)
prp(fitclass, type = 3, extra = 3, fallen.leaves = TRUE)

predicted <- predict(fitclass, test2, type = "class")
mean(predicted == test2$survived) # 0.798

# Try different parameters to control
morecomplex <- rpart.control(minsplit=4, cp=0)
fitclass <- rpart(survived ~ age + sex + pclass + embarked + fare + sibsp, data=train2, method="class", control = morecomplex)
mean(predicted <- predict(fitclass, test2, type = "class") == test2$survived) # 0.826

morecomplex <- rpart.control(minsplit=4, cp=0.01)
fitclass <- rpart(survived ~ age + sex + pclass + embarked + fare + sibsp, data=train2, method="class", control = morecomplex)
mean(predicted <- predict(fitclass, test2, type = "class") == test2$survived) # 0.848

morecomplex <- rpart.control(minsplit=8, cp=0.0025)
fitclass <- rpart(survived ~ age + sex + pclass + embarked + fare + sibsp + parch, data=train2, method="class", control = morecomplex)
mean(predicted <- predict(fitclass, test2, type = "class") == test2$survived) # 0.893

prp(fitclass, type = 3, extra = 3, fallen.leaves = TRUE)

# submit solution from best tree (minsplit=8, cp=0.00025)
fitclass <- rpart(survived ~ age + sex + pclass + embarked + fare + sibsp + parch, data=train, method="class", control = morecomplex)
test$survived <- predict(fitclass, test, type = "class")
levels(test$survived) <- c(0, 1)
submitdata(test$passengerid, as.character(test$survived), "decisiontree3.csv")
# nope, this performed worse than my previous submission. Overfitting? 



# After feature engineering, make new decision tree

fitclass <- rpart(survived ~ age + sex + pclass + embarked + fare + familysize + title + sibsp + parch + familyid, data=train, method="class")

# Plot decision trees
prp(fitclass, type = 3, extra = 3, fallen.leaves = TRUE)

# Export solution 
test$survived <- predict(fitclass, test, type = "class")
levels(test$survived) <- c(0, 1)
submitdata(test$passengerid, as.character(test$survived), "featureengineering.csv")

# Doing a lot better!
# Can i improve this further by trying to do something about the missing age
# Also, explore other features, such as cabin
table(combi$cabin)

# is there any difference between A, B, C and D cabins?
# Does the cabin numbermake a difference?

# No obvious connection, but doesn't hurt to include in the decision tree
# Passengers with known sections seem to survive more often 
qplot(cabinsection, data=train, fill=as.factor(survived), binwidth=1)
# only plot known section, now it seems to be some connections
qplot(cabinsection, data=train[!is.na(train$cabinsection),], fill=as.factor(survived), binwidth=1)
qplot(cabinnumber, data=train[!is.na(train$cabinsection),], fill=as.factor(survived), binwidth=1)

# Making tree to check for improvements
fitclass <- rpart(survived ~ age + sex + pclass + embarked + fare + familysize + title + sibsp + parch + familyid + cabinsection, data=train, method="class")
prp(fitclass, type = 3, extra = 3, fallen.leaves = TRUE)
# The tree look exactly the same as before

# Try changing bin sizes
smallerbins <- rpart.control(minsplit=20, cp=0.01)
fitclass <- rpart(survived ~ age + sex + pclass + embarked + fare + familysize + title + sibsp + parch + familyid + cabinsection, data=train, method="class", control=smallerbins)
prp(fitclass, type = 4, extra = 3, fallen.leaves = TRUE)
# changing the complexity from 0.01 to 0.005 makes a large difference in the plot

# Export solution 
test$survived <- predict(fitclass, test, type = "class")
levels(test$survived) <- c(0, 1)
submitdata(test$passengerid, as.character(test$survived), "addingcabinsection.csv")

# No, does not make a difference. Maybe because of the large amount of missing data
# What about age? 
# Is there a pattern for missing age?
qplot(age, data=train, fill=as.factor(survived), binwidth=1)
sum(is.na(train$age))
# 177 of 891 missing values 
# Idea: Construct a decision tree using the passengers with known age
# Use this to estimate the age of the remaining passengers
fitage <- rpart(age ~ sex + pclass + embarked + fare + title + sibsp + parch + familysize + cabinsection, data=combi[!is.na(combi$age),], method="anova")
combi$age[is.na(combi$age)] <- round(predict(fitage, combi[is.na(combi$age),]),0)
# Split back into train and test data
train <- combi[1:891,]
test <- combi[892:1309,]

# I now have an estimate of age. This was probably not the best way to do it though. Feels weird to put everybody into 
# just a few age bins 

# Anyway, I can try redo the classification with the new ages, maybe it makes it a little bit better 
fitclass <- rpart(survived ~ age + sex + pclass + embarked + fare + familysize + title + sibsp + parch + familyid + cabinsection, data=train, method="class")
prp(fitclass, type = 4, extra = 3, fallen.leaves = TRUE)
# changing the complexity from 0.01 to 0.005 makes a large difference in the plot

# Export solution 
test$survived <- predict(fitclass, test, type = "class")
levels(test$survived) <- c(0, 1)
submitdata(test$passengerid, as.character(test$survived), "addingestimatedage.csv")
#nah, doesn't make a difference 

# Could there be a connection that members of the same family/cabin survive?


# Random forest approach 
set.seed(49)

fit <- randomForest(as.factor(survived) ~ pclass + sex + age + sibsp + parch + fare + embarked + title + familysize +
                      familyid2, data=train, importance=TRUE, ntree=2000)
test$survived <- predict(fit, test)
submitdata(test$passengerid, as.character(test$survived), "randomforest1.csv")

# No, performs worse than the simple tree

# New approach: Conditional influence trees
set.seed(49)
fit <- cforest(as.factor(survived) ~ pclass + sex + age + sibsp + parch + fare + embarked + title + familysize +
                 familyid + cabinsection,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
test$survived <- predict(fit, test, OOB=TRUE, type = "response")
submitdata(test$passengerid, as.character(test$survived), "randomforest3.csv")

# Yes! New score 0.8134 better than before
# Not sure why this worked better though...

# After adding cabinsection to the prediction: 0.8038, so actually worse. hmm... 

# Do something with ticket? 

# Try regresison?
# Naive Bayes? 







# Ideas for algorithms: logistic regression, knn
titanic.kknn <- kknn(survived~sex+agegroup+pclass, train=train, test=test, distance = 1, kernel ="triangular")
str(titanic.kknn)
summary(titanic.kknn)
# returns fewer values than I have rows in test data. Why?



