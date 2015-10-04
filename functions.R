# Cleans input data 
cleandata <- function(dataset){
  
  names(dataset) <- tolower(names(dataset))
  
  # Create factors 
  dataset$pclass <- as.factor(dataset$pclass)
  levels(dataset$pclass) <- c("1st Class", "2nd Class", "3rd Class")
  dataset$sex <- factor(dataset$sex, levels=c("female", "male"))
  levels(dataset$sex) <- c("Female", "Male")
  
  # remove factors
  dataset$name <- as.character(dataset$name)
  dataset$cabin <- as.character(dataset$cabin)
  
  return(dataset)
}

# Writes data to file for submission
submitdata <- function(passengerid, survived, filename="submission.csv"){
  submissiondata <- cbind(passengerid, as.numeric(survived))
  colnames(submissiondata) <- c("PassengerId","Survived")
  filename = paste0("Solutions/" , filename)
  
  write.table(submissiondata, file=filename, col.names=TRUE, row.names=FALSE, sep=",")
}

set.title <- function(combi){
  # Extract titles from second word of names column
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
  rm(n)
  return(combi)
}

set.family.info <- function(combi){
  
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

  # Reduce factors for familyid for use in random forest
  combi$familyid2 <- as.character(combi$familyid)
  combi$familyid2[combi$familysize <= 3] <- 'Small'
  combi$familyid2 <- factor(combi$familyid2)
  
  return(combi)
}

predict.age <- function(combi){
  
  # Estimate age where missing using decision tree
  fitage <- rpart(age ~ sex + pclass + embarked + fare + title + sibsp + parch + familysize, data=combi[!is.na(combi$age),], method="anova")
  combi$age[is.na(combi$age)] <- round(predict(fitage, combi[is.na(combi$age),]),0)
  return(combi)
}
  
set.cabin <- function(cabin){
  # get cabin section and number
  # Some passengers have several cabins. In this case just choose the first one (I assume they are located close to each other)
  singlecabin <- sapply(strsplit(combi$cabin, " "), function(x){x[1]})
  combi$cabinnumber  <- as.numeric(sub("[^0-9]", "", singlecabin))
  combi$cabinsection <- gsub("[^A-Z]", "", singlecabin)
  combi$cabinsection <- as.factor(combi$cabinsection)
  
  # Estimate msising cabin sections using random forest
  # Cabin is mostly specified for passengers in first class
  # because of this I will only estimate for 1st class passengers
  
  unknown  <- which(is.na(combi$cabinsection) & combi$pclass=="1st Class")
  fitcabin <- rpart(cabinsection ~ age + sex + pclass + embarked + fare + title + sibsp + parch + familysize, data=combi[which(!is.na(combi$cabinsection) & combi$pclass=="1st Class"),], method="class")
  combi$cabinsection[unknown] <- predict(fitcabin, combi[unknown,], type="class")
  levels(combi$cabinsection)<-c(levels(combi$cabinsection), c("2","3"))
  combi$cabinsection[which(is.na(combi$cabinsection) & combi$pclass=="2nd Class")] <- "2"
  combi$cabinsection[which(is.na(combi$cabinsection) & combi$pclass=="3rd Class")] <- "3"  
  return(combi)  
}  
  
# Extract new features and reform old ones 
featureengineering <-function(combi){

  # Set missing values of embarked and fare to most common value
  combi$embarked[which(is.na(combi$embarked))]="S"
  combi$fare[which(is.na(combi$fare))]<-median(combi$fare, na.rm=TRUE)

  combi <- set.title(combi)
  combi <- set.family.info(combi)
  combi <- predict.age(combi)
  combi <- set.cabin(combi)
  return(combi)
}








