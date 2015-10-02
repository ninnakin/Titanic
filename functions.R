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
  
  return(dataset)
}

# Writes data to file for submission
submitdata <- function(passengerid, survived, filename="submission.csv"){
  submissiondata <- cbind(passengerid, as.numeric(survived))
  colnames(submissiondata) <- c("PassengerId","Survived")
  filename = paste0("Solutions/" , filename)
  
  write.table(submissiondata, file=filename, col.names=TRUE, row.names=FALSE, sep=",")
}
