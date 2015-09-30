cleandata <- function(dataset){
  names(dataset) <- tolower(names(dataset))
  
  # Create factors 
  dataset$pclass <- as.factor(dataset$pclass)
  levels(dataset$pclass) <- c("1st Class", "2nd Class", "3rd Class")
  dataset$sex <- factor(dataset$sex, levels=c("female", "male"))
  levels(dataset$sex) <- c("Female", "Male")
  
  # Exclude not used columns
  dataset <- dataset[,names(dataset)!=c("name")]
  dataset <- dataset[,names(dataset)!=c("ticket")]
  
  return(dataset)
}