complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  directory <- paste(directory, "/", sep = "")
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  nobs <- c()
  
  for (i in id){
    fileName <- paste(directory, sprintf("%03.0f", i), ".csv", sep = "")
    fileDataFrame <- read.csv(fileName)
    # Append all non NA values to pollutantValues
    nobs <- c(nobs, nrow(na.omit(fileDataFrame)))
  }
  
  data.frame(id, nobs) 
}