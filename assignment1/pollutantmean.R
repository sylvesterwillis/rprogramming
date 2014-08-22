pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files  
  directory <- paste(directory, "/", sep = "")
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  pollutantValues <- c()
  
  for (i in id){
    fileName <- paste(directory, sprintf("%03.0f", i), ".csv", sep = "")
    fileDataFrame <- read.csv(fileName)
    # Append all non NA values to pollutantValues
    pollutantValues <- c(pollutantValues, fileDataFrame[!is.na(fileDataFrame[pollutant]), pollutant])
  }
  mean(pollutantValues)
}