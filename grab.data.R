
grab.data <- function(path) {
  require(dplyr)
  require(tidyverse)
  require(jsonlite)
  require(RCurl, warn.conflicts = F)
  
  filenames <- list.files(path = path, pattern = "\\.csv$")
  
  filenames <- paste(path, filenames, sep ="")
  
  
  for(i in 1:length(filenames)){
    a <- read.csv(filenames[[i]])
    if(i == 1) mydata <- a
    if(i != 1) mydata <- plyr::rbind.fill(mydata,a)
    
  }
  
  rm(list=setdiff(ls(), "mydata"))
  
  
  ParseJSONColumn <- function(x)  {
    str_c("[ ", str_c(x, collapse = ",", sep=" "), " ]")  %>% 
      fromJSON(flatten = T) %>% 
      as.tibble()}
  
  
  
  return(mydata)
}
