grab.data <- function(path) {
  require(dplyr)
  require(tidyverse)
  require(jsonlite)
  require(RCurl)
  
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
      as_tibble()}
  
  
  
  survey <- mydata[mydata$trial_type == "survey-html-form" & "age" %in% mydata$response ,]
  if(nrow(survey) > 0){
    JSONcolumn_data <-  survey %>% 
      select(response)  %>% 
      map_dfc(.f = ParseJSONColumn)
    
    survey <- cbind(survey, JSONcolumn_data)
    survey <- survey[,c("ID", colnames(JSONcolumn_data))]
    mydata <- merge(mydata, survey, by = "ID", all.x = T)
    
    mydata <- mydata %>% 
      group_by(ID) %>%
      mutate(age = max(age, na.rm = T))
  }
  
  return(mydata)
}
