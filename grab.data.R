download_osf_data <- function(node){
# Download from OSF
require(osfr)

# Verify Access token is loaded
Sys.getenv("OSF_PAT")

# List files
psych_rp <- osf_retrieve_node(node)
psych_rp <- osf_ls_files(psych_rp)
psych_rp[1,]

# Download files
x <- osf_download(psych_rp, conflicts = "overwrite")

# List Files
filenames <- x$local_path

# Merge Files
for(i in 1:length(filenames)){
  a <- read.csv(filenames[[i]])
  if(i == 1) mydata <- a
  if(i != 1) mydata <- plyr::rbind.fill(mydata,a)
  
}


return(mydata)

}


# Merge Local Data
grab.data <- function(path, surveycapture = T) {
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
  
  
  
  if(surveycapture == T){
  ParseJSONColumn <- function(x)  {
    str_c("[ ", str_c(x, collapse = ",", sep=" "), " ]")  %>% 
      fromJSON(flatten = T) %>% 
      as_tibble()}
  
  

  survey <- mydata[mydata$trial_type == "survey-html-form",]
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
  }
  
  return(mydata)
}
