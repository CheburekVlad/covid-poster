path <<- "/home/cheburek/Desktop/csse_covid_19_daily_reports"
setwd(path)
library(dplyr)


X = function(file){
  Data <- read.csv(file)
  preprocessed = Data %>%
    group_by(Country_Region) %>%
    summarise(
      Confirmed = sum(Confirmed),
      Deaths = sum(Deaths),
      Recoverd = sum(Recovered),
      Active = sum(Active),
      Lat = mean(Lat, na.rm = TRUE),
      Long = mean(Long_, na.rm = TRUE))
  return(preprocessed)
}

read = function(path){
  files = list.files(path,pattern= "\\.csv", full.names = TRUE)[61:1143]
  dat = lapply(files,X)
  return(dat)
}
  

l = read(path)

