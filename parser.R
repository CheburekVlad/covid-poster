path <<- "/home/cheburek/Desktop/csse_covid_19_daily_reports"
setwd(path)
library(dplyr)

days = tools::file_path_sans_ext(list.files(path, pattern = "\\.csv")[61:1143])
country= 

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
  days = tools::file_path_sans_ext(list.files(path, pattern = "\\.csv")[61:1143])
  dat = lapply(files,X)
  names(dat) <- days
  return(dat)
}
  
days = tools::file_path_sans_ext(list.files(path, pattern = "\\.csv")[61:1143])
l = read(path)

