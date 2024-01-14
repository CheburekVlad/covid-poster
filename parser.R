path <<- "/home/cheburek/Desktop/csse_covid_19_daily_reports"
setwd(path)
library(dplyr)
library(countrycode)

days = tools::file_path_sans_ext(list.files(path, pattern = "\\.csv")[61:1143])


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
  
get_countries = function(){
  country = l[[1083]]$Country_Region
  continents <- countrycode(sourcevar = country , origin = "country.name", destination = "continent")
  continents[is.na(continents)] <- "Other"
  df = data.frame(country,continents)
  continent_mapping <- c(
    "Antigua and Barbuda" = "North America",
    "Argentina" = "South America",
    "Bahamas" = "North America",
    "Barbados" = "North America",
    "Belize" = "North America",
    "Bolivia" = "South America",
    "Brazil" = "South America",
    "Canada" = "North America",
    "Chile" = "South America",
    "Colombia" = "South America",
    "Costa Rica" = "North America",
    "Cuba" = "North America",
    "Dominica" = "North America",
    "Dominican Republic" = "North America",
    "Ecuador" = "South America",
    "El Salvador" = "North America",
    "Grenada" = "North America",
    "Guatemala" = "North America",
    "Guyana" = "South America",
    "Haiti" = "North America",
    "Honduras" = "North America",
    "Jamaica" = "North America",
    "Mexico" = "North America",
    "Nicaragua" = "North America",
    "Panama" = "North America",
    "Paraguay" = "South America",
    "Peru" = "South America",
    "Saint Kitts and Nevis" = "North America",
    "Saint Lucia" = "North America",
    "Saint Vincent and the Grenadines" = "North America",
    "Suriname" = "South America",
    "Trinidad and Tobago" = "North America",
    "US" = "North America",
    "Uruguay" = "South America",
    "Venezuela" = "South America"
  ) # Quick and dirty workaround
  for (countr in names(continent_mapping)) {
    df$continent[df$country == countr] = continent_mapping[countr]
  }
  return(df[,c(1,3)])
}



l = read(path)
cont_count = get_countries()
