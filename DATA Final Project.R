setwd("C:\\ParkerR")
getwd()  
list.files()

library(tidyverse)
library(stargazer)
library(haven)
library(magrittr) 
library(tibble)
library(forcats)
library(purrr)

####################
####################
####################

CountryData <- read.csv("time_series_covid19_vaccine_doses_admin_global.csv")
  clean_CountryData <- CountryData[, c("Country_Region", "Population")]


VaxData <- read.csv("time_series_covid19_vaccine_global.csv")
  clean_VaxData <- VaxData[, c("Date", "Country_Region", "Doses_admin", "People_at_least_one_dose")]
  clean_VaxData <- clean_VaxData[clean_VaxData$Date == "2022-03-01", ]

  library(dplyr)
  
  CovidData <- left_join(clean_VaxData, clean_CountryData, by = "Country_Region")

  #Create a new variable called VaxxedOncePercentage
  CovidData$VaxxedOncePercentage <- (CovidData$People_at_least_one_dose / CovidData$Population) * 100
  
  #Create a new variable called TotalDosesPerIndividual
  CovidData$TotalDosesPerIndividual <- CovidData$Doses_admin / CovidData$Population
  
  #Rename the country variable
  names(CovidData)[names(CovidData) == "Country_Region"] <- "country"
  
  #Remove world from country
  CovidData <- subset(CovidData, country != "World")

  #Remove all extra countries 
  CovidData <- CovidData %>% 
    filter(!(country == "United Kingdom" & is.na(Population))) 
    
  CovidData <- CovidData %>% 
    filter(!(country == "China" & is.na(Population))) 
  
  CovidData <- CovidData %>% 
    filter(!(country == "Denmark" & is.na(Population))) 
  
  CovidData <- CovidData %>% 
    filter(!(country == "France" & is.na(Population))) 
  
  CovidData <- CovidData %>% 
    filter(!(country == "Netherlands" & is.na(Population))) 
  
  CovidData <- CovidData %>% 
    filter(!(country == "New Zealand" & is.na(Population))) 
  
  #Recognize Taiwan as an independent country
  CovidData$country[CovidData$country == "Taiwan*"] <- "Taiwan"
  
  #Uniform country names
  CovidData$country[CovidData$country == "Congo (Brazzaville)"] <- "Congo Republic"
  CovidData$country[CovidData$country == "Congo (Kinshasa)"] <- "Congo Democratic Republic"
  CovidData$country[CovidData$country == "Guinea-Bissau"] <- "Guinea Bissau"
  CovidData$country[CovidData$country == "US"] <- "United States"
  CovidData$country[CovidData$country == "Korea, South"] <- "South Korea"
  
  
  #Save and download the dataframe to share
  write.csv(CovidData, file = "CovidData.csv", row.names = FALSE)
  
  #FSI Data
  fsi <- read.csv("fsi-2022.csv")
  
  fsi <- fsi %>% 
    select("Country", "Year", "P1..State.Legitimacy")
  
  fsi <- na.omit(fsi)
  
  names(fsi)[names(fsi) == "Country"] <- "country"
  
  fsi$country <- gsub("C<93>te d'Ivoire", "Cote d'Ivoire", fsi$country)
  
  
  
  #Merge FSI and CovidData
  
  FSICovid <- left_join(CovidData, fsi, by = "country")
  FSICovid <- subset(FSICovid, select = -Year)
  
  
  #Save the new dataframe
  write.csv(FSICovid, file = "FSICovid.csv", row.names = FALSE)
  
  