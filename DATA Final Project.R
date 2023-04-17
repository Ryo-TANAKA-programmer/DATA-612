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
  
  #Save and download the dataframe to share
  write.csv(CovidData, file = "CovidData.csv", row.names = FALSE)
  
  