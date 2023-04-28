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
  
  #########################
  ##Import Group Dataset ##
  #########################
  
  ProjectData <- read.csv("project_data.csv")
   
  
    #Remove West Bank and Gaza
    
    ProjectData <- ProjectData %>% 
      filter(!(country == "West Bank and Gaza"))
    
    #Remove extra variables 'c.t' and '...1'
      
    ProjectData <- ProjectData[, !(names(ProjectData) == "year")]
    ProjectData <- ProjectData[, !(names(ProjectData) == "X")]
    ProjectData <- ProjectData[, !(names(ProjectData) == "c.t")]
    ProjectData <- ProjectData[, !(names(ProjectData) == "...1")]
    
  summary(ProjectData)
  
  #Rename variables
  names(ProjectData)[names(ProjectData) == "P1..State.Legitimacy"] <- "state_legitimacy"
  
  names(ProjectData)[names(ProjectData) == "rights_1"] <- "FOE"
  
  names(ProjectData)[names(ProjectData) == "rights_2"] <- "civil_liberties"
  
  names(ProjectData)[names(ProjectData) == "status"] <- "freedom_status"
  
  names(ProjectData)[names(ProjectData) == "...1"] <- ""
  
  names(ProjectData)[names(ProjectData) == "c.t"] <- ""
  
  
  #DATA VISUALIZATION
  
  # load required packages
  library(ggplot2)
  library(tidyr)
  
  
  ### WORKING ###
  
  # plot stacked bar chart of "freedom_status" by "region"
  ProjectData %>%
    count(freedom_status, region) %>%
    group_by(freedom_status) %>%
    mutate(prop = n/sum(n)) %>%
    ggplot(aes(x = freedom_status, y = prop, fill = region)) +
    geom_col() +
    labs(title = "Proportions of Freedom Status by Region",
         x = "Freedom Status", y = "Proportion") +
    scale_fill_brewer(palette = "Set3")
  
  
  # plot boxplots of "civil_liberties" by "freedom_status" and "region"
  ggplot(ProjectData, aes(x = freedom_status, y = civil_liberties)) +
    geom_boxplot(aes(fill = region)) +
    labs(title = "Boxplots of Civil Liberties by Freedom Status",
         x = "Freedom Status", y = "Civil Liberties") +
    scale_fill_brewer(palette = "Set3")
  
  
  
  # plot boxplots of "civil_liberties" by "freedom_status" and "region"
  ggplot(ProjectData, aes(x = freedom_status, y = VaxxedPercentage)) +
    geom_boxplot(aes()) +
    labs(title = "Boxplots of Civil Liberties by Freedom Status",
         x = "Freedom Status", y = "VaxxedPercentage") +
    scale_fill_brewer(palette = "Set3")
  
  
  
  
  ### NOT WORKING ###
  
  # plot scatterplot matrix for all numeric variables
  ggplot(data = ProjectData, aes(x = ..density..)) +
    geom_histogram(aes(y = ..density.., fill = factor(..count..))) +
    geom_density(alpha = 0.2) 
    theme(legend.position = "none") +
    labs(title = "Scatterplot Matrix of Numeric Variables in ProjectData",
         x = NULL, y = NULL)
  
  # plot scatterplot matrix for selected numeric variables
  ggplot(data = ProjectData, aes(x = TotalDosesPerIndividual, y = FOE)) +
    geom_point(aes(color = region)) +
    facet_grid(freedom_status ~ state_legitimacy) +
    labs(title = "Scatterplot Matrix of TotalDosesPerIndividual and FOE",
         x = "Total Doses Per Individual", y = "Freedom of Expression")
  
 
  

  