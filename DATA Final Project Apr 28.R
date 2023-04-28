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
library(ggplot2)
library(tidyr)


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
  
  #...
  
  names(ProjectData)[names(ProjectData) == "People_at_least_one_dose"] <- "Ppl_1_Dose"
  
  names(ProjectData)[names(ProjectData) == "country"] <- "Country"
  
  names(ProjectData)[names(ProjectData) == "region"] <- "Region"
  
  names(ProjectData)[names(ProjectData) == "state legitimacy"] <- "State_Legitimacy"
  
  names(ProjectData)[names(ProjectData) == "freedom_status"] <- "Freedom_Status"
  
  names(ProjectData)[names(ProjectData) == "civil_liberties"] <- "Civil_Liberties"
  
  
  #Fix the Freedom Status Variable
  
  
  ProjectData <- ProjectData %>% 
    mutate(Freedom_Status = case_when(Freedom_Status %in% "F" ~ "3",
                              Freedom_Status %in% "NF" ~ "1",
                              Freedom_Status %in% "PF" ~ "2"))
  
  ProjectData <- ProjectData %>% 
    mutate(Freedom_Status = as.character(Freedom_Status))

  # Remove missing values
  ProjectData <- na.omit(ProjectData)
  
  ### SUMMARY STATISTICS VISUALIZATIONS ###

  
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
  
  
  ### DATA VISUALIZATIONS ###
 
  ggplot(ProjectData, aes(x=Civil_Liberties, y=VaxxedPercentage)) +
    geom_point() +
    labs(title="Relationship Between VaxxedPercentage and Freedom Status",
         x="Civil Liberties", y="Vaxxed Percentage") +
    geom_smooth(method="lm", se=FALSE, color="red") +
    theme_classic()
  
  ggplot(ProjectData, aes(x = Freedom_Status, y = VaxxedPercentage)) +
    geom_boxplot(aes()) +
    labs(title = "Boxplots of Civil Liberties by Freedom Status",
         x = "Freedom Status", y = "VaxxedPercentage") +
  geom_smooth(method="lm", se=FALSE, color="red")

  
  ggplot(ProjectData, aes(x = Freedom_Status, y = VaxxedPercentage)) +
    geom_point(aes()) +
    geom_smooth(method="lm", se=FALSE, color="red", na.rm=TRUE) +
    labs(title = "Boxplots of Civil Liberties by Freedom Status",
         x = "Freedom Status", y = "VaxxedPercentage") 
    
    
    
   
  
  
  ###Scraps
  
  # plot boxplots of "civil_liberties" by "freedom_status" and "VaxxedPercentage"

  
  ggplot(ProjectData, aes(x=Freedom_Status, y=VaxxedPercentage)) +
    geom_violin(scale="width", trim=FALSE) +
    geom_boxplot(width=0.1, fill="white", alpha=0.7) +
    labs(title="VaxxedPercentage by Region and Freedom Status",
         x="Freedom Status", y="VaxxedPercentage") +
    theme_classic()
  
  
  
  
  ProjectData %>%
    ggplot(aes(hp, qsec, linetype = factor(freedom_status))) +
    geom_jitter(alpha = 0.2, size = 2.5) +
    geom_smooth(color = "black", se = FALSE) +
    theme_bw(base_size = 12) +
    ggtitle("Relationship Between Horsepower and Quarter Mile Time") +
    xlab("Horsepower") +
    ylab("Quarter Mile Time") +
    scale_linetype(name = "Cylinder Count")
  
  
  

  
  
  write.csv(ProjectData, file = "ProjectData.csv", row.names = FALSE)
  
  
  ProjectData <- read.csv("ProjectDataApr28New.csv")
 
  
  