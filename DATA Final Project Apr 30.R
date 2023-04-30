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


##############################
### CONNER'S DATA CLEANING ###
##############################


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
  
  ##############################
  ### SUYASH'S DATA CLEANING ###
  ##############################
  
  fsi <- read.csv("fsi-2022.csv")
  
  fsi <- fsi %>% 
    select("Country", "Year", "P1..State.Legitimacy")
  
  fsi <- na.omit(fsi)
  
  names(fsi)[names(fsi) == "Country"] <- "country"
  
  fsi$country <- gsub("C<93>te d'Ivoire", "Cote d'Ivoire", fsi$country)

  
  ###############################
  ### FSI/COVID DATA CLEANING ###
  ###############################
  
  #Merge FSI and CovidData
  
  FSICovid <- left_join(CovidData, fsi, by = "country")
  FSICovid <- subset(FSICovid, select = -Year)
  
  
  #Save the new dataframe
  write.csv(FSICovid, file = "FSICovid.csv", row.names = FALSE)
  

  ###########################
  ### RYO'S DATA CLEANING ###
  ###########################
  
  #Loading dataset
  
  fiw <- read_csv("fiw.csv")
  
  #Renaming columns
  
  colnames(fiw)[1] = "country"
  colnames(fiw)[2] = "region"
  colnames(fiw)[3] = "c/t"
  colnames(fiw)[4] = "year"
  colnames(fiw)[5] = "status"
  colnames(fiw)[7] = "rights_1"
  colnames(fiw)[8] = "rights_2"
  
  #deleting the 1st row and 6th column
  
  fiw <- fiw[-1]
  fiw <- fiw %>% 
    select(-"...6")
  
  #Converting below variables to data.frame to numeric
  
  fiw <- fiw %>% 
    mutate(rights_1 = as.numeric(rights_1),
           rights_2 = as.numeric(rights_2))
  
  #Renaming specific countries’ name
  
  fiw[39,"country/territory"] <- "Congo Republic"
  fiw[40,"country/territory"] <- "Congo Democratic Republic"
  fiw[73,"country/territory"] <- "Guinea Bissau"
  
  mutate(country/territory = case_when(`country/territory` == "Congo (Brazzaville)" ~ "Congo Republic",
                                       `country/territory` == "Congo (Kinshasa)" ~ "Congo Democratic Republic"))
  
  
  
  #Exporting the dataset as a csv file
  
  write.csv(new,
            file = "project_data.csv")
  
  ###################################
  ### JOINING ALL THREE DATASETS ###
  ##################################
  
  #merging data
  
  ProjectData <- merge(fsi_covid, fiw, by = "country")
  

  ###################################
  ### FINAL PROJECT DATA CLEANING ###
  ###################################
  
  
  ProjectData <- read.csv("project_data.csv")
   
  
    #Remove West Bank and Gaza
    
    ProjectData <- ProjectData %>% 
      filter(!(country == "West Bank and Gaza"))
    
    #Remove extra variables 'c.t' and '...1'
      
    ProjectData <- ProjectData[, !(names(ProjectData) == "year")]
    ProjectData <- ProjectData[, !(names(ProjectData) == "X")]
    ProjectData <- ProjectData[, !(names(ProjectData) == "c.t")]
    ProjectData <- ProjectData[, !(names(ProjectData) == "...1")]
    ProjectData <- ProjectData[, !(names(ProjectData) == "Date")]
    ProjectData <- ProjectData[, !(names(ProjectData) == "VaxxedPercentage")]
    
    #Make Freedom Status a factor
    
    ProjectData <- ProjectData %>%
      mutate(Freedom_Status = as.factor(Freedom_Status))
    
    
    
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
  
  names(ProjectData)[names(ProjectData) == "State_Legitimacy"] <- "State_Illegitimacy"
  
  names(ProjectData)[names(ProjectData) == "freedom_status"] <- "Freedom_Status"
  
  names(ProjectData)[names(ProjectData) == "civil_liberties"] <- "Civil_Liberties"
  
  
  #Fix the Freedom Status Variable (Some formulas needed numeric, some needed character)
  
  
  ProjectData <- ProjectData %>% 
    mutate(Freedom_Status = case_when(Freedom_Status %in% "F" ~ "3",
                              Freedom_Status %in% "NF" ~ "1",
                              Freedom_Status %in% "PF" ~ "2"))
  
  ProjectData <- ProjectData %>% 
    mutate(Freedom_Status = as.numeric(Freedom_Status))
  
  ProjectData <- ProjectData %>% 
    mutate(Freedom_Status = as.character(Freedom_Status))

  # Remove missing values
  ProjectData <- na.omit(ProjectData)
  
  
  
  
  #Fix the VaxxedPercentage Variable
  
  #Fix the variables VaxxedPercentage and VaxxedOncePercentage
  ProjectData$VaxxedPercentage <- (ProjectData$Ppl_1_Dose / ProjectData$Population) * 100
  
  ProjectData$VaxxedPercentage <- (ProjectData$Doses_admin / ProjectData$Population) * 100
  
  
  
  
  ##########################
  ### SUMMARY STATISTICS ###
  ##########################
  
  summary(ProjectData)
  
 # plot stacked bar chart of "freedom status" by "region"
  ProjectData %>%
    count(Freedom_Status, Region) %>%
    group_by(Freedom_Status) %>%
    mutate(prop = n/sum(n)) %>%
    ggplot(aes(x = Freedom_Status, y = prop, fill = Region)) +
    geom_col() +
    labs(title = "Proportions of Freedom Status by Region",
         x = "Freedom Status", y = "Proportion") +
    scale_fill_brewer(palette = "Set3")+
    theme_light()
  
  
  
  
  
  ###########################
  ### DATA VISUALIZATIONS ###
  ###########################
  
  #Set 1 ... FREEDOM STATUS
  
  ggplot(ProjectData, aes(x = Freedom_Status, y = TotalDosesPerIndividual)) +
    geom_boxplot(aes()) +
    labs(title = "Boxplots of Total Doses Per Individual and Freedom Status",
         x = "Freedom Status", y = "Total Doses Per Individual")  +
    theme_light()
  
  ggplot(ProjectData, aes(x = Freedom_Status, y = TotalDosesPerIndividual)) +
    geom_point(aes()) +
    labs(title = "Scatterplot of Total Doses Per Individual and Freedom Status",
         x = "Freedom Status", y = "Total Doses Per Individual")  +
    geom_smooth(method="lm", se=FALSE, color="red") +
    theme_light() +
    xlim(0.5, 3.5)
  
  ggplot(ProjectData, aes(x = Freedom_Status, y = VaxxedOncePercentage)) +
    geom_boxplot(aes()) +
    labs(title = "Boxplots of Vaccination Percentageand Freedom Status",
         x = "Freedom Status", y = "Vaccination Percentage")  +
    theme_light()
  
  ggplot(ProjectData, aes(x = Freedom_Status, y = VaxxedOncePercentage)) +
    geom_point(aes()) +
    labs(title = "Scatterplot of Vaccination Percentageand Freedom Status",
         x = "Freedom Status", y = "Vaccination Percentage")  +
    geom_smooth(method="lm", se=FALSE, color="red") +
    theme_light() +
    xlim(0.5, 3.5)
  
  
  #Set 2 ... CIVIL LIBERTIES
  
  
  ggplot(ProjectData, aes(Civil_Liberties, VaxxedOncePercentage, color = factor(Freedom_Status))) +
    geom_point() +
    scale_color_manual(name = "Freedom Status", values = c("red", "orange", "darkgreen"))+
    theme_minimal() +
    geom_smooth(method="lm", se=FALSE, color="blue", na.rm=TRUE) +
    ggtitle("Vaccination Percentage and Civil Liberties") +
    xlab("Civil Liberties") +
    ylab("Vaccination Percentage") +
    theme_light()
  
  
  ggplot(ProjectData, aes(Civil_Liberties, TotalDosesPerIndividual, color = factor(Freedom_Status))) +
    geom_point() +
    scale_color_manual(name = "Freedom Status", values = c("red", "orange", "darkgreen"))+
    theme_minimal() +
    geom_smooth(method="lm", se=FALSE, color="blue", na.rm=TRUE) +
    ggtitle("Total Doses Per Individual and Civil Liberties") +
    xlab("Civil Liberties") +
    ylab("Total Doses Per Individual") +
    theme_light()
  

  ProjectData %>%
    ggplot(aes(Civil_Liberties, VaxxedOncePercentage, linetype = factor(Freedom_Status))) +
    geom_jitter(alpha = .2, size = 2.5) +
    geom_smooth(color = "blue", se = FALSE) +
    theme_bw(base_size = 12)+
    ylim(0, 100) +
    geom_smooth(method="lm", se=FALSE, color="red", na.rm=TRUE)+
    ggtitle("Civil Liberties and Vaccination Percentage") +
    xlab("Civil Liberties") +
    ylab("Vaccination Percentage") +
    scale_linetype(name = "Freedom Status")
  
  ProjectData %>%
    ggplot(aes(Civil_Liberties, TotalDosesPerIndividual, linetype = factor(Freedom_Status))) +
    geom_jitter(alpha = .2, size = 2.5) +
    geom_smooth(color = "blue", se = FALSE) +
    theme_bw(base_size = 12)+
    ylim(0, 100) +
    geom_smooth(method="lm", se=FALSE, color="red", na.rm=TRUE)+
    ggtitle("Civil Liberties and Total Doses Per Individual") +
    xlab("Civil Liberties") +
    ylab("Total Doses Per Individual") +
    scale_linetype(name = "Freedom Status") +
    ylim(0,3)

  #Set 3 ... FREEDOM OF EXPRESSION
  
  ggplot(ProjectData, aes(FOE, VaxxedOncePercentage, color = factor(Freedom_Status))) +
    geom_point() +
    scale_color_manual(name = "Freedom Status", values = c("red", "orange", "darkgreen"))+
    theme_minimal() +
    geom_smooth(method="lm", se=FALSE, color="blue", na.rm=TRUE) +
    ggtitle("Vaccination Percentage and Freedom of Expression") +
    xlab("Freedom of Expression") +
    ylab("Vaccination Percentage") +
    theme_light()
  
  ggplot(ProjectData, aes(FOE, TotalDosesPerIndividual, color = factor(Freedom_Status))) +
    geom_point() +
    scale_color_manual(name = "Freedom Status", values = c("red", "orange", "darkgreen"))+
    theme_minimal() +
    geom_smooth(method="lm", se=FALSE, color="blue", na.rm=TRUE) +
    ggtitle("Vaccination Percentage and Total Doses Per Individual") +
    xlab("Freedom of Expression") +
    ylab("Total Doses Per Individual") +
    theme_light()
  
  
  ProjectData %>%
    ggplot(aes(FOE, VaxxedOncePercentage, linetype = factor(Freedom_Status))) +
    geom_jitter(alpha = .2, size = 2.5) +
    geom_smooth(color = "blue", se = FALSE) +
    theme_bw(base_size = 12)+
    geom_smooth(method="lm", se=FALSE, color="red", na.rm=TRUE)+
    ggtitle("Freedom of Expression and Vaccination Percentage") +
    xlab("Freedom of Expression") +
    ylab("Vaccination Percentage") +
    scale_linetype(name = "Freedom Status") +
    ylim(0, 100) 
  
  ProjectData %>%
    ggplot(aes(FOE, TotalDosesPerIndividual, linetype = factor(Freedom_Status))) +
    geom_jitter(alpha = .2, size = 2.5) +
    geom_smooth(color = "blue", se = FALSE) +
    theme_bw(base_size = 12)+
    geom_smooth(method="lm", se=FALSE, color="red", na.rm=TRUE)+
    ggtitle("Freedom of Expression and Total Doses Per Individual") +
    xlab("Freedom of Expression") +
    ylab("Total Doses Per Individual") +
    scale_linetype(name = "Freedom Status")  +
    ylim(0,3)
  
  
    #Set 4 ... State Illegitimacy
    
    ggplot(ProjectData, aes(State_Illegitimacy, VaxxedOncePercentage, color = factor(Freedom_Status))) +
      geom_point() +
      scale_color_manual(name = "Freedom Status", values = c("#FF4040", "orange", "darkgreen"))+
      theme_minimal() +
      geom_smooth(method="lm", se=FALSE, color="blue", na.rm=TRUE) +
      ggtitle("Vaccination Percentage and State Illegitimacy") +
      xlab("State Illegitimacy") +
      ylab("Vaccination Percentage") +
      theme_light()
    
    ggplot(ProjectData, aes(State_Illegitimacy, TotalDosesPerIndividual, color = factor(Freedom_Status))) +
      geom_point() +
      scale_color_manual(name = "Freedom Status", values = c("#FF4040", "orange", "darkgreen"))+
      theme_minimal() +
      geom_smooth(method="lm", se=FALSE, color="blue", na.rm=TRUE) +
      ggtitle("Total Vaccine Doses Per Individual and State Illegitimacy") +
      xlab("State Illegitimacy") +
      ylab("Total Doses Per Individual") +
      theme_light()
    
    ProjectData %>%
      ggplot(aes(State_Illegitimacy, VaxxedOncePercentage, linetype = factor(Freedom_Status))) +
      geom_jitter(alpha = .2, size = 2.5) +
      geom_smooth(color = "blue", se = FALSE) +
      theme_bw(base_size = 12)+
      geom_smooth(method="lm", se=FALSE, color="red", na.rm=TRUE)+
      ggtitle("State Illegitimacy and Vaccination Percentage") +
      xlab("State Illegitimacy") +
      ylab("Vaccination Percentage") +
      scale_linetype(name = "Freedom Status") +
      ylim(0, 100) 
    
    ProjectData %>%
      ggplot(aes(State_Illegitimacy, TotalDosesPerIndividual, linetype = factor(Freedom_Status))) +
      geom_jitter(alpha = .2, size = 2.5) +
      geom_smooth(color = "blue", se = FALSE) +
      theme_bw(base_size = 12)+
      geom_smooth(method="lm", se=FALSE, color="red", na.rm=TRUE)+
      ggtitle("State Illegitimacy and Total Doses Per Individual") +
      xlab("State Illegitimacy") +
      ylab("Total Doses Per Individual") +
      scale_linetype(name = "Freedom Status") +
      ylim(0, 3) 
    
    #Scraps
    ProjectData %>%
      group_by(day = wday(dep_time, label = TRUE, week_start = 1)) %>%
      summarize(avg_delay = mean(dep_delay)) %>%
      ggplot(aes(day, avg_delay)) +
      geom_line(aes(group = 1)) +
      geom_point()
    
  
  #### REGRESSION ####

    est1 <- lm(TotalDosesPerIndividual ~ State_Illegitimacy + Region, data = ProjectData)
    est2 <- lm(TotalDosesPerIndividual ~ Freedom_Status + Region, data = ProjectData)
    est3 <- lm(TotalDosesPerIndividual ~ Civil_Liberties + Region, data = ProjectData)
    est4 <- lm(TotalDosesPerIndividual ~ FOE + Region, data = ProjectData)
    est5 <- lm(VaxxedOncePercentage ~ State_Illegitimacy + Region, data = ProjectData)
    stargazer(est1, est2, est3, est4,est5, type = "text", keep.stat = "n")
    
    
  
  #### SUMMARY STATISTICS ####
    
    sum_illegimacy_project <- project %>% 
      group_by(Region) %>% 
      summarize(Total_ille = sum(State_Illegitimacy,na.rm = TRUE))
    
    sum_illegimacy_project %>% 
      ggplot(aes(x = Region,
                 y = Total_ille,
                 fill = Region)) +
      geom_bar(stat = "identity") +
      labs(y = "State Illegitimacy",
           title = "State Illegitimacy by Region") +
      scale_fill_brewer(palette = "Set3") +
      theme_bw()
    ```
    
    #Making a visualization based on Freedom of Expression by region
    
    ```{r}
    foe_project <- project %>% 
      group_by(Region) %>% 
      summarize(Total_foe = sum(FOE,na.rm = TRUE))
    
    foe_project %>% 
      ggplot(aes(x = Region,
                 y = Total_foe,
                 fill = Region)) +
      geom_bar(stat = "identity") +
      labs(y = "Freedom of Expression",
           title = "Freedom of Expression by Region") +
      scale_fill_brewer(palette = "Set3") +
      theme_bw()
    ```
    
    #Making a visualization based on Freedom of Expression by region
    
    ```{r}
    cl_project <- project %>% 
      group_by(Region) %>% 
      summarize(Total_cl = sum(Civil_Liberties,na.rm = TRUE))
    
    cl_project %>% 
      ggplot(aes(x = Region,
                 y = Total_cl,
                 fill = Region)) +
      geom_bar(stat = "identity") +
      labs(y = "Civil Liberties",
           title = "Civil Liberties by Region") +
      scale_fill_brewer(palette = "Set3") +
      theme_bw()
    ```
  
  
  #Save the csv
  
  write.csv(ProjectData, file = "ProjectDataApr30.csv", row.names = FALSE)
  
  
  