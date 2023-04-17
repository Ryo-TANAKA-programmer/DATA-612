## DATA-612 Governments Analysis Project

### **Suyash Pasi, Conner Parker, Ryo Tanaka**

[Fragile States Index (2022)](https://fragilestatesindex.org/2022/07/13/fragile-states-index-2022-annual-report/)
[Freedom House](https://fragilestatesindex.org/2022/07/13/fragile-states-index-2022-annual-report/)
[COVID-19 Data Repository](https://doi.org/10.1016/S1473-3099(20)30120-1)

https://github.com/CSSEGISandData/COVID-19
Country Data (includes population, we can merge this with the dataset below?)
CountryData <- read.csv("time_series_covid19_vaccine_doses_admin_global.csv")
Vaccine Data (3-9-2023 is the most recent data, maybe we can erase the rest?)
VaxData <- read.csv("time_series_covid19_vaccine_global.csv")


What insights are you looking to gain from your analysis (any hypotheses to be tested?)
Did authoritarian governments have a worse response to COVID-19?
Your plan should show how you’ll be using many of the concepts we’ve discussed in class

## What is the research question?

Our research project will explore the relationship between government oppression and the effectiveness of a nation’s Covid response. We hypothesize that a more oppressive government is less likely to have a robust and effective plan to combat the Covid-19 pandemic.  
What type of dataset is it?
These datasets come in the form of either CSV, Excel, or Google Spreadsheets. With respect to data cleaning, we are planning to use Google Spreadsheets or Excel if the files come in either of these formats. We will convert all of these datasets into CSV format before combining and organizing the most relevant variables into a new dataframe using Rstudio. We may also seek to create new variables as appropriate to support the research process. If not, we are going to use RStudio to organize the data. 
What are the possible outcomes of your research?
Our first step will be to outline a set of variables that can be used to define an oppressive government. 
Non-democratic governments, defined on a set of variables selected by the team members, did in fact poorly handle the pandemic response. In literature review that we conducted prior to this project, some democratic or non-democratic nations effectively responded to the pandemic.  We also noted some democratic  governments had relatively poor responses to the pandemic. We hypothesize that oppressive governments may not be able to effectively prioritize their resources to develop an effective plan to fight off the pandemic, and that these outliers do not represent the relationship when data from a wider sample set is analyzed.

## Final Project Proposal

Our research project will explore the relationship between oppressive governments and the effectiveness of its Covid-19 pandemic response. We hypothesize that an oppressive government is less likely to have a robust and effective plan to combat the Covid-19 pandemic.  

Our first step will be to outline a set of variables that can be used to define an oppressive government. In a literature review that we conducted prior to this project, some non-democratic or authoritarian-leaning nations effectively responded to the pandemic. We also noted some democratic governments had relatively poor responses to the pandemic. We hypothesize that oppressive governments may not be able to effectively prioritize their resources to develop an effective plan to fight off the pandemic, and that these outliers do not represent the relationship when data from a wider sample set is analyzed.

These datasets come in the form of either CSV, Excel, or Google Spreadsheets. With respect to data cleaning, we are planning to use Google Spreadsheets or Excel if the files come in either of these formats. We will convert all of these datasets into CSV format before combining and organizing the most relevant variables into a new dataframe using Rstudio. We may also seek to create new variables as appropriate to support the research process. Furthermore, we will be creating summary statistics and data visualizations to test our hypothesis and display their relationship more clearly.

### Data Sources:
[The CIRI Human Rights Dataset](https://dataverse.harvard.edu/dataverse/cirihumanrightsdata)
[Freedom House](https://docs.google.com/spreadsheets/d/12fmXLRZY9FvJDMYTgtNOUkGR_VamVyx9/edit?usp=drive_web&ouid=116956964118385825390&rtpof=true)
[COVID-19 Data Repository](https://doi.org/10.1016/S1473-3099(20)30120-1) 
[COVID-19 Data Another Repository](https://github.com/CSSEGISandData/COVID-19)

## Variables

Fragile states index
Ranking of state fragility

HR index
Economic Inequality

FIW
Freedom of Expression and Belief = D
Civil Liberties = CL
Selecting 2022 data only

## Analysis
Descriptive statistics
FIW
