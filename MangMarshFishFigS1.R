# MangMarshFish Fig. S1

# Replicate figure S1 from "Marshes to mangroves: Resident attitudes 
# and perceptions indicate perceived trade-offs in fisheries ecosystem services"

# Source: github.com/shswinea/MangMarshFish

# Savannah Swinea
# swinea.s@northeastern.edu
# January 17, 2024

# Before you begin:
# Install these packages if you haven't already, and retrieve them from your library
# Also, put your data file in the same folder as this R script on your computer

#____________________________________________________________________________________________________________

# install.packages("rstudioapi")
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("modelsummary")
library(rstudioapi)
library(readxl)
library(tidyverse)
library(modelsummary)

current_path = rstudioapi::getActiveDocumentContext()$path # finds the file path for this R script
setwd(dirname(current_path)) # sets your working directory equal to that file path


file_path <- './H4.x954.0000003_MangMarshFish.xlsx' # identifying the dataset 
df <- read_excel(path = file_path) # reads in data from Excel spreadsheet

#____________________________________________________________________________________________________________
# Cleaning and Re-Classifying Data

df$Sex[is.na(df$Sex)] <- "Prefer not to answer"
df$Race[is.na(df$Race)] <- "Prefer not to answer"
df$HHI2020[is.na(df$HHI2020)] <- "Prefer not to answer"
df$Education[df$Education == "Less than high school" | 
               df$Education == "High school diploma or GED"] <- "High school or less"
df$Education[is.na(df$Education)] <- "Prefer not to answer"
df$Politics[is.na(df$Politics)] <- "Prefer not to answer"

#____________________________________________________________________________________________________________
# Setting level orders for demographic variables

df$StudySite <- factor(df$StudySite, levels = c("Cedar Key / Homosassa", "Panama City Beach", 
                                                "Galveston", "Corpus Christi"))

df$Sex <- factor(df$Sex, levels = c("Man", "Woman", "Other", "Prefer not to answer"))

df$Race <- factor(df$Race, levels = c("White", "Black or African American",
                                      "American Indian or Alaska Native", "Asian",
                                      "Native Hawaiian or Pacific Islander", "Hispanic or Latino",
                                      "Prefer not to answer"))

df$HHI2020 <- factor(df$HHI2020, levels = c('$25k or less', '$25,001 to $35k',
                                            '$35,001 to $50k', '$50,001 to $75k',
                                            '$75,001 to $100k', '$100,001 to $150k',
                                            '150,001 to 250k', 'More than $250k',
                                            'Prefer not to answer'))

df$Education <- factor(df$Education, levels = c("High school or less", "Some college or 2 year degree",
                                                "Bachelor's degree", "Master's degree",
                                                "Law or MD", "Doctorate (PhD)",
                                                "Prefer not to answer"))

df$Politics <- factor(df$Politics, levels = c("Very liberal", "Somewhat liberal",
                                              "Moderate / middle of the road", "Somewhat conservative",
                                              "Very conservative", "Other",
                                              "Prefer not to answer"))

#____________________________________________________________________________________________________________
# Producing the demographics table!

table <- datasummary(1 + (Heading("Gender")*Sex + Heading("Race/Ethnicity")*Race + Education + 
                        Heading("Household Income (2020)")*HHI2020 + Heading("Political Affiliation")*Politics) ~ 
                       Factor(StudySite) * Heading("% of Respondents")*(Percent(denom = "col")) + N, data = df, fmt = 0,
                     title = 'Sample Demographics',
                     notes = c('n = 530'), output = 'MangMarshFishFigS1.docx')
# this will export as a Word document in your working directory


# Resources used to produce this figure:

# https://elbersb.de/public/pdf/web-7-regression-tables-graphs.pdf
# https://vincentarelbundock.github.io/modelsummary/articles/datasummary.html
# https://vincentarelbundock.github.io/modelsummary/reference/datasummary.html
