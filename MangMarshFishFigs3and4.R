# MangMarshFish Figs. 3 and 4

# Replicate figures 3 and 4 from "Marshes to mangroves: Resident attitudes 
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
# install.packages("ggpubr")
# install.packages("viridis")
# install.packages("stringr")
# install.packages("scales")
library(rstudioapi)
library(readxl)
library(tidyverse)
library(ggpubr)
library(viridis)
library(stringr)  
library(scales)

current_path = rstudioapi::getActiveDocumentContext()$path # finds the file path for this R script
setwd(dirname(current_path)) # sets your working directory equal to that file path


file_path <- './H4.x954.0000003_MangMarshFish.xlsx' # identifying the dataset 
data <- read_excel(path = file_path) # reads in data from Excel spreadsheet

#____________________________________________________________________________________________________________
# Cleaning and Re-Classifying Data

data_fac <- data %>%
  filter(MangMarshFisheries != "I don't know") # removes respondents who chose "I don't know"
# for the survey question that represents the response variable

data_fac$Education[data_fac$Education == "Less than high school"] <- "High school diploma or GED"# combines "Less than high school"
# and "High school diploma or GED" to "High school or less" because of small sample
# size for these two levels of the variable

data_fac$StudySite[data_fac$StudySite == "Cedar Key"] <- "Cedar Key/ Homosassa"

#____________________________________________________________________________________________________________
# Setting level orders for variables

data_fac$StateResidence <- factor(data_fac$StateResidence, levels = c("Florida", "Texas"))

data_fac$StudySite <- factor(data_fac$StudySite, levels = c("Cedar Key/ Homosassa", "Panama City Beach", 
                                                            "Galveston", "Corpus Christi"))

data_fac$RecFishAccess <- factor(data_fac$RecFishAccess, levels = c("Never", "Less than once per year",
                                                                    "Yearly", "Monthly", "Weekly",
                                                                    "Daily"))

data_fac$MangMarshFisheries <- factor(data_fac$MangMarshFisheries, levels = c("Mangroves are Much Better", 
                                                                              "Mangroves are Slightly Better",
                                                                              "No Difference",
                                                                              "Marshes are Slightly Better",
                                                                              "Marshes are Much Better"), ordered = TRUE)

data_fac$FishedFromShore <- factor(data_fac$FishedFromShore, levels = c("Yes", "No"))
data_fac$FishedInshore <- factor(data_fac$FishedInshore, levels = c("Yes", "No"))
data_fac$FishedOffshore <- factor(data_fac$FishedOffshore, levels = c("Yes", "No"))

#____________________________________________________________________________________________________________
# Figure 3

fig3.1 <- data_fac %>%
  drop_na(StateResidence) %>% # remove NA
  drop_na(MangMarshFisheries) %>% # remove NA from response variable
  filter(MangMarshFisheries != "I don't know") %>%
  ggplot(aes(x = StateResidence, fill = MangMarshFisheries)) + 
  geom_bar(position = "fill") + # stacked bar chart
  xlab("State of Residence") + ylab("") +
  scale_x_discrete(limits = rev, labels = function(x) str_wrap(x, width = 20)) +
  guides(fill=guide_legend(title=NULL)) + #removes legend title
  theme(legend.position = "none") + 
  scale_y_continuous(labels=scales::percent) + # scales y-axis to 100%
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_viridis(discrete = TRUE) 


fig3.2 <- data_fac %>%
  drop_na(StudySite) %>% # remove NA
  drop_na(MangMarshFisheries) %>% # remove NA from response variable
  filter(MangMarshFisheries != "I don't know") %>%
  ggplot(aes(x = StudySite, fill = MangMarshFisheries)) +
  geom_bar(position = "fill") +
  xlab("Study Site") + ylab("") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels=scales::percent) + # scales y-axis to 100%
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_viridis(discrete = TRUE) 

ggarrange(fig3.1, fig3.2, nrow = 1, common.legend = TRUE, legend = "right", labels = c("a)", "b)"))

#____________________________________________________________________________________________________________
# Figure 4

fig4.1 <- data_fac %>%
  drop_na(RecFishAccess) %>% # remove NA
  drop_na(MangMarshFisheries) %>% # remove NA from response variable
  filter(MangMarshFisheries != "I don't know") %>%
  group_by(RecFishAccess, MangMarshFisheries) %>%
  dplyr::summarize(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(x = RecFishAccess, y = prop, fill = MangMarshFisheries)) +
  geom_bar(stat = "identity") + # stacked bar chart
  xlab("Frequency of Recreational Fishing") + ylab("Proportion of Respondents") +
  scale_x_discrete(labels = label_wrap(10)) +
  guides(fill=guide_legend(title=NULL)) + #removes legend title
  scale_y_continuous(labels=scales::percent) + # scales y-axis to 100%
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_viridis(discrete = TRUE) 

fig4.2 <- data_fac %>%
  drop_na(FishedFromShore) %>% # remove NA
  drop_na(MangMarshFisheries) %>% # remove NA from response variable
  filter(MangMarshFisheries != "I don't know") %>%
  ggplot(aes(x = FishedFromShore, fill = MangMarshFisheries)) +
  geom_bar(position = "fill", show.legend = FALSE) + # stacked bar chart
  xlab("Fished From Shore") + ylab("Proportion of Respondents") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels=scales::percent) + # scales y-axis to 100%
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_viridis(discrete = TRUE) 

fig4.3 <- data_fac %>%
  drop_na(FishedInshore) %>% # remove NA
  drop_na(MangMarshFisheries) %>% # remove NA from response variable
  filter(MangMarshFisheries != "I don't know") %>%
  ggplot(aes(x = FishedInshore, fill = MangMarshFisheries)) + 
  geom_bar(position = "fill", show.legend = FALSE) + # stacked bar chart
  xlab("Fished Inshore") + ylab("") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  guides(fill=guide_legend(title=NULL)) + #removes legend title
  scale_y_continuous(labels=scales::percent) + # scales y-axis to 100%
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_viridis(discrete = TRUE) 

fig4.4 <- data_fac %>%
  drop_na(FishedOffshore) %>% # remove NA
  drop_na(MangMarshFisheries) %>% # remove NA from response variable
  filter(MangMarshFisheries != "I don't know") %>%
  ggplot(aes(x = FishedOffshore, fill = MangMarshFisheries)) +
  geom_bar(position = "fill", show.legend = FALSE) + # stacked bar chart
  xlab("Fished Offshore") + ylab("") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  guides(fill=guide_legend(title=NULL)) + #removes legend title
  scale_y_continuous(labels=scales::percent) + # scales y-axis to 100%
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_viridis(discrete = TRUE)

ggarrange(fig4.1,
          ggarrange(fig4.2, fig4.3, fig4.4, nrow = 1, labels = c("b)", "c)", "d)")),
          ncol = 1, labels = "a)")
