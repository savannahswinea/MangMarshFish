# MangMarshFish Fig. 5

# Replicate figure 5 from "Marshes to mangroves: Resident attitudes 
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
# install.packages("VGAM")
library(rstudioapi)
library(readxl)
library(tidyverse)
library(VGAM)
library(dotwhisker)
library(viridis)

current_path = rstudioapi::getActiveDocumentContext()$path # finds the file path for this R script
setwd(dirname(current_path)) # sets your working directory equal to that file path

file_path <- './H4.x954.0000003_MangMarshFish.xlsx' # identifying the dataset 
data <- read_excel(path = file_path) # reads in data from Excel spreadsheet

#____________________________________________________________________________________________________________
# Cleaning and Re-Classifying Data

data_fac <- data %>%
  filter(MangMarshFisheries != "I don't know") # removes respondents who chose "I don't know"
# for the survey question that represents the response variable

# combines "Less than high school"
# and "High school diploma or GED" to "High school or less" because of small sample
# size for these two levels of the variable
data_fac <- data_fac %>%
  mutate(Education = recode(Education,
                            "Less than high school" = "High school or less",
                            "High school diploma or GED" = "High school or less"))

data_fac$StudySite[data_fac$StudySite == "Cedar Key"] <- "Cedar Key/ Homosassa" # recodes study site variable

data_fac <- data_fac %>% # recodes shoreline attitude variable levels
  mutate(DesShoreMang = recode(DesShoreMang, 
                                "Absent" = "No",
                                "Present" = "Yes")) %>%
  mutate(DesShoreMarsh = recode(DesShoreMarsh, 
                               "Absent" = "No",
                               "Present" = "Yes"))

#____________________________________________________________________________________________________________
# Setting level orders for variables

data_fac$StateResidence <- factor(data_fac$StateResidence, levels = c("Florida", "Texas"))

data_fac$StudySite <- factor(data_fac$StudySite, labels = c("CedarKey", "PanamaCityBeach", "Galveston", "CorpusChristi"),
                             levels = c("Cedar Key/ Homosassa", "Panama City Beach", "Galveston", "Corpus Christi"))

data_fac$RecFishAccess <- factor(data_fac$RecFishAccess, levels = c("Never", "Less than once per year",
                                                                    "Yearly", "Monthly", "Weekly",
                                                                    "Daily"),
                                 labels = c("Never", "Lessthanonceperyear",
                                            "Yearly", "Monthly", "Weekly",
                                                                    "Daily"))

data_fac$ShoreMang <- factor(data_fac$ShoreMang, levels = c("Absent", "Present"))
data_fac$ShoreMarsh <- factor(data_fac$ShoreMarsh, levels = c("Absent", "Present"))

data_fac$DesShoreMang <- factor(data_fac$DesShoreMang, levels = c("No", "Yes"))
data_fac$DesShoreMarsh <- factor(data_fac$DesShoreMarsh, levels = c("No", "Yes"))

data_fac$MangMarshFisheries <- factor(data_fac$MangMarshFisheries, levels = c("Mangroves are Much Better", 
                                                                              "Mangroves are Slightly Better",
                                                                              "No Difference",
                                                                              "Marshes are Slightly Better",
                                                                              "Marshes are Much Better"), ordered = TRUE)

data_fac$Sex <- factor(data_fac$Sex, levels = c("Man", "Woman", "Prefer not to answer"), 
                       labels = c("Man", "Woman", "Prefernottoanswer"))

data_fac$HHI2020 <- factor(data_fac$HHI2020, levels = c('$25k or less', '$25,001 to $35k',
                                                        '$35,001 to $50k', '$50,001 to $75k',
                                                        '$75,001 to $100k', '$100,001 to $150k',
                                                        '$150,000 to $250k', 'More than $250k',
                                                        'Prefer not to answer'), labels = c("25korless",
                                                                                       "25001to35k",
                                                                                       "35001to50k",
                                                                                       "50001to75k",
                                                                                       "75001to100k",
                                                                                       "100001to150k",
                                                                                       "150001to250k",
                                                                                       "Morethan250k",
                                                                                       "Prefernottoanswer"))

data_fac$Education <- factor(data_fac$Education, levels = c("High school or less", "Some college or 2 year degree",
                                                                     "Bachelor's degree", "Master's degree",
                                                                     "Law or MD", "Doctorate (PhD)",
                                                                     "Prefer not to answer"), labels = c("Highschoolorless",
                                                                                       "Somecollegeor2yeardegree",
                                                                                       "Bachelorsdegree",
                                                                                       "Mastersdegree",
                                                                                       "LaworMD",
                                                                                       "DoctoratePhD",
                                                                                       "Prefernottoanswer"))

data_fac$Politics <- factor(data_fac$Politics, levels = c("Very liberal", "Somewhat liberal",
                                                          "Moderate / middle of the road", "Somewhat conservative",
                                                          "Very conservative", "Other",
                                                          "Prefer not to answer"), labels = c("Veryliberal",
                                                                                     "Somewhatliberal",
                                                                                     "Moderatemiddleoftheroad",
                                                                                     "Somewhatconservative",
                                                                                     "Veryconservative",
                                                                                     "Other",
                                                                                     "Prefernottoanswer"))

# do the data look right? checkpoint
data_fac$StateResidence

levels(data_fac$MangMarshFisheries)

data_fac_step <- na.omit(data_fac[, c("MangMarshFisheries", "StudySite", "YearsWaterfront", "RecFishAccess",
                                      "ShoreMang", "ShoreMarsh", "DesShoreMang", "DesShoreMarsh", "BirthYear", 
                                      "Sex", "HHI2020", "Education", "Politics")]) # grabbing relevant columns/predictor variables

# perform the ordinal logistic regression
vglm.coastal <- vglm(MangMarshFisheries ~ StudySite + YearsWaterfront + 
                       RecFishAccess + ShoreMarsh + ShoreMang + DesShoreMarsh + DesShoreMang +  BirthYear + Sex + HHI2020 + Education + 
                       Politics, family = cumulative(parallel = TRUE, reverse = TRUE), data = data_fac_step)

summary(vglm.coastal)

vglm_df <- coef(summary(vglm.coastal)) %>% 
# turning the vglm output into a dataframe to be compatible with dotwhisker package
  data.frame() %>%
  tibble::rownames_to_column("term") %>%
  rename(estimate = Estimate, std.error = Std..Error) %>%
  filter(term != "(Intercept):1") %>%
  filter(term != "(Intercept):2") %>%
  filter(term != "(Intercept):3") %>%
  filter(term != "(Intercept):4")
vglm_df

# adding a column indicating direction of significance for use in the figure
vglm_df$model <- ifelse(vglm_df$Pr...z.. > 0.05, "Not Significant", 
                        ifelse(vglm_df$Pr...z.. < 0.05 & vglm_df$estimate > 0, "Marsh Preference", "Mangrove Preference"))


brackets <- list( # defining brackets of variables for the figure
  c("Study Site", "Panama City Beach", "Corpus Christi"),
  c("Shoreline", "Marsh Present", "Mangrove Desired"),
  c("Fishing Activity", "Less than once per year", "Daily"),
  c("Time", "Birth Year", "Years Waterfront"),
  c("Gender", "Woman", "Prefer not to answer"),
  c("Household Income", "$25,001 to $35k", " Prefer not to answer"),
  c("Education", "Some college or 2 year degree", "  Prefer not to answer"),
  c("Politics", "Somewhat liberal", "   Prefer not to answer")
) # I had trouble with Prefer not to answer and this was my solution

fig <- {
  dwplot(vglm_df,
         dot_args = list(size = 3), # change dot sizes
         model_name = "model", # specifies column name that's used to decide colors
         model_order = c("Mangrove Preference", "Not Significant", 
                         "Marsh Preference"), # specifies legend order
         vline = geom_vline(xintercept = 0, colour = "grey60", 
                            linetype = 2)) %>%  # adds dotted gray line at 0
    relabel_predictors(c( # fixing labelling of all variables
      # the order here is reflected on the graph
      StudySitePanamaCityBeach = "Panama City Beach",
      StudySiteGalveston = "Galveston",
      StudySiteCorpusChristi = "Corpus Christi",
      ShoreMarshPresent = "Marsh Present",
      ShoreMangPresent = "Mangrove Present",
      DesShoreMarshYes = "Marsh Desired",
      DesShoreMangYes = "Mangrove Desired",
      RecFishAccessLessthanonceperyear = "Less than once per year",
      RecFishAccessYearly = "Yearly",
      RecFishAccessMonthly = "Monthly",
      RecFishAccessWeekly = "Weekly",
      RecFishAccessDaily = "Daily",
      HHI202025001to35k = "$25,001 to $35k",
      HHI202035001to50k = "$35,000 to $50k",
      HHI202050001to75k = "$50,001 to $75k",
      HHI202075001to100k = "$75,001 to $100k",
      HHI2020100001to150k = "$100,001 to $150k",
      HHI2020150001to250k = "$150,001 to $250k",
      HHI2020Morethan250k = "More than $250k",
      HHI2020Prefernottoanswer = " Prefer not to answer",
      SexWoman = "Woman",
      SexPrefernottoanswer = "Prefer not to answer",
      EducationSomecollegeor2yeardegree = "Some college or 2 year degree",
      EducationBachelorsdegree = "Bachelor's degree",
      EducationMastersdegree = "Master's degree",
      EducationLaworMD = "Law or MD",
      EducationDoctoratePhD = "Doctorate (PhD)",
      EducationPrefernottoanswer = "  Prefer not to answer",
      PoliticsSomewhatliberal = "Somewhat liberal",
      PoliticsModeratemiddleoftheroad = "Moderate / middle of the road",
      PoliticsSomewhatconservative = "Somewhat conservative",
      PoliticsVeryconservative = "Very conservative", 
      PoliticsOther = "Other",
      PoliticsPrefernottoanswer = "   Prefer not to answer",
      BirthYear = "Birth Year",
      YearsWaterfront = "Years Waterfront")) + # now you have a ggplot object
    xlab("Coefficient Estimate") + ylab("") +
    ggtitle("Results of Ordinal Logistic Regression") +
    theme_classic() + 
    theme(plot.title = element_text(face="bold", hjust = 0.5),
          legend.position = "bottom",
          legend.background = element_rect(colour="grey80"),
          legend.title.align = .5) +
    labs(color='Perceived Habitat Performance') + # legend title
    scale_colour_viridis_d( # assigning viridis color palette
      alpha = 1,
      begin = 0,
      end = 1,
      direction = -1,
      option = "D",
      aesthetics = "colour"
    )
} %>%
  add_brackets(brackets, fontSize = 0.7) # adds brackets to the figure
fig

# other resources

# personalization
# https://www.biostars.org/p/436837/
# https://cran.r-project.org/web/packages/dotwhisker/vignettes/kl2007_examples.html
# https://indrajeetpatil.github.io/ggstatsplot/reference/ggcoefstats.html