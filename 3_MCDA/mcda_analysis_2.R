# This is the script that will be used for the MCDA procedures

# Installing relevant packages
install.packages("librarian")
librarian::shelf(tidyverse,
                 dplyr,
                 ggpubr,
                 rstudioapi,
                 readr,
                 devtools,
                 ahpsurvey,
                 magrittr,
                 knitr,
                 update_all = FALSE)



# Setting the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))




# DATA IMPORT CLEANING AND FORMATTING

# Importing raw data file
survey_data <- read_csv("raw_survey_data.csv")%>%
  select(c(18:38))%>%
  slice(-2,-1)%>% # if you want to see the question, remove '-1' in ht slice function. 
  dplyr::rename(height_part = 1, 
                height_duration = 2,
                height_trials = 3,
                height_activities = 4,
                height_products = 5,
                height_population = 6,
                part_duration = 7,
                part_trials = 8,
                part_activities = 9,
                part_products = 10,
                part_population = 11,
                duration_trials = 12,
                duration_activities = 13,
                duration_products = 14,
                duration_population = 15,
                trials_activities = 16,
                trials_products = 17,
                trials_population = 18,
                activities_products = 19,
                activities_population = 20,
                products_population = 21
                )%>%
  mutate(across(everything(), as.numeric))


head(survey_data)






## Define the attribute used
# Loading the data
# The attributes to be compared are the following:
atts <- c("height", "part", "duration", "trials", "activities", "products", "population")
dict <- c("height"     = "Plant height",
          "part"       = "Part of use",
          "duration"   = "Duration to harvest",
          "trials"     = "No. of clinical trials",
          "activities" = "Medicinal activities",
          "products"   = "Number of commercial products",
          "population" = "Population status")

# Creating pairwise comparison matrices
AHP_data_mat <- ahp.mat(df = survey_data, atts, negconvert = TRUE)
AHP_data_mat %>% head(1) %>% kable()


# compute the individual priorities of the decision-makers, aggregated as geometric mean
geom_ind <- ahp.indpref(AHP_data_mat, atts, method = "geometric")
round(geom_ind, 3) %>% 
  rownames_to_column('ID') %>% 
  kable()



# compute the aggregated priorities of all decision-makers using the specified methods
geom_agg <- ahp.aggpref(AHP_data_mat, atts, method = "arithmetic", aggmethod = "geometric")
round(geom_agg, 3) %>% t() %>% kable()