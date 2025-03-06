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
                 readxl,
                 sysfonts,
                 showtext,
                 quiet = TRUE,
                 update_all = FALSE)



# Setting the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



#_______________________________________________________________________________
# 1. DATA IMPORT CLEANING AND FORMATTING
#_______________________________________________________________________________


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

survey_data$height_part <- as.numeric(survey_data$height_part)




#_______________________________________________________________________________
# 2. AHP ANALYSIS AND WEIGHT AGGREGATION
#_______________________________________________________________________________

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

# Creating pairwise comparison matrices from the raw survey dataset. 
AHP_data_mat <- ahp.mat(df = survey_data, atts, negconvert = TRUE) #This function also converts negative values to positive values  and converts negative to reciprocal values for the pairwise comparison matrices.


# Display the first row of the pairwise comparison matrices
AHP_data_mat %>% head(1) %>% kable()


# Compute the consistency ratio of the decision-makers
ahp.cr(AHP_data_mat, atts, ri = NULL) %>% kable()



# Compute the individual weights of the decision-makers, aggregated as geometric mean
ind_weight <- ahp.indpref(AHP_data_mat, atts, method = "eigen")%>%
  round(3) %>% 
  rownames_to_column('ID') %>%
  as_tibble()
kable(ind_weight)

# compute the aggregated priorities of all decision-makers using the specified methods
agg_weight <- ahp.aggpref(AHP_data_mat, atts, method = "eigen", aggmethod =  "geometric")%>%
  round(3) %>% 
  t()%>%
  as_tibble()
kable(agg_weight)


#_______________________________________________________________________________
# 3. DECISION MATRIX IMPORT AND NORMALIZATION
#_______________________________________________________________________________

# Importing the decision matrix file
decision_mat <- read_csv("species_dataset_data_complete.csv")


# Filter the decision matrix to remove trees (height>=4)
decision_mat <- decision_mat %>%
  filter(height < 4)



# Convert raw data to numeric 5-point scale

## Define the binning function for continuous variables with direction control
bin_continuous <- function(value, bins, reverse = TRUE) {
  if (reverse) {
    # Smaller values get higher scores (5,4,3,2,1)
    cut(value, breaks = bins, labels = 5:1, include.lowest = TRUE)
  } else {
    # Larger values get higher scores (1,2,3,4,5)
    cut(value, breaks = bins, labels = 1:5, include.lowest = TRUE)
  }
}

## Define categorical mapping function
map_categorical <- function(value, mapping) {
  unname(sapply(value, function(x) mapping[x]))
}

## Define the binning keys for continuous variables
bin_keys <- list(
  height = c(0, 0.30, 0.5, 0.75, 1, Inf), # Height in meters
  duration = c(0, 60, 120, 180, 240, Inf), # Duration in days
  trials = c(0, 20, 50, 100, 200, Inf), # Number of clinical trials
  activities = c(0, 75, 200, 400, 700, Inf), # Number of medicinal activities
  products = c(0, 5, 10, 20, 40, Inf) # Number of commercial products
)

# Define which attributes should have reversed scoring (smaller is better)
reverse_scoring <- c("height", "duration")  # For these, smaller values get higher scores
# Attributes not in this list will have normal scoring (larger is better)

# Define mapping for categorical variables
cat_mappings <- list(
part = c(
  "Leaves" = 5,
  "Aerial Parts" = 5, 
  "Whole Plant" = 5,
  "Stems" = 5,
  "Fruit" = 4,
  "Flowers" = 3,
  "Seeds" = 2,
  "Roots" = 1,
  "Rhizome" = 1,
  "Tuber" = 1,
  "Bulb" = 1
),  
population = c(
    "EN" = 5, # Endangered
    "CR" = 5, # Critically Endangered
    "VU" = 4, # Vulnerable
    "NT" = 3, # Near Threatened
    "LC" = 1, # Least Concern
    "DD" = 1 # Data Deficient
  )
)

# Enhanced function to handle both continuous and categorical variables with different directions
convert_to_ranking <- function(data, bin_keys, cat_mappings, reverse_scoring) {
  ranked_data <- data
  
# Process continuous variables
for (col in names(bin_keys)) {
  if (col %in% names(data)) {
    # Check if this attribute has reverse scoring
    is_reverse <- col %in% reverse_scoring
    # Add some debugging to verify
    print(paste("Column:", col, "- Reverse scoring:", is_reverse))
    # Make sure to convert to numeric
    ranked_data[[paste0(col, "_rank")]] <- as.numeric(as.character(bin_continuous(data[[col]], bin_keys[[col]], reverse = is_reverse)))
  }
}

  # Process categorical variables
  for (col in names(cat_mappings)) {
    if (col %in% names(data)) {
      ranked_data[[paste0(col, "_rank")]] <- map_categorical(data[[col]], cat_mappings[[col]])
    }
  }
  
  return(ranked_data)
}

# Convert the column values to rankings
ranked_decision_mat <- 
convert_to_ranking(decision_mat, bin_keys, cat_mappings, reverse_scoring) %>% 
  select(c(3,4, 14:20))








#_______________________________________________________________________________
# 4. WEIGHTED SUM ANALYSIS
#_______________________________________________________________________________

#Add the weights to the decision matrix

# Create a weights dataframe with the same structure as ranked_decision_mat
weights_df <- data.frame(matrix(NA, nrow = 1, ncol = ncol(ranked_decision_mat)))
colnames(weights_df) <- colnames(ranked_decision_mat)
weights_df[[1]] <- "Weights" # Set "Weights" label in the first column (species name)



# Map the weights from agg_weight to the corresponding columns in ranked_decision_mat
for (col_name in names(ranked_decision_mat)) {
  # Check if it's a rank column
  if (grepl("_rank$", col_name)) {
    # Extract the base attribute name by removing "_rank" suffix
    base_name <- gsub("_rank$", "", col_name)
    
    # If the base attribute exists in agg_weight, add its value
    if (base_name %in% names(agg_weight)) {
      weights_df[[col_name]] <- as.numeric(agg_weight[[base_name]])
    }
  }
}

# Combine the weights row with the original data
weighted_decision_mat <- rbind(weights_df, ranked_decision_mat)








# Calculate the weighted values by multiplying each value by its corresponding weight
weighted_values_mat <- weighted_decision_mat %>%
  slice(-1) %>%
  mutate(species_name = .[[1]], species_family = .[[2]]) %>%
  select(ends_with("_rank")) %>%
  mutate(across(everything(), ~as.numeric(.x) * as.numeric(weighted_decision_mat[1, cur_column()]))) %>%
  bind_cols(weighted_decision_mat[-1, 1:2]) %>%
  select(1:2, everything())




# Calculate weighted sum for each species
species_scores <- weighted_values_mat %>%
  rowwise() %>%
  mutate(total_score = sum(c_across(ends_with("_rank")), na.rm = TRUE)) %>%
  ungroup()%>%
  mutate(perc_score = (total_score/max(total_score))*100) %>%
  mutate(plant_score = ((height_rank+duration_rank+part_rank)/max(total_score))*100)%>%
  mutate(medicinal_score = ((trials_rank+activities_rank)/max(total_score))*100)%>%
  mutate(see_score = ((products_rank+population_rank)/max(total_score))*100) %>%
  select(c(8:14))


write_csv(species_scores, "species_scores.csv")






#_______________________________________________________________________________
# 5. DATA VISUALIZATION AND SUMMARY
#_______________________________________________________________________________

# Processing data for visualization
# Convert to long format
species_scores_long <- species_scores %>%
  pivot_longer(
    cols = c(plant_score, medicinal_score, see_score),
    names_to = "score_category",
    values_to = "score"
  )

# Setting up my theme parameters
fig_theme <-   theme(
  text = element_text(family = "Source sans pro", face = "plain"),
  axis.ticks = element_line(linewidth = 0.2),
  axis.line = element_line(linewidth = 0.2),
  axis.text = element_text(size=10))








# Figure: Weighted sum scores
fig_weighted_sum<-
  species_scores_long %>%
  arrange(desc(perc_score)) %>%
  slice(c(1:60))%>%
  ggplot(aes(reorder(plant_name, score, sum), y = score, fill = score_category)) +
  geom_col() +
  coord_flip() +
  labs(x = "Species", y = "Weighted sum score", fill = "Score category") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr() +
  fig_theme
fig_weighted_sum












# Raw data summary
## Create a boxplot of the raw data for each criteria. Create histograms for categorical data:














#---------------------------------------------------------
# Individual weights

## create a stacked bar plot of the aggregate weights
fig_weight_collage <- 
  agg_weight %>%
  pivot_longer(cols = everything(), names_to = "atts", values_to = "value") %>%
    mutate(scores = 1)%>%
  mutate(atts = dict[atts])%>%                             #change the attribute codes to the actual names
  ggplot(aes(x = scores, y = value, fill = atts))+
  geom_col() +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(x = "", y = "Weights", fill = "Criteria")+
  theme_pubr()+
  fig_theme+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank())



fig_weight_collage
## Create a heatmap of the individual weights from all decision-makers.

fig_ind_weight <-
  ind_weight %>%
  pivot_longer(cols = -ID, names_to = "atts", values_to = "value") %>% # convert to long format
  mutate(atts = dict[atts])%>%
  ggplot(aes(x = atts, y = ID, fill = value))+
  geom_raster()+
  scale_fill_gradient(low="white", high="royalblue4")+
  labs(x = "Attributes", y = "Decision-makers", fill = "Weights")+
  theme_bw()+
  theme(axis.text=element_text(size=12))
fig_ind_weight





# Aggregated weights
fig_agg_weight <- 
  agg_weight %>%
  pivot_longer(cols = everything(), names_to = "atts", values_to = "value")%>% # convert to long format
  mutate(atts = dict[atts])%>%                             #change the attribute codes to the actual names
  mutate(ID = 1)%>%                                        #create a dummy ID column
  ggplot(aes(x = atts, y = ID, fill = value))+
  geom_raster()+
  scale_fill_gradient(low="white", high="royalblue4")+
  labs(x = "Attributes", y = "Decision-makers", fill = "Weights")+
  theme_bw()+
  theme(axis.text=element_text(size=12))
fig_agg_weight

# Weights collage
fig_weight_collage 




#---------------------------------------------------------
# Species ranking











### END OF SCRIPT ###