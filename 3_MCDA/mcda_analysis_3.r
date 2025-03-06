# MCDA Analysis for Crop Selection

# Setup ----------------------------------------------------------------------
library(tidyverse)
library(ahpsurvey)
library(knitr)
library(readxl)
library(rstudioapi)

# Set working directory to script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1. Data Import & Formatting ------------------------------------------------
# Import and clean survey data
survey_data <- read_csv("raw_survey_data.csv") %>%
  select(c(18:38)) %>%
  slice(-2,-1) %>%
  dplyr::rename(
    height_part = 1, height_duration = 2, height_trials = 3,
    height_activities = 4, height_products = 5, height_population = 6,
    part_duration = 7, part_trials = 8, part_activities = 9,
    part_products = 10, part_population = 11, duration_trials = 12,
    duration_activities = 13, duration_products = 14, duration_population = 15,
    trials_activities = 16, trials_products = 17, trials_population = 18,
    activities_products = 19, activities_population = 20, products_population = 21
  ) %>%
  mutate(across(everything(), as.numeric))

# 2. AHP Analysis & Weight Aggregation ---------------------------------------
# Define attributes and descriptive dictionary
atts <- c("height", "part", "duration", "trials", "activities", "products", "population")
dict <- c(
  "height" = "Plant height", "part" = "Part of use",
  "duration" = "Duration to harvest", "trials" = "No. of clinical trials",
  "activities" = "Medicinal activities", "products" = "Number of commercial products",
  "population" = "Population status"
)

# Create pairwise comparison matrices and analyze
AHP_data_mat <- ahp.mat(df = survey_data, atts, negconvert = TRUE)

# Calculate individual weights
ind_weight <- ahp.indpref(AHP_data_mat, atts, method = "eigen") %>%
  round(3) %>% 
  rownames_to_column('ID') %>%
  as_tibble()

# Calculate aggregated weights
agg_weight <- ahp.aggpref(AHP_data_mat, atts, method = "eigen", aggmethod = "geometric") %>%
  round(3) %>% 
  t() %>%
  as_tibble()

agg_weight%>%kable()




# 3. Decision Matrix Processing ----------------------------------------------
# Import decision matrix and filter out trees
decision_mat <- read_csv("species_dataset_data_complete.csv") %>%
  filter(height < 4) # Filter out trees with height > 4 meters

# Define scoring functions
bin_continuous <- function(value, bins, reverse = TRUE) {
  if (reverse) {
    # Smaller values get higher scores (5,4,3,2,1)
    cut(value, breaks = bins, labels = 5:1, include.lowest = TRUE)
  } else {
    # Larger values get higher scores (1,2,3,4,5)
    cut(value, breaks = bins, labels = 1:5, include.lowest = TRUE)
  }
}

map_categorical <- function(value, mapping) {
  unname(sapply(value, function(x) mapping[x]))
}

# Define binning keys and mappings
bin_keys <- list(
  height = c(0, 0.30, 0.5, 0.75, 1, Inf),          # Height in meters
  duration = c(0, 60, 120, 180, 240, Inf),         # Duration in days
  trials = c(0, 20, 50, 100, 200, Inf),            # Number of clinical trials
  activities = c(0, 75, 200, 400, 700, Inf),       # Number of medicinal activities
  products = c(0, 5, 10, 20, 40, Inf)              # Number of commercial products
)

reverse_scoring <- c("height", "duration")  # Smaller values get higher scores

cat_mappings <- list(
  part = c(
    "Leaves" = 5, "Aerial Parts" = 5, "Whole Plant" = 5, "Stems" = 5,
    "Fruit" = 4, 
    "Flowers" = 3, 
    "Seeds" = 2, 
    "Roots" = 1, "Rhizome" = 1, "Tuber" = 1, "Bulb" = 1
  ),  
  population = c(
    "EN" = 5, "CR" = 5, "VU" = 4, "NT" = 3, "LC" = 1, "DD" = 1
  )
)

# Function to convert data to rankings
convert_to_ranking <- function(data, bin_keys, cat_mappings, reverse_scoring) {
  ranked_data <- data
  
  # Process continuous variables
  for (col in names(bin_keys)) {
    if (col %in% names(data)) {
      is_reverse <- col %in% reverse_scoring
      ranked_data[[paste0(col, "_rank")]] <- 
        as.numeric(as.character(bin_continuous(data[[col]], bin_keys[[col]], reverse = is_reverse)))
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

# Apply ranking conversion
ranked_decision_mat <- convert_to_ranking(decision_mat, bin_keys, cat_mappings, reverse_scoring) %>% 
  select(c(3, 4, 14:20))

# 4. Weighted Sum Analysis --------------------------------------------------
# Create weights dataframe
weights_df <- data.frame(matrix(NA, nrow = 1, ncol = ncol(ranked_decision_mat)))
colnames(weights_df) <- colnames(ranked_decision_mat)
weights_df[[1]] <- "Weights"

# Map weights from AHP to ranked decision matrix
for (col_name in names(ranked_decision_mat)) {
  if (grepl("_rank$", col_name)) {
    base_name <- gsub("_rank$", "", col_name)
    if (base_name %in% names(agg_weight)) {
      weights_df[[col_name]] <- as.numeric(agg_weight[[base_name]])
    }
  }
}

# Combine weights with decision matrix
weighted_decision_mat <- rbind(weights_df, ranked_decision_mat)

# Calculate weighted values
weighted_values_mat <- weighted_decision_mat %>%
  slice(-1) %>%
  mutate(species_name = .[[1]], species_family = .[[2]]) %>%
  select(ends_with("_rank")) %>%
  mutate(across(everything(), ~as.numeric(.x) * as.numeric(weighted_decision_mat[1, cur_column()]))) %>%
  bind_cols(weighted_decision_mat[-1, 1:2]) %>%
  select(1:2, everything())

# Calculate final scores
species_scores <- weighted_values_mat %>%
  rowwise() %>%
  mutate(total_score = sum(c_across(ends_with("_rank")), na.rm = TRUE)) %>%
  ungroup() %>%
  select(c(7:9)) %>%
  mutate(perc_score = (total_score/max(total_score))*100)

# 5. Data Visualization -----------------------------------------------------
# Individual weights visualization
fig_ind_weight <- ind_weight %>%
  pivot_longer(cols = -ID, names_to = "atts", values_to = "value") %>%
  mutate(atts = dict[atts]) %>%
  ggplot(aes(x = atts, y = ID, fill = value)) +
  geom_raster() +
  scale_fill_gradient(low="white", high="royalblue4") +
  labs(x = "Attributes", y = "Decision-makers", fill = "Weights") +
  theme_bw() +
  theme(axis.text = element_text(size = 12))

# Aggregated weights visualization
fig_agg_weight <- agg_weight %>%
  pivot_longer(cols = everything(), names_to = "atts", values_to = "value") %>%
  mutate(atts = dict[atts], ID = 1) %>%
  ggplot(aes(x = atts, y = ID, fill = value)) +
  geom_raster() +
  scale_fill_gradient(low="white", high="royalblue4") +
  labs(x = "Attributes", y = "Decision-makers", fill = "Weights") +
  theme_bw() +
  theme(axis.text = element_text(size = 12))