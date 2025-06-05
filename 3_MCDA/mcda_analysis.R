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
                 ggsci,
                 ggrain,
                 tidyplots,
                 cowplot,
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
  mutate(across(everything(), as.numeric))%>%
  as.data.frame()


head(survey_data)
str(survey_data)

survey_data$height_part <- as.numeric(survey_data$height_part)




#_______________________________________________________________________________
# 2. AHP ANALYSIS AND WEIGHT AGGREGATION
#_______________________________________________________________________________

## Define the attribute used
# Loading the data
# The attributes to be compared are the following:
atts <- c("height", "part", "duration", "trials", "activities", "products", "population")
dict <- c("height"     = "Plant height",
          "part"       = "Medicinal tissue",
          "duration"   = "Duration to harvest",
          "trials"     = "Clinical trials",
          "activities" = "Medicinal activities",
          "products"   = "Commercial products",
          "population" = "Population status")

# Creating pairwise comparison matrices from the raw survey dataset.
#select only first row of the survey data
AHP_data_mat <- ahp.mat(df = survey_data, atts, negconvert = TRUE) #This function also converts negative values to positive values  and converts negative to reciprocal values for the pairwise comparison matrices.



# Display the first pairwise comparison matrix
AHP_data_mat %>% head(1) %>% kable()


# Compute the consistency ratio of the decision-makers; ri is from Saaty Table
ahp.cr(AHP_data_mat, atts, ri = 1.32) %>% kable()



# Compute the weights of the individual decision-makers
ind_weight <- ahp.indpref(AHP_data_mat, atts, method = "eigen")%>%
  round(3) %>% 
  rownames_to_column('ID') %>%
  as_tibble()
kable(ind_weight)

# compute the aggregated priorities of all decision-makers using the specified methods, aggregated as geometric means
agg_weight <- ahp.aggpref(AHP_data_mat, atts, method = "eigen", aggmethod =  "geometric")%>%
  round(3) %>% 
  t()%>%
  as_tibble()
kable(agg_weight)


rowSums(agg_weight)

#Normalise the weights such that the sum equals to 1
agg_weight_norm <- agg_weight/sum(agg_weight)
agg_weight_norm
rowSums(agg_weight_norm)

# Domain weights
# calculate sum of height part and duration as "plant weight", trials and activites as "medicinal weight", and products and population as "SEE weight"
agg_weight_norm_domain <- agg_weight_norm %>%
  mutate(plant_weight = height + part + duration,
         medicinal_weight = trials + activities,
         see_weight = products + population) %>%
  select(-c(height, part, duration, trials, activities, products, population))
agg_weight_norm_domain


# Normalise individual weights of each ID across rows from column 2 to 8, to 1
ind_weight_norm <- ind_weight %>%
  rowwise() %>%
  mutate(across(2:8, ~ .x / sum(c_across(2:8)))) %>%
  ungroup()

# calculate the min and max range of the weughts for each criteria 
ind_weight_range <- ind_weight_norm %>%
  summarise(across(2:8, list(min = min, max = max))) %>%
  pivot_longer(cols = everything(), names_to = c("atts", ".value"), names_sep = "_") %>%
  mutate(atts = dict[atts])%>%                             #change the attribute codes to the actual names
  rename(min = min, max = max)
ind_weight_range

#_______________________________________________________________________________
# 3. DECISION MATRIX IMPORT AND NORMALIZATION
#_______________________________________________________________________________

# Importing the decision matrix file
decision_mat <- read_csv("species_dataset_data_complete.csv")


# Filter the decision matrix to remove trees (height>=4)
decision_mat <- decision_mat %>%
  filter(height < 4)


#combining the clinical trials from us and from who into one column
decision_mat <- decision_mat %>%
  mutate(trials = trials_us + trials_who) %>%
  select(-c(trials_us, trials_who)) # Remove the original columns



# Convert raw data to numeric 5-point scale

## Define the binning function for continuous variables with direction control
bin_continuous <- function(value, bins, reverse = TRUE) {
  if (reverse) {
    # Smaller values get higher scores (5,4,3,2,1)
    cut(value, breaks = bins, labels = 5:1, include.lowest = TRUE)
  } else {
    # Larger values get higher scores (1:5)
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
  products = c(0, 50, 250, 750, 1500, Inf) # Number of commercial products
)


# Define which attributes should have reversed scoring (smaller is better)
reverse_scoring <- c("height", "duration")  # For these, smaller values get higher scores
# Attributes not in this list will have normal scoring (larger is better)

# Define mapping for categorical variables
cat_mappings <- list(
part = c(
  "Leaves" = 5,
  "Aerial Parts" = 5,
  "Stems" = 5, 
  "Whole Plant" = 4,
  "Flowers" = 3,
  "Fruit" = 3,
  "Seeds" = 2,
  "Rhizome" = 1,
  "Tuber" = 1,
  "Bulb" = 1,
  "Roots" = 1
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



# Map the weights from agg_weight_norm to the corresponding columns in ranked_decision_mat
for (col_name in names(ranked_decision_mat)) {
  # Check if it's a rank column
  if (grepl("_rank$", col_name)) {
    # Extract the base attribute name by removing "_rank" suffix
    base_name <- gsub("_rank$", "", col_name)
    
    # If the base attribute exists in agg_weight_norm, add its value
    if (base_name %in% names(agg_weight_norm)) {
      weights_df[[col_name]] <- as.numeric(agg_weight_norm[[base_name]])
    }
  }
}

# Combine the weights row with the original data
weighted_decision_mat <- rbind(weights_df, ranked_decision_mat)








# Calculate the weighted values by multiplying each value by its corresponding weight
weighted_values_mat <- weighted_decision_mat %>%
  slice(-1) %>%
  mutate(species_name = .[[1]]) %>%
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
# 5. SENSITIVITY ANALYSIS
#_______________________________________________________________________________

# Function to perform sensitivity analysis for a given criterion and weight change percentage


sensitivity_analysis <- function(criterion, weight_percentage, weights_data = agg_weight_norm, species_data = weighted_values_mat) {
  # Validate that criterion exists in weights
  if (!criterion %in% names(weights_data)) {
    stop(paste("Criterion", criterion, "not found in weights data"))
  }
  
  # Calculate the actual weight change based on percentage
  original_weight <- weights_data[[criterion]]
  weight_change <- original_weight * (weight_percentage/100)
  
  # Create results containers
  plus_result <- minus_result <- list()
  
  # INCREASED WEIGHT ANALYSIS
  # Make a copy of the original weights
  plus_weights <- weights_data
  # Adjust weight up by percentage
  plus_weights[[criterion]] <- plus_weights[[criterion]] + weight_change
  # Normalize weights to sum to 1
  plus_weights <- plus_weights / sum(plus_weights)
  
  # Create a copy of the original data for plus analysis
  plus_data <- species_data
  # Recalculate weighted values using increased weights
  for (col in names(plus_data)) {
    if (grepl("_rank$", col)) {
      base_name <- gsub("_rank$", "", col)
      if (base_name %in% names(plus_weights)) {
        plus_data[[col]] <- as.numeric(plus_data[[col]]) * as.numeric(plus_weights[[base_name]])
      }
    }
  }
  
  # Calculate new total scores for increased weights
  plus_result <- plus_data %>%
    rowwise() %>%
    mutate(total_score_plus = sum(c_across(ends_with("_rank")), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(perc_score_plus = (total_score_plus/max(total_score_plus))*100) %>%
    arrange(desc(total_score_plus))
  # Update rankings
  plus_result$rank_plus <- rank(-plus_result$total_score_plus)
  
  # DECREASED WEIGHT ANALYSIS
  # Make a copy of the original weights
  minus_weights <- weights_data
  # Adjust weight down by percentage
  minus_weights[[criterion]] <- minus_weights[[criterion]] - weight_change
  # Ensure weights don't go negative
  if (minus_weights[[criterion]] < 0) {
    warning("Weight adjustment resulted in negative value, setting to 0")
    minus_weights[[criterion]] <- 0
  }
  # Normalize weights to sum to 1
  minus_weights <- minus_weights / sum(minus_weights)
  
  # Create a copy of the original data for minus analysis
  minus_data <- species_data
  # Recalculate weighted values using decreased weights
  for (col in names(minus_data)) {
    if (grepl("_rank$", col)) {
      base_name <- gsub("_rank$", "", col)
      if (base_name %in% names(minus_weights)) {
        minus_data[[col]] <- as.numeric(minus_data[[col]]) * as.numeric(minus_weights[[base_name]])
      }
    }
  }
  
  # Calculate new total scores for decreased weights
  minus_result <- minus_data %>%
    rowwise() %>%
    mutate(total_score_minus = sum(c_across(ends_with("_rank")), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(perc_score_minus = (total_score_minus/max(total_score_minus))*100) %>%
    arrange(desc(total_score_minus))
  # Update rankings
  minus_result$rank_minus <- rank(-minus_result$total_score_minus)
  
  # ORIGINAL ANALYSIS (for comparison)
  original_result <- species_data %>%
    rowwise() %>%
    mutate(total_score = sum(c_across(ends_with("_rank")), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(perc_score = (total_score/max(total_score))*100) %>%
    arrange(desc(total_score))
  original_result$rank <- rank(-original_result$total_score)
  
  # Combine results
  combined_results <- original_result %>%
    select(plant_name, total_score, perc_score, rank) %>%
    left_join(
      plus_result %>% select(plant_name, total_score_plus, perc_score_plus, rank_plus),
      by = "plant_name"
    ) %>%
    left_join(
      minus_result %>% select(plant_name, total_score_minus, perc_score_minus, rank_minus),
      by = "plant_name"
    ) %>%
    mutate(
      rank_change_plus = rank - rank_plus,
      rank_change_minus = rank - rank_minus
    )
  
  return(combined_results)
}



























sensitivity_analysis_2 <- function(criterion, weight_percentage, weights_data = agg_weight_norm, species_data = weighted_values_mat, 
                                   decision_mat = ranked_decision_mat) {
  # Validate that criterion exists in weights
  if (!criterion %in% names(weights_data)) {
    stop(paste("Criterion", criterion, "not found in weights data"))
  }
  
  # Calculate the actual weight change based on percentage
  original_weight <- weights_data[[criterion]]
  weight_change <- original_weight * (weight_percentage/100)
  
  # Create results containers
  plus_result <- minus_result <- list()
  
  # INCREASED WEIGHT ANALYSIS
  # Make a copy of the original weights
  plus_weights <- weights_data
  # Adjust weight up by percentage
  plus_weights[[criterion]] <- plus_weights[[criterion]] + weight_change
  # Normalize weights to sum to 1
  plus_weights <- plus_weights / sum(plus_weights)
  
  # Create a plus weights dataframe with the same structure as ranked_decision_mat
  weights_df_plus <- data.frame(matrix(NA, nrow = 1, ncol = ncol(decision_mat)))
  colnames(weights_df_plus) <- colnames(decision_mat)
  weights_df_plus[[1]] <- "Weights" # Set "Weights" label in the first column (species name)

  # Map the weights from agg_weight_norm to the corresponding columns in decision_mat
  for (col_name in names(decision_mat)) {
  # Check if it's a rank column
  if (grepl("_rank$", col_name)) {
    # Extract the base attribute name by removing "_rank" suffix
    base_name <- gsub("_rank$", "", col_name)
    
    # If the base attribute exists in plus_weights, add its value
    if (base_name %in% names(plus_weights)) {
      weights_df_plus[[col_name]] <- as.numeric(plus_weights[[base_name]])
    }
  }
  }

  # Combine the weights row with the original data
  weighted_decision_mat_plus <- rbind(weights_df_plus, decision_mat)

  # Create a copy of the original data for plus analysis
  plus_data <- weighted_decision_mat_plus %>%
    slice(-1) %>%
    mutate(species_name = .[[1]]) %>%
    select(ends_with("_rank")) %>%
    mutate(across(everything(), ~as.numeric(.x) * as.numeric(weighted_decision_mat_plus[1, cur_column()]))) %>%
    bind_cols(weighted_decision_mat_plus[-1, 1:2]) %>%
    select(1:2, everything())
  
  # Calculate new total scores for increased weights
  plus_result <- plus_data %>%
    rowwise() %>%
    mutate(total_score_plus = sum(c_across(ends_with("_rank")), na.rm = TRUE)) %>%
    ungroup() %>%
    # mutate(perc_score_plus = (total_score_plus/max(total_score_plus))*100) %>%
    arrange(desc(total_score_plus))
  # Update rankings
  plus_result$rank_plus <- rank(-plus_result$total_score_plus)
  
  # DECREASED WEIGHT ANALYSIS
  # Make a copy of the original weights
  minus_weights <- weights_data
  # Adjust weight down by percentage
  minus_weights[[criterion]] <- minus_weights[[criterion]] - weight_change
  # Ensure weights don't go negative
  if (minus_weights[[criterion]] < 0) {
    warning("Weight adjustment resulted in negative value, setting to 0")
    minus_weights[[criterion]] <- 0
  }
  # Normalize weights to sum to 1
  minus_weights <- minus_weights / sum(minus_weights)
  
  # Create a plus weights dataframe with the same structure as ranked_decision_mat
  weights_df_minus <- data.frame(matrix(NA, nrow = 1, ncol = ncol(decision_mat)))
  colnames(weights_df_minus) <- colnames(decision_mat)
  weights_df_minus[[1]] <- "Weights" # Set "Weights" label in the first column (species name)

  # Map the weights from weights_df_minus to the corresponding columns in decision_mat
  for (col_name in names(decision_mat)) {
  # Check if it's a rank column
  if (grepl("_rank$", col_name)) {
    # Extract the base attribute name by removing "_rank" suffix
    base_name <- gsub("_rank$", "", col_name)
    
    # If the base attribute exists in minus_weights, add its value
    if (base_name %in% names(minus_weights)) {
      weights_df_minus[[col_name]] <- as.numeric(minus_weights[[base_name]])
    }
  }
  }

  # Combine the weights row with the original data
  weighted_decision_mat_minus <- rbind(weights_df_minus, decision_mat)

  # Create a copy of the original data for minus analysis
  minus_data <- weighted_decision_mat_minus %>%
    slice(-1) %>%
    mutate(species_name = .[[1]]) %>%
    select(ends_with("_rank")) %>%
    mutate(across(everything(), ~as.numeric(.x) * as.numeric(weighted_decision_mat_minus[1, cur_column()]))) %>%
    bind_cols(weighted_decision_mat_minus[-1, 1:2]) %>%
    select(1:2, everything())

  
  # Calculate new total scores for decreased weights
  minus_result <- minus_data %>%
    rowwise() %>%
    mutate(total_score_minus = sum(c_across(ends_with("_rank")), na.rm = TRUE)) %>%
    ungroup() %>%
    # mutate(perc_score_minus = (total_score_minus/max(total_score_minus))*100) %>%
    arrange(desc(total_score_minus))
  # Update rankings
  minus_result$rank_minus <- rank(-minus_result$total_score_minus)
  
  # ORIGINAL ANALYSIS (for comparison)
  original_result <- species_data %>%
    rowwise() %>%
    mutate(total_score = sum(c_across(ends_with("_rank")), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(perc_score = (total_score/max(total_score))*100) %>%
    arrange(desc(total_score))
  original_result$rank <- rank(-original_result$total_score)
  
  # Combine results
  combined_results <- original_result %>%
    select(plant_name, total_score, perc_score, rank) %>%
    left_join(
      plus_result %>% select(plant_name, total_score_plus, rank_plus),
      by = "plant_name"
    ) %>%
    left_join(
      minus_result %>% select(plant_name, total_score_minus, rank_minus),
      by = "plant_name"
    ) %>%
    mutate(
      perc_score_minus = (total_score_minus/max(total_score_minus))*100,
      perc_score_plus = (total_score_plus/max(total_score_plus))*100) %>%
    mutate(
      rank_change_plus = rank - rank_plus,
      rank_change_minus = rank - rank_minus
    )%>%
    # add a c0loumn that calculates the total score change in percentage
    mutate(
      !!paste0("total_score_change_", criterion) := (abs(perc_score - perc_score_plus)+
                              abs(perc_score - perc_score_minus))
                              )

  
  return(combined_results)
}

sensitivity_analysis_2("height", 10)



# CONDUCT SENSITIVITY ANALYSIS FOR EACH CRITERION IN A SINGLE LOOP
# This will create separate dataframes for each criterion with sensitivity results

# Define criteria names
criteria_names <- c("height", "part", "duration", "trials", "activities", "products", "population")

# Create a list to store sensitivity results
sensitivity_results <- list()

# Loop through each criterion and perform sensitivity analysis
for (criterion in criteria_names) {
  # Perform sensitivity analysis and assign to separate dataframes
  assign(paste0("sensitivity_", criterion), sensitivity_analysis_2(criterion, 10))
}


# View the results of each sensitivity analysis
View(sensitivity_height)
View(sensitivity_part)
View(sensitivity_duration)
View(sensitivity_trials)
View(sensitivity_activities)
View(sensitivity_products)
View(sensitivity_population)




# Total score change for each species
# bind all sensitivity results sid-byside, matching the plant names
total_score_change <- sensitivity_height %>%
  select(plant_name, total_score_change_height) %>%
  left_join(sensitivity_part %>% select(plant_name, total_score_change_part), by = "plant_name") %>%
  left_join(sensitivity_duration %>% select(plant_name, total_score_change_duration), by = "plant_name") %>%
  left_join(sensitivity_trials %>% select(plant_name, total_score_change_trials), by = "plant_name") %>%
  left_join(sensitivity_activities %>% select(plant_name, total_score_change_activities), by = "plant_name") %>%
  left_join(sensitivity_products %>% select(plant_name, total_score_change_products), by = "plant_name") %>%
  left_join(sensitivity_population %>% select(plant_name, total_score_change_population), by = "plant_name")%>%
  mutate(total_score_change = rowSums(select(., starts_with("total_score_change_")), na.rm = TRUE))









# Calculate the total sum of score changes for each criterion
total_score_change_summary <- total_score_change %>%
  summarise(across(starts_with("total_score_change_"), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "criterion", values_to = "total_change") %>%
  mutate(criterion = gsub("total_score_change_", "", criterion)) %>%
  arrange(desc(total_change))










# Plot the total score change for the top 10 and bottom 10 species separately
# Select top and bottom 10 species based on total score change
most_sensitive_species <- total_score_change %>%
  arrange(desc(total_score_change)) %>%
  slice(1:10)
least_sensitive_species <- total_score_change %>%
  arrange(total_score_change) %>%
  slice(1:10)
# Combine top and bottom species into one dataframe for plotting
sensitivity_plot_data <- bind_rows(
  top_10_species %>% mutate(group = "Most sensitive"),
  bottom_10_species %>% mutate(group = "Least sensitive")
)





#_______________________________________________________________________________
# 6. DATA VISUALIZATION AND SUMMARY
#_______________________________________________________________________________

# Summary Table of raw data
summary_table <- decision_mat %>%
  select(height, duration, trials, activities, products) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    max = max(value, na.rm = TRUE)
  )
summary_table





# Setting some theme elements before plotting

## Visual and font parameters
fig_theme <-   theme(
  text = element_text(family = "Source sans pro", face = "plain"),
  axis.ticks = element_line(linewidth = 0.2),
  axis.line = element_line(linewidth = 0.2),
  axis.text = element_text(size=10),
  plot.margin = margin(5,15,5,5))




## Theme to erase all y axis elements
theme_erase_y <-
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank())








# FIGURE 1: RAW DATA

# Fig 1a: Height

fig_height <- decision_mat %>%
  filter(!is.na(height)) %>% # First, clean the data by removing NA values
  select(plant_name, height)%>%
  ggplot( aes(x = "", y = height)) +
  geom_rain(fill = "royalblue4", alpha = 0.7,
  violin.args = list(alpha = .3, fill = "royalblue4", colour = NA),
  boxplot.args = list(fill = NA, colour = "darkblue", alpha = 0.7, width = 0.25,  outlier.shape = NA),
  point.args = list(size = 1.5, color = "darkblue", alpha = 0.8)) +
  scale_y_continuous(limit = c(0, 3.5),
                    breaks = seq(0, 3.5, by = 0.5),
                     expand = c(0, 0)) +
  labs(x = "",
       y = "Plant height (m)") +
  theme_pubr() +
  fig_theme+
  coord_flip()+
  theme_erase_y+
  theme(plot.margin = margin(5, 15, 35, 15)) 

fig_height


# Fig 1b: Duration
fig_duration <- decision_mat %>%
  filter(!is.na(duration)) %>% # First, clean the data by removing NA values
  select(plant_name, duration)%>%
  ggplot( aes(x = "", y = duration)) +
  geom_rain(fill = "royalblue4", alpha = 0.7,
  violin.args = list(alpha = .3, fill = "royalblue4", colour = NA),
  boxplot.args = list(fill = NA, colour = "darkblue", alpha = 0.7, width = 0.25, outlier.shape = NA),
  point.args = list(size = 1.5, color = "darkblue", alpha = 0.8)
  ) +
  scale_y_continuous(limit = c(0, 2500),
  breaks = seq(0, 2500, by = 500),
    expand = c(0, 0)) +
  labs(
       x = "",
       y = "Duration to harvest (days)") +
  theme_pubr() +
  fig_theme+
  coord_flip()+
  theme_erase_y+
  theme(plot.margin = margin(5, 15, 35, 15)) 


fig_duration


# Fig 1c: Trials
fig_trials <- decision_mat %>%
  filter(!is.na(trials)) %>% # First, clean the data by removing NA values
  filter(trials > 0) %>% # Filter out zeros which would cause problems with log10
  select(plant_name, trials) %>%
  ggplot(aes(x = "", y = trials)) +
  geom_rain(
    fill = "royalblue4", alpha = 0.7,
    violin.args = list(alpha = .3, fill = "royalblue4", colour = NA, scale = "area"),
    boxplot.args = list(fill = NA, colour = "darkblue", alpha = 0.7, width = 0.25, outlier.shape = NA),
    point.args = list(size = 1.5, color = "darkblue", alpha = 0.8)
  ) +
  scale_y_log10(
    breaks = c(1, 10, 100, 1000),
    labels = scales::comma_format(),
    limit = c(1, 1000),
    expand = c(0, 0)
  ) +
  coord_flip() +
  labs(x = "", y = "Trials (#) [log scale]") +
  theme_pubr() +
  fig_theme +
  theme_erase_y +
  theme(plot.margin = margin(5, 15, 35, 15))

fig_trials


# Fig 1d: Activities
fig_activities <- decision_mat %>%
  filter(!is.na(activities)) %>% # First, clean the data by removing NA values
  select(plant_name, activities)%>%
  ggplot( aes(x = "", y = activities)) +
  geom_rain(fill = "royalblue4", alpha = 0.7,
  violin.args = list(alpha = .3, fill = "royalblue4", colour = NA),
  boxplot.args = list(fill = NA, colour = "darkblue", alpha = 0.7, width = 0.25, outlier.shape = NA),
  point.args = list(size = 1.5, color = "darkblue", alpha = 0.8)
  ) +
  # geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
    scale_y_continuous(limit = c(0, 900),
  breaks = seq(0, 900, by = 150),
    expand = c(0, 0)) +
  labs(
       x = "",
       y = "Activities (#)") +
  theme_pubr() +
  fig_theme+
  coord_flip()+
  theme_erase_y+
  theme(plot.margin = margin(5, 15, 35, 15)) 


fig_activities






# Fig 1e: Commerical products
fig_products <- decision_mat %>%
  filter(!is.na(products)) %>% # First, clean the data by removing NA values
  select(plant_name, products)%>%
  ggplot( aes(x = "", y = products)) +
  geom_rain(fill = "royalblue4", alpha = 0.7,
  violin.args = list(alpha = .3, fill = "royalblue4", colour = NA),
  boxplot.args = list(fill = NA, colour = "darkblue", alpha = 0.7, width = 0.25, outlier.shape = NA),
  point.args = list(size = 1.5, color = "darkblue", alpha = 0.8)
  ) +
  # geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
  scale_y_continuous(limit = c(0, 4000),
  breaks = seq(0, 4000, by = 500),
    expand = c(0, 0)) +
  labs(
       x = "",
       y = "Commercial Products (#)") +
  theme_pubr() +
  fig_theme+
  coord_flip()+
  theme_erase_y+
  theme(plot.margin = margin(5, 15, 5, 15)) 


fig_products



# Fig 1f: Parts of use
fig_part <- decision_mat %>%
  filter(!is.na(part)) %>%  # Filter out NA values
  count(part) %>%           # Count frequency of each category
  ggplot(aes(x = reorder(part, -n), y = n)) +
  geom_col(fill = "royalblue4", alpha = 0.85) +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +  # Add count labels
  labs(title = "Medicinal tissue",
       x = "",
       y = "Species") +
  scale_y_continuous(expand = c(0, 0), limit = c(0,25)) +
  theme_pubr() +
  fig_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Display the plot
fig_part



# Fig 1g: Population status
fig_population <- decision_mat %>%
  filter(!is.na(population)) %>%  # Filter out NA values
  count(population) %>%           # Count frequency of each category
  mutate(population = factor(population, 
                            levels = c("CR", "EN", "VU", "NT", "LC", "DD"),
                            labels = c("Critically Endangered", "Endangered", 
                                       "Vulnerable", "Near Threatened", 
                                       "Least Concern", "Data Deficient"))) %>%
  ggplot(aes(x = reorder(population, -n), y = n)) +
  geom_col(fill = "royalblue4", alpha = 0.85) +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +  # Add count labels
  labs(title = "Population status",
       x = "",
       y = "Species") +
  scale_y_continuous(expand = c(0, 0),
  limits =c(0,60) ) +
  theme_pubr() +
  fig_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Display the plot
fig_population











# FIGURE 1 COLLAGE
# Combine raw data plots into one figure
fig_1x <- ggarrange(fig_height, fig_duration, fig_trials, 
                           fig_activities, fig_products, 
                           labels = c("A", "B", "C", "D", "E"),
                           ncol = 1, nrow = 5,
                           align = "v")
fig_1x


fig_1y <- ggarrange(fig_part, fig_population, 
                           labels = c("F", "G"),
                           ncol = 1, nrow = 2,
                           align = "h")

fig_1y



fig_1 <- ggarrange(fig_1x, fig_1y, 
                    ncol = 2, nrow = 1,
                    common.legend = F, legend = "top", 
                    widths = c(1, 1))
fig_1

showtext_auto()
ggsave("fig_1.pdf", fig_1, width = 11, height = 8, dpi = 300)







#_______

# FIGURE 2: WEIGHTS


## Aggregate weights





  fig_weight_agg <- 
    agg_weight_norm %>%
    pivot_longer(cols = everything(), names_to = "atts", values_to = "value") %>%
    mutate(scores = 1)%>%
    mutate(atts = dict[atts])%>%  #change the attribute codes to the actual names
    group_by(scores) %>%
    arrange(value) %>%  
    mutate(atts = factor(atts, levels = atts)) %>%
    mutate(label_y = cumsum(value) - 0.5*value) %>%
    ungroup() %>%
    ggplot(aes(x = scores, y = value, fill = atts))+
    geom_col(position = "stack") +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0,1,by = 0.2)) +
    coord_flip() +
    labs(x = "", y = "Weighting", fill = "Criteria")+
    theme_pubr()+
    scale_fill_aaas(alpha = 0.75)+
    fig_theme+
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          plot.margin = margin(5, 10, 15, 5),
          legend.position = "none")  # Added this line to hide the legend
  fig_weight_agg







## A plot of range of min and max weights for each criteria as error bars

fig_weight_range <- 
  agg_weight_norm %>%
  pivot_longer(cols = everything(), names_to = "atts", values_to = "value") %>%
    mutate(scores = 1)%>%
  mutate(atts = dict[atts])%>%
  ggplot(aes(x = atts, y = value)) +
  # Add individual data points from ind_weight_norm
  geom_point(data = ind_weight_norm %>% 
                pivot_longer(cols = -ID, names_to = "atts", values_to = "value") %>%
                mutate(atts = dict[atts]), 
              aes(x = atts, y = value),
              shape = 108, size = 3.5, alpha = 0.7, color = "navy") +
  geom_point(size = 3, color = "royalblue4") +
  geom_errorbar(data = ind_weight_range, inherit.aes = F, aes(x = atts, ymin = min, ymax = max), width = 0.2, color = "darkblue") +
  scale_y_continuous(expand = c(0, 0), limit = c(0,0.5)) +
  labs(x = "", y = "Weighting", fill = "Criteria") +
  theme_pubr() +
  fig_theme+
  scale_fill_aaas(alpha = 0.75)+
  coord_flip() +
  scale_x_discrete(limits = rev(unique(ind_weight_range$atts))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text=element_text(size=12),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())
fig_weight_range








## Create a heatmap of the individual weights from all decision-makers.

fig_ind_weight <-
  ind_weight_norm %>%
  pivot_longer(cols = -ID, names_to = "atts", values_to = "value") %>% # convert to long format
  mutate(atts = dict[atts])%>%
  ggplot(aes(x = atts, y = ID, fill = value))+
  geom_tile()+
  scale_fill_gradient(low="white", high="royalblue4")+
  scale_y_discrete(limits = rev(unique(ind_weight_norm$ID)))+
  labs(x = "Criteria", y = "Decision-makers", fill = "Weighting")+
  theme_pubr()+
  fig_theme+
  theme(axis.text=element_text(size=12),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.4),
        axis.ticks.length = unit(-0.15, "cm"))
fig_ind_weight




# Weights collage
fig_2x <- ggarrange(fig_weight_range, fig_ind_weight, 
                    labels = c("B", "C"),
                    ncol = 2, nrow = 1,
                    common.legend = F, legend = "right", align = "h",
                    widths = c(5, 7))

fig_2x

fig_2 <- ggarrange(fig_weight_agg, fig_2x, 
                    labels = c("A", ""),
                    ncol = 1, nrow = 2,
                    common.legend = F, legend = "top", align = "v",
                    heights = c(5, 15))

fig_2


showtext_auto()
ggsave( "fig_2.pdf", fig_2, width = 10, height = 7.5, dpi = 300)





#______________________________________________________________________________
# FIGURE 3: WEIGHTED SUM SCORES

# Processing data for visualization
# Convert to long format
species_scores_long <- species_scores %>%
  pivot_longer(
    cols = c(plant_score, medicinal_score, see_score),
    names_to = "score_category",
    values_to = "score"
  )







# Figure: Weighted sum scores
fig_weighted_sum <-
  species_scores_long %>%
  arrange(desc(perc_score)) %>%
  slice(c(1:60))%>%
  ggplot(aes(reorder(plant_name, score, sum), y = score, fill = score_category)) +
  geom_col() +
  coord_flip() +
  labs(x = "Species", y = "Total weighted score (%)", fill = "Criteria category") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr() +
  fig_theme+
  theme(axis.text.y = element_text(face = "italic"))+
  scale_fill_aaas(alpha = 0.75, guide = guide_legend(reverse=TRUE),
                  labels = c("Medicinal", "Plant", "Environmental and ecocnomic"))
fig_weighted_sum




fig_scores_plant <- 
  species_scores %>%
  select(plant_name, plant_score)%>%
  arrange(desc(plant_score))%>%
  slice(c(1:10))%>%
  ggplot(aes(reorder(plant_name, plant_score, sum), y = plant_score)) +
  geom_col(fill = "royalblue4", alpha = 0.85) +
  coord_flip() +
  labs(title = "Plant criteria",
  x = "Species", y = "Weighted score (%)") +
  scale_y_continuous(expand = c(0, 0),
  limits = c(0,50)) +
  theme_pubr() +
  fig_theme+
  theme(axis.text.y = element_text(face = "italic"),
  plot.title = element_text(size=12, hjust = -4))

fig_scores_plant



fig_scores_medicinal <- 
  species_scores %>%
  select(plant_name, medicinal_score)%>%
  arrange(desc(medicinal_score))%>%
  slice(c(1:10))%>%
  ggplot(aes(reorder(plant_name, medicinal_score, sum), y = medicinal_score)) +
  geom_col(fill = "royalblue4", alpha = 0.85) +
  coord_flip() +
  labs(title = "Medicinal criteria", x = "", y = "Weighted score (%)") +
  scale_y_continuous(expand = c(0, 0),
  limits = c(0,50)) +
  theme_pubr() +
  fig_theme+
  theme(axis.text.y = element_text(face = "italic"),
  plot.title = element_text(size=12, hjust = -4.2))

fig_scores_medicinal



fig_scores_see <- 
  species_scores %>%
  select(plant_name, see_score)%>%
  arrange(desc(see_score))%>%
  slice(c(1:10))%>%
  ggplot(aes(reorder(plant_name, see_score, sum), y = see_score)) +
  geom_col(fill = "royalblue4", alpha = 0.85) +
  coord_flip() +
  labs(title = "Environmental and Economic criteria", x = "", y = "Weighted score (%)") +
  scale_y_continuous(expand = c(0, 0),
  limits = c(0,50)) +
  theme_pubr() +
  fig_theme+
  theme(axis.text.y = element_text(face = "italic"),
  plot.title = element_text(size=12, hjust = 1))

fig_scores_see



# fig 3 collage
fig_3x <- ggarrange(fig_scores_plant, fig_scores_medicinal, fig_scores_see, 
                    labels = c( "B", "C", "D"),
                    ncol = 3, nrow = 1,
                    common.legend = F, legend = "top", align = "h")

fig_3x


fig_3 <- ggarrange(fig_weighted_sum, fig_3x, 
                    labels = c("A", ""),
                    ncol = 1, nrow = 2,
                    common.legend = F, legend = "top", align = "h",
                    heights = c(7, 5))
fig_3


showtext_auto()
ggsave("fig_3.pdf", fig_3, width = 10, height = 8.5, dpi = 300)


#---------------------------------------------------------


#_________________________________________________________
# FIGURE 4: SENSITIVITY ANALYSIS


  # Create a function to plot sensitivity analysis results
  plot_sensitivity <- function(sensitivity_data, criterion_name, top_n = 20) {
    sensitivity_data %>%
      slice(1:top_n) %>%
      ggplot(aes(x = reorder(plant_name, perc_score))) +
      coord_flip() +
      geom_segment( aes(x= plant_name, y=perc_score, yend=perc_score_plus), color="#4FAE62", linewidth = 4) +
      geom_segment( aes(x= plant_name, y=perc_score, yend=perc_score_minus), color="#C02D45", linewidth = 4) +
      geom_point(aes(y = perc_score), color = "black", size = 2.5) +
      labs(
        # title = paste("Sensitivity Analysis:", criterion_name),
        x = "Species",
        y = "Weighted Score (%)",
        # subtitle = "Green: +10% weight, Red: -10% weight, Black dot: Original"
      ) +
      # Add legend annotations
      annotate("text", x = 20, y = 86, 
             label = paste(criterion_name),
             hjust = 0, size = 4, color = "black") +
      theme_pubr() +
      theme(
        text = element_text(family = "Source sans pro", face = "plain"),
        axis.ticks = element_line(linewidth = 0.2),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.line = element_line(linewidth = 0.2),
        axis.text = element_text(size=11),
        plot.margin = margin(5,15,5,5),
        axis.text.y = element_text(face = "italic", size = 10),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        axis.title.y = element_blank()
        )+
        scale_y_continuous(limits = c(85, 100.25),
                           breaks = seq(75, 100, by = 5),
                           expand = c(0, 0))
  }

plot_sensitivity(sensitivity_height, "Height")
plot_sensitivity(sensitivity_part, "Part of Use")
plot_sensitivity(sensitivity_duration, "Duration")
plot_sensitivity(sensitivity_trials, "Trials")
plot_sensitivity(sensitivity_activities, "Activities")
plot_sensitivity(sensitivity_products, "Products")
plot_sensitivity(sensitivity_population, "Population Status")


# Plot the total score change for top and bottom species
total_sensitivity_plot <- sensitivity_plot_data %>%
  ggplot(aes(x = reorder(plant_name, -total_score_change), y = total_score_change, fill = group)) +
  geom_col(position = "dodge", alpha = 0.8) +
  coord_flip() +
  geom_vline(xintercept = 10.5, linetype = "dashed", color = "grey") +
  labs(fill = "Sensitivity Group",
       y = "Total Score Change (%)") +
  scale_fill_manual(values = c("Most sensitive" = "firebrick3", "Least sensitive" = "royalblue4")) +
      theme_pubr() +
      theme(
        text = element_text(family = "Source sans pro", face = "plain"),
        axis.ticks = element_line(linewidth = 0.2),
        axis.ticks.length.y = unit(-0, "cm"),
        axis.line = element_line(linewidth = 0.2),
        axis.text = element_text(size=11),
        plot.margin = margin(5,15,5,5),
        axis.text.y = element_text(face = "italic", size = 10),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        axis.title.y = element_blank()
        )+
  theme(
        legend.position = c(0.75, 0.85),
        legend.direction = "vertical",  # Arrange legend items horizontally
        # legend.justification = c(1, 1),   # Anchor point at the top-right of the legend box
        # legend.background = element_rect(fill = "white", color = NA, alpha = 0.7),  # Semi-transparent background
        legend.margin = margin(2, 2, 2, 2),  # Small margin around legend
        legend.key.size = unit(0.99, "lines"),  # Slightly smaller legend keys
        legend.title = element_text(size = 10),
    )+
  scale_y_continuous(limits = c(0, 15),
                    breaks = seq(0, 15, by = 5),
                    expand = c(0, 0))

total_sensitivity_plot


# Combine all sensitivity plots into one figure using plot_grid
 plot_sensitivity_fig <- 
  plot_grid(
    plot_sensitivity(sensitivity_height, "Height")+
      annotate("segment", x = 18, y = 89, xend = 18, yend = 91, 
           color = "#4FAE62", linewidth = 3) +
      annotate("segment", x = 18, y = 86, xend = 18, yend = 88, 
           color = "#C02D45", linewidth = 3) +
      annotate("text", x = 19, y = 89, label = "+10% weight", 
           hjust = 0, size = 3, color = "#4FAE62") +
      annotate("text", x = 19, y = 86, label = "-10% weight", 
           hjust = 0, size = 3, color = "#C02D45"),
    plot_sensitivity(sensitivity_part, "Part of Use"),
    plot_sensitivity(sensitivity_duration, "Duration"),
    plot_sensitivity(sensitivity_trials, "Trials"),
    plot_sensitivity(sensitivity_activities, "Activities"),
    plot_sensitivity(sensitivity_products, "Products"),
    plot_sensitivity(sensitivity_population, "Population Status"),
    total_sensitivity_plot,
    ncol = 2, labels = "AUTO" , align = "v")

plot_sensitivity_fig

showtext_auto()
ggsave("fig_4.pdf", plot_sensitivity_fig, width = 12, height = 14, dpi = 300)
















  plot_ranking_sensitivity <- function(sensitivity_data, criterion_name, top_n = 20) {
      sensitivity_data %>%
      slice(1:top_n) %>%
      ggplot(aes(x = reorder(plant_name, -rank))) +
      coord_flip() +
      geom_segment( aes(x= plant_name, y=rank, yend=rank_plus), color="#4FAE62", linewidth = 4) +
      geom_segment( aes(x= plant_name, y=rank, yend= rank_minus), color="#C02D45", linewidth = 4) +
      geom_point(aes(y = rank), color = "black", size = 2.5) +
      labs(
        title = paste("Rank sensitivity:", criterion_name),
        x = "Species",
        y = "Rank",
        # subtitle = "Green: +10% weight, Red: -10% weight, Black dot: Original"
      ) +
      # Add legend annotations
      annotate("text", x = 20, y = 86, 
             label = paste(criterion_name),
             hjust = 0, size = 4, color = "black") +
      theme_pubr() +
      theme(
        text = element_text(family = "Source sans pro", face = "plain"),
        axis.ticks = element_line(linewidth = 0.2),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.line = element_line(linewidth = 0.2),
        axis.text = element_text(size=11),
        plot.margin = margin(5,15,5,5),
        axis.text.y = element_text(face = "italic", size = 10),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        axis.title.y = element_blank()
        )+
        scale_y_continuous(limits = c(0, 25),
                           breaks = c(1, 5, 10, 15, 20, 25),
                           expand = c(0, 0))
  }



sensitivity_rank_fig <- 
  plot_grid(
    plot_ranking_sensitivity(sensitivity_height, "Height"),
    plot_ranking_sensitivity(sensitivity_part, "Part of Use"),
    plot_ranking_sensitivity(sensitivity_duration, "Duration"),
    plot_ranking_sensitivity(sensitivity_trials, "Trials"),
    plot_ranking_sensitivity(sensitivity_activities, "Activities"),
    plot_ranking_sensitivity(sensitivity_products, "Products"),
    plot_ranking_sensitivity(sensitivity_population, "Population Status"),
    ncol = 2, labels = "AUTO" , align = "v")

sensitivity_rank_fig

showtext_auto()
ggsave("fig_s1.pdf", sensitivity_rank_fig, width = 12, height = 14, dpi = 300)









# Create a bar plot for total score change by criterion
criteria_sensitivity_plot <-
  total_score_change_summary %>%
  ggplot(aes(x = reorder(criterion, total_change), y = total_change, f)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Total Score Change by Criterion",
       x = "Criterion",
       y = "Total Score Change") +
  theme_pubr() +
  theme(
      text = element_text(family = "Source sans pro", face = "plain"),
      axis.ticks = element_line(linewidth = 0.2),
      axis.ticks.length.y = unit(-0, "cm"),
      axis.line = element_line(linewidth = 0.2),
      axis.text = element_text(size=11),
      plot.margin = margin(5,15,5,5),
      # axis.text.y = element_text(face = "italic", size = 10),
      plot.title = element_text(size = 12),
      plot.subtitle = element_text(size = 10),
      axis.title.y = element_blank()
    )+
    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, by = 20),
                       expand = c(0, 0)) 
criteria_sensitivity_plot


showtext_auto()
ggsave("fig_s2.pdf", criteria_sensitivity_plot, width = 6, height = 4, dpi = 300)





### END OF SCRIPT ###


