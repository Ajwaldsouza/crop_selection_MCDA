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
          "part"       = "Part of use",
          "duration"   = "Duration to harvest",
          "trials"     = "No. of clinical trials",
          "activities" = "Medicinal activities",
          "products"   = "Number of commercial products",
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

rowSums(agg_weight_norm)




# Normalise individual weights of each ID across rows from column 2 to 8, to 1
ind_weight_norm <- ind_weight %>%
  rowwise() %>%
  mutate(across(2:8, ~ .x / sum(c_across(2:8)))) %>%
  ungroup()


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
  boxplot.args = list(fill = NA, colour = "darkblue", alpha = 0.7),
  point.args = list(size = 1.5, color = "darkblue")
  ) +
  # geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
  scale_y_continuous(limit = c(0, 3.5),
                    breaks = seq(0, 3.5, by = 0.5),
                     expand = c(0, 0)) +
  labs(title = "Plant height",
       x = "",
       y = "Plant height (m)") +
  theme_pubr() +
  fig_theme+
  coord_flip()+
  theme_erase_y

fig_height


# Fig 1b: Duration
fig_duration <- decision_mat %>%
  filter(!is.na(duration)) %>% # First, clean the data by removing NA values
  select(plant_name, duration)%>%
  ggplot( aes(x = "", y = duration)) +
  geom_rain(fill = "royalblue4", alpha = 0.7,
  violin.args = list(alpha = .3, fill = "royalblue4", colour = NA),
  boxplot.args = list(fill = NA, colour = "darkblue", alpha = 0.7),
  point.args = list(size = 1.5, color = "darkblue")
  ) +
  # geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
    scale_y_continuous(limit = c(0, 2500),
  breaks = seq(0, 2500, by = 500),
    expand = c(0, 0)) +
  labs(title = "Duration",
       x = "",
       y = "Duration to harvest (days)") +
  theme_pubr() +
  fig_theme+
  coord_flip()+
  theme_erase_y

fig_duration


# Fig 1c: Trials
fig_trials <- decision_mat %>%
  filter(!is.na(trials)) %>% # First, clean the data by removing NA values
  select(plant_name, trials)%>%
  ggplot( aes(x = "", y = trials)) +
  geom_rain(fill = "royalblue4", alpha = 0.7,
  violin.args = list(alpha = .3, fill = "royalblue4", colour = NA),
  boxplot.args = list(fill = NA, colour = "darkblue", alpha = 0.7),
  point.args = list(size = 1.5, color = "darkblue")
  ) +
  # geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
  #   scale_y_continuous(limit = c(0, 2500),
  # breaks = seq(0, 2500, by = 500),
  #   expand = c(0, 0)) +
  labs(title = "Trials",
       x = "",
       y = "Trials (#)") +
  theme_pubr() +
  fig_theme+
  coord_flip()+
  theme_erase_y

fig_trials


# Fig 1d: Activities
fig_activities <- decision_mat %>%
  filter(!is.na(activities)) %>% # First, clean the data by removing NA values
  select(plant_name, activities)%>%
  ggplot( aes(x = "", y = activities)) +
  geom_rain(fill = "royalblue4", alpha = 0.7,
  violin.args = list(alpha = .3, fill = "royalblue4", colour = NA),
  boxplot.args = list(fill = NA, colour = "darkblue", alpha = 0.7),
  point.args = list(size = 1.5, color = "darkblue")
  ) +
  # geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
    scale_y_continuous(limit = c(0, 900),
  breaks = seq(0, 900, by = 150),
    expand = c(0, 0)) +
  labs(title = "Activities",
       x = "",
       y = "Activities (#)") +
  theme_pubr() +
  fig_theme+
  coord_flip()+
  theme_erase_y

fig_activities






# Fig 1e: Commerical products
fig_products <- decision_mat %>%
  filter(!is.na(products)) %>% # First, clean the data by removing NA values
  select(plant_name, products)%>%
  ggplot( aes(x = "", y = products)) +
  geom_rain(fill = "royalblue4", alpha = 0.7,
  violin.args = list(alpha = .3, fill = "royalblue4", colour = NA),
  boxplot.args = list(fill = NA, colour = "darkblue", alpha = 0.7),
  point.args = list(size = 1.5, color = "darkblue")
  ) +
  # geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
  scale_y_continuous(limit = c(0, 4000),
  breaks = seq(0, 4000, by = 500),
    expand = c(0, 0)) +
  labs(title = "Distribution of Commercial Products by Species",
       x = "",
       y = "Number of Commercial Products") +
  theme_pubr() +
  fig_theme+
  coord_flip()+
  theme_erase_y

fig_products



# Fig 1f: Parts of use
fig_part <- decision_mat %>%
  filter(!is.na(part)) %>%  # Filter out NA values
  count(part) %>%           # Count frequency of each category
  # mutate(part = factor(part, 
  #                           levels = c("CR", "EN", "VU", "NT", "LC", "DD"),
  #                           labels = c("Critically Endangered", "Endangered", 
  #                                      "Vulnerable", "Near Threatened", 
  #                                      "Least Concern", "Data Deficient"))) %>%
  ggplot(aes(x = part, y = n)) +
  geom_col(alpha = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +  # Add count labels
  labs(title = "IUCN Conservation Status Distribution",
       x = "Conservation Status",
       y = "Count") +
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
  ggplot(aes(x = population, y = n)) +
  geom_col(alpha = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +  # Add count labels
  labs(title = "IUCN Conservation Status Distribution",
       x = "Conservation Status",
       y = "Count") +
  scale_y_continuous(expand = c(0, 0)) +
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
                           labels = c("a", "b", "c", "d", "e"),
                           ncol = 1, nrow = 5)
fig_1x


fig_1y <- ggarrange(fig_part, fig_population, 
                           labels = c("f", "g"),
                           ncol = 1, nrow = 2,
                           align = "h")

fig_1y



fig_1 <- ggarrange(fig_1x, fig_1y, 
                    ncol = 2, nrow = 1,
                    common.legend = F, legend = "top", 
                    widths = c(4, 3))
fig_1

# Save the combined figure if needed
ggsave("fig_1.pdf", fig_1, width = 12, height = 8, dpi = 300)







#_______

# FIGURE 2: WEIGHTS


## Aggregate weights
fig_weight_agg <- 
  agg_weight_norm %>%
  pivot_longer(cols = everything(), names_to = "atts", values_to = "value") %>%
    mutate(scores = 1)%>%
  mutate(atts = dict[atts])%>%                             #change the attribute codes to the actual names
  ggplot(aes(x = scores, y = value, fill = atts))+
  geom_col() +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(x = "", y = "Normalized weightage", fill = "Criteria")+
  theme_pubr()+
  scale_fill_aaas(alpha = 0.75)+
  fig_theme+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank())



fig_weight_agg





## Create a heatmap of the individual weights from all decision-makers.

fig_ind_weight <-
  ind_weight_norm %>%
  pivot_longer(cols = -ID, names_to = "atts", values_to = "value") %>% # convert to long format
  mutate(atts = dict[atts])%>%
  ggplot(aes(x = atts, y = ID, fill = value))+
  geom_raster()+
  scale_fill_gradient(low="white", high="royalblue4")+
  scale_y_discrete(limits = rev(unique(ind_weight_norm$ID)))+
  labs(x = "Criteria", y = "Decision-makers", fill = "Weights")+
  theme_pubr()+
  fig_theme+
  theme(axis.text=element_text(size=12),
  axis.text.x=element_text(angle=45, hjust=1))

fig_ind_weight






# Weights collage



fig_2 <- ggarrange(fig_weight_agg, fig_ind_weight, 
                    labels = c("a", "b"),
                    ncol = 1, nrow = 2,
                    common.legend = F, legend = "top", align = "v",
                    heights = c(5, 15))

fig_2


showtext_auto()
ggsave( "fig_2.pdf", fig_2, width = 10, height = 7.5, dpi = 300)





#_______
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
  labs(x = "Species", y = "Weighted sum score", fill = "Score category") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr() +
  fig_theme+
  theme(axis.text.y = element_text(face = "italic"))+
  scale_fill_aaas(alpha = 0.75, guide = guide_legend(reverse=TRUE),
                  labels = c("Medicinal", "Plant", "SEE"))
fig_weighted_sum




fig_scores_plant <- 
  species_scores %>%
  select(plant_name, plant_score)%>%
  arrange(desc(plant_score))%>%
  slice(c(1:10))%>%
  ggplot(aes(reorder(plant_name, plant_score, sum), y = plant_score)) +
  geom_col(fill = "royalblue4") +
  coord_flip() +
  labs(x = "Species", y = "Plant score") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr() +
  fig_theme+
  theme(axis.text.y = element_text(face = "italic"))

fig_scores_plant



fig_scores_medicinal <- 
  species_scores %>%
  select(plant_name, medicinal_score)%>%
  arrange(desc(medicinal_score))%>%
  slice(c(1:10))%>%
  ggplot(aes(reorder(plant_name, medicinal_score, sum), y = medicinal_score)) +
  geom_col(fill = "royalblue4") +
  coord_flip() +
  labs(x = "", y = "Medicinal score") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr() +
  fig_theme+
  theme(axis.text.y = element_text(face = "italic"))

fig_scores_medicinal



fig_scores_see <- 
  species_scores %>%
  select(plant_name, see_score)%>%
  arrange(desc(see_score))%>%
  slice(c(1:10))%>%
  ggplot(aes(reorder(plant_name, see_score, sum), y = see_score)) +
  geom_col(fill = "royalblue4") +
  coord_flip() +
  labs(x = "", y = "SEE score") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr() +
  fig_theme+
  theme(axis.text.y = element_text(face = "italic"))

fig_scores_see



# fig 3 collage
fig_3x <- ggarrange(fig_scores_plant, fig_scores_medicinal, fig_scores_see, 
                    labels = c( "b", "c", "d"),
                    ncol = 3, nrow = 1,
                    common.legend = F, legend = "top", align = "h")

fig_3x


fig_3 <- ggarrange(fig_weighted_sum, fig_3x, 
                    labels = c("a", ""),
                    ncol = 1, nrow = 2,
                    common.legend = F, legend = "top", align = "h",
                    heights = c(7, 5))
fig_3


showtext_auto()
ggsave("fig_3.pdf", fig_3, width = 10, height = 8.5, dpi = 300)


#---------------------------------------------------------












### END OF SCRIPT ###