
#-------------------------------------------------------------------------------
#            1. INSTALL AND LOAD PACKAGES + SET WORKING DIRECTORY
#-------------------------------------------------------------------------------


# Install the 'librarian' package if not already installed
install.packages("librarian")
# Load multiple packages using the 'librarian' package
librarian::shelf(
  plyr,
  dplyr,
  tidyr,
  tibble,
  rstudioapi,
  stringr,
  readr,
  tidyverse,
  tidytext,
  purrr,
  devtools,
  taxadb,
  rredlist,
  update_all = FALSE,
  quiet = TRUE
)
# Install specific versions of archived packages
install_version("taxizedb", version = "0.3.1", repos = "https://cloud.r-project.org")
install_version("bold", version = "1.3.0", repos = "https://cloud.r-project.org")
install_version("ape", version = "5.5", repos = "https://cloud.r-project.org")

install_version("taxize", version = "0.9.100", repos = "https://cloud.r-project.org")

# Load the installed packages 
library(taxizedb)
library(taxize)



# Setting working directory
setwd(dirname(getActiveDocumentContext()$path))


#-------------------------------------------------------------------------------
#             2. MASTER DATA IMPORT AND CLEANING
#-------------------------------------------------------------------------------

#Import the raw master dataset
species_dataset_raw <- read_csv("species_dataset_raw.csv") # This is the dataset that will contain all the raw data for the assessment criteria. 

# filter species that contain 'count' <30. This is to remove species with low counts of research papers
species_dataset_filtered <- species_dataset_raw %>%
  filter(count > 30)






#-------------------------------------------------------------------------------
#             3. CRITERIA: PLANT HEIGHT
#-------------------------------------------------------------------------------

# Import the master file and clean to only retain plant height-related columns and rows
plant_height <- read_delim(
  "plant_height.txt",
  delim = "\t",
  quote = "'",
  escape_double = FALSE,
  trim_ws = TRUE
) %>%
  as_tibble() %>%
  # select(c(3, 6, 7, 8, 10, 21, 22)) %>%
  filter(TraitID == '3106')


# Count the number of unique species in the dataset
species_name_count <- plant_height %>%
  count(AccSpeciesName)




# Summarise the dataset by getting the median for 'StdValue' for every unique "AccSpeciesID"
plant_height_summary <- plant_height %>%
  group_by(AccSpeciesID, AccSpeciesName) %>%
  summarise(median_height = median(StdValue, na.rm = TRUE)) %>%
  as_tibble()%>%
  na.omit()

#---











# get the unique ID for each species in the summary dataset using taxizedb  package
plant_height_summary_uid <-
  plant_height_summary %>%
  mutate(uid = get_uid(AccSpeciesName, db = "ncbi", ask = TRUE))


write.csv(plant_height_summary_uid, "plant_height_summary_uid.csv")

# If you need this dataset again, import it without having to run through the uid process.
plant_height_summary_uid <- read_csv("plant_height_summary_uid.csv")










# Add the median height to the master dataset using uid matching
# Merge the datasets based on the 'uid' column
species_dataset1 <- 
  species_dataset_filtered %>%
  dplyr::left_join(plant_height_summary_uid %>% 
  select(uid, median_height), by = "uid")





#-------------------------------------------------------------------------------
#             4. CRITERIA: IUCN POPULATION STATUS
#-------------------------------------------------------------------------------
# In this section, we will use the 'rredlist' package to retrieve the IUCN Red List 
# status for each species in the dataset.


# Run the following function when using 'rredlist' for the first time to set up the IUCN API key
# rredlist::rl_use_iucn()




# Function to add IUCN Red List population status codes to dataset
#'Processes each 'plant_name' entry (formatted "genus species") to:
#' 1. Query IUCN API for species conservation status
#' 2. Extract Red List category code (e.g., LC, EN, VU)
#' 3. Populate 'pop_status' column with codes or "DD" (Data Deficient)
#' when species data is unavailable/errors occur


add_pop_status <- function(data) {
  # Ensure the 'plant_name' column exists
  if (!"plant_name" %in% names(data)) {
    stop("Dataset must contain a column named 'plant_name'")
  }
  
  # Create new column for population status
  data$pop_status <- NA_character_
  
  # Iterate through each row
  for (i in seq_len(nrow(data))) {
    # Split binomial name into genus and species
    name_parts <- strsplit(data$plant_name[i], " ")[[1]]
    
    # Skip if name format is invalid
    if (length(name_parts) < 2) next
    
    genus <- name_parts[1]
    species <- name_parts[2]
    
    # Try to get IUCN data with error handling
    iucn_result <- tryCatch({
      rredlist::rl_species_latest(genus, species)
    }, error = function(e) NULL)
    
    # Extract status code if available
    if (!is.null(iucn_result)) {
      status_code <- iucn_result$red_list_category$code
      if (!is.null(status_code)) {
        data$pop_status[i] <- as.character(status_code)
      }
    }
  }
  # Convert remaining NAs to "DD"
  data$pop_status[is.na(data$pop_status)] <- "DD"
  return(data)
}


# Apply the function to the dataset
species_dataset2 <- add_pop_status(species_dataset1)







#-------------------------------------------------------------------------------
#             5. CRITERIA: MEDICINAL ACTIVITIES
#-------------------------------------------------------------------------------
# In this section, we will quantify the number of medicinal activities
# associated with each species in the dataset. The raw data is sourced from Dr.
# Duke's Phytochemical and Ethnobotanical Databases.


# The dataset containing medicinal activities are downloaded for each species
# and stored in the 'raw' folder. 

# The code below processes raw data files to quantify medicinal activities:
# 1. Reads all CSV files from the 'raw' directory (each file representing a plant species)
# 2. For each file, counts the number of non-NA entries in the "Activity" column
# 3. Compiles results into a dataset showing each species and its activity count
# 4. Saves the compiled results to "activity_count_results.csv"
#




# Get list of all CSV files in the 'raw' folder. The raw files should be downloded seprately and stored in the 'raw' folder
csv_files <- list.files(path = "raw", pattern = "*.csv", full.names = TRUE)




# Create empty dataframe to store results
count <- tibble(
  species = character(),
  activities = numeric()
)

# Process each CSV file
for (file in csv_files) {
  # Extract just the file name without path
  file_name <- basename(file)
  
  # Read the CSV file
  data <- read_csv(file)
  
  # Count rows where "Activity" column is not NA
  if ("Activity" %in% colnames(data)) {
    activity_count <- sum(!is.na(data$Activity))
  } else {
    activity_count <- NA  # If Activity column doesn't exist
  }
  
  # Add results to the count dataframe
  count <- count %>%
    add_row(species = file_name, activities = activity_count)
}

# Display the results
print(count)

# Optional: Save the results
write_csv(count, "activity_count_results.csv")







# Add the medicinal activities count to the master dataset using species matching
# Merge the datasets based on the 'species' column
species_dataset3 <- 
  species_dataset2 %>%
  dplyr::left_join(count, by = c("species" = "species"))















write.csv(species_dataset3, "species_dataset_data_pending.csv")
