#-------------------------------------------------------------------------------
# INITIAL SETUP
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
mydir <- "Raw" # Mention the directory (as "mydir") in the W.Dir. containing the datafiles
myfiles <- list.files(path = mydir, pattern = "*.txt", full.names = TRUE) # List all the .csv files in the specified "mydir" and assign it to "myfiles".
myfiles # Shows all the files present


#-------------------------------------------------------------------------------
# DATA IMPORT AND PREPROCESSING
#-------------------------------------------------------------------------------


# Load the raw bibliometric dataset into R
bibliometric_data <- ldply(myfiles, read_delim,
  delim = "\t",    # collectively apply "import function for all the files in the specified directory
  quote = "'", escape_double = FALSE,
  trim_ws = TRUE
) %>%
  as_tibble() %>% # Convert the imported data as a dataframe
  select(select = c(2, 10)) %>%
  rename(
    authors = select1,
    title = select2
  ) %>%
  mutate(id = row_number()) # Add a row called "id" which is the study id





# Extract scientific names from the titles in the bibliometric data
scientific_names <- str_extract_all(bibliometric_data$title, "\\b[A-Z][a-z]+ [a-z]+\\b")


# Add extracted scientific names to the bibliometric data in a new column named 'species'
species_raw <- bibliometric_data %>%
  mutate(species = sapply(scientific_names, function(x) if(length(x) > 0) paste(x, collapse = ", ") else NA)) %>%
  filter(!is.na(species)) %>%
  separate_rows(species, sep = ",")


# Words/phrases to exclude
# This list includes the most common filler words in the identified list
# Using this list, all the entries containing these words will be removed

words_to_remove <- c(
  "the", "in", "this", "and", "of", "results", "study", "is",
  "to", "these", "we", "it", "our", "all", "medicinal", "was",
  "present", "addition", "chinese", "for", "species", "plants",
  "were", "aim", "on", "plant", "as", "analysis", "by", "are",
  "findings", "with", "data", "among", "rights", "materials",
  "extracts", "most", "test", "medicine", "studies", "a",
  "highest", "extract", "from", "total", "based", "there",
  "acid", "antioxidant", "has", "an", "published",
  "staphylococcus", "family", "also", "conclusion", "further",
  "aureus", "although", "compounds", "phytochemical",
  "ethnopharmacological", "current", "relevance",
  "escherichia", "have", "coli", "at", "rats", "research",
  "essential", "traditional", "activity", "treatment",
  "chemical", "showed", "both", "two", "using", "after",
  "main", "different", "according", "region", "use", "its",
  "work", "vitro", "leaves", "bacillus", "molecular",
  "effects", "they", "expression", "pseudomonas", "method",
  "due", "three", "their", "herbal", "many", "high",
  "western", "one", "several", "obtained", "together",
  "samples", "despite", "cell", "major", "objective", "some",
  "phenolic", "order", "taken", "leaf", "effect", "indian",
  "maximum", "found", "genus", "china", "content",
  "production", "can", "recent", "antimicrobial", "cd",
  "natural", "while", "antibacterial", "anti", "compared",
  "pharmacological", "concentrations", "structures", "that",
  "african", "aspergillus", "significant", "herb", "herbs",
  "four", "docking", "since", "methanol", "extraction",
  "purpose", "no", "contrast", "during", "murashige",
  "various", "india", "levels", "pervious", "potential",
  "root", "water", "identified", "crude", "application",
  "result", "quantitative", "investigation", "methanolic",
  "such", "used", "oil", "presence", "aqueous", "blot",
  "more", "information", "experimental", "asian", "review",
  "arabidopsis", "genes", "ethonobotanical", "here", "new",
  "active", "cytotoxicity", "under", "paper", "them",
  "inhibition", "protein", "when", "analyses", "people",
  "screening", "identification", "cells", "chromatography",
  "fusarium", "ayurvedic", "mice", "seeds", "stress",
  "methods", "salmonella", "five", "considering", "key",
  "province", "test", "countries", "contents", "ethyl",
  "other", "administration", "genetic", "through", "isolated",
  "toxicity", "vivo", "gene", "concentration", "developed",
  "best", "district", "growth", "flavonoids", "or", "acute",
  "could", "cancer", "out", "medicines", "seed", "biological",
  "africa", "bioactive", "general", "combination", "knowledge",
  "oral", "then", "first", "brazilian", "changes", "comparison",
  "diabetes", "terminalia", "alkaloids", "beacuse", "material",
  "following", "applications", "particular", "average",
  "compound", "increased", "each", "preliminary", "regarding",
  "six", "correlation", "had", "investigated", "european",
  "structure", "observed", "system", "design", "food", "fruits",
  "korean", "mean", "mass", "show", "collected", "control",
  "group", "accumulation", "report", "against", "complete",
  "into", "inhibits", "survey", "transcriptome", "via"
)




# Create a regular expression pattern to match standalone words
pattern <- paste0("\\b", paste(words_to_remove, collapse = "\\b|\\b"), "\\b")



# Remove rows containing the specified words as standalone words 
species_clean <- species_raw %>%
  filter(!grepl(pattern, species, ignore.case = TRUE)) %>%
  distinct()

write.csv(species_clean, "species_clean.csv")


#-------------------------------------------------------------------------------
# TAXONOMIC IDENTIFICATION
#-------------------------------------------------------------------------------


# Identify and standardize the scientific names using the NCBI database
species_identified <- species_clean %>%
  mutate(uid = get_uid(species, db = "ncbi", ask = FALSE)) %>%
  na.omit() %>%
  mutate(sci_name = taxid2name(uid, db = "ncbi", verbose = FALSE, warn = FALSE)) %>%
  select(-c(2))


write.csv(species_identified, "species_identified.csv")



# _______

# FILTERING ONLY THE PLANT KINGDOM

## Use the `classification` function from the 'taxizedb' package to retrieve the classification
classification <-
  taxizedb::classification(
    species_identified$sci_name , db = "ncbi", get = "kingdom"
  )




# # Extract the kingdom for each species from the classification list
kingdom <- map_chr(classification, ~ {
  if (is.list(.x) || is.data.frame(.x)) {
    rank_index <- which(.x$rank == "kingdom")
    if (length(rank_index) > 0) {
      .x$id[rank_index]
    } else {
      NA
    }
  } else {
    # Handle the case where .x is an atomic vector or something else unexpected
    NA
  }
})



# Assign kingdom to species_identified and filter for plants only
species_plant <- species_identified %>%
  mutate(kingdom = kingdom) %>%
  filter(kingdom == 33090) %>%
  select(-c(1,3))

write_csv(species_plant, "species_plant.csv")



#-------------------------------------------------------------------------------
# DATA SUMMARY
#-------------------------------------------------------------------------------


# Total study counts of each species
frequency_counts <- species_plant %>%
  group_by(sci_name, uid) %>%
  summarise(count = n()) %>%
  arrange(desc(count))







write.csv(frequency_counts, "species_count.csv")


# ______





#-------------------------------------------------------------------------------
# SUPPLEMENTARY SCRIPT
#-------------------------------------------------------------------------------



# Supplementary script used to identify the most common words for elimination
df_tokens <- species_clean %>%
  unnest_tokens(word, species)


word_counts <- df_tokens %>%
  count(word, sort = TRUE)


word_list <- word_counts %>%
  arrange(desc(n))

write_csv(word_list, "commonwords.csv")

#-------------------------------END OF SCRIPT-----------------------------------
