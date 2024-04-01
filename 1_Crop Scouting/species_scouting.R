install.packages("librarian")
librarian::shelf(plyr, dplyr, tidyr, tibble, rstudioapi,
                 stringr,taxize,taxizedb, readr, tidyverse, tidytext, purrr,
                 update_all = FALSE)

# Setting working directory
setwd(dirname(getActiveDocumentContext()$path))                                 # Set working directory to source file location
mydir="Raw"                                                                     #Mention the directory (as "mydir") in the W.Dir. containing the datafiles
myfiles= list.files(path=mydir, pattern="*.txt", full.names=TRUE)               #List all the .csv files in the specified "mydir" and assign it to "myfiles". 
myfiles                                                                         #Shows all the files present 






# Load the dataset into R
bibliometric_data <- ldply(myfiles, read_delim, delim = "\t",             #collectively apply "import function for all the files in the specified directory
                                 quote = "'", escape_double = FALSE, 
                                 trim_ws = TRUE)%>%     
  as_tibble()%>%                                                                #Convert the imported data as a dataframe
  select(select = c(2, 9))%>%
  rename(authors = select1,
         title = select2)%>%
  mutate(id = row_number())                                                     # Add a row called "id" which is the study id





scientific_names <- str_extract_all(bibliometric_data$title, "\\b[A-Z][a-z]+ [a-z]+\\b")

#Create a new column 'species' and retain 'id'
bibliometric_data$species <- NA
bibliometric_data$species[!sapply(scientific_names, is.null)] <- sapply(scientific_names, paste, collapse = ", ")
species_raw <- bibliometric_data[!is.na(bibliometric_data$species), c("id", "species")]%>%separate_rows(species, sep = ",")





# Words/phrases to exclude
# This list includes the most common filler words in the identified list 
# Using this list, all the entries containing these words will be removed

words_to_remove <- c("the", "in", "this", "and", "of", "results", "study", "is", 
                     "to", "these", "we", "it", "our", "all", "medicinal", "was", 
                     "present", "addition", "chinese", "for", "species", "plants", 
                     "were", "aim", "on", "plant", "as", "analysis", "by", "are", 
                     "findings", "with", "data","among", "rights", "materials", 
                     "extracts", "most", "test", "medicine", "studies", "a", 
                     "highest", "extract", "from", "total",  "based", "there",
                     "acid", "antioxidant", "has", "an", "published", 
                     "staphylococcus", "family", "also", "conclusion", "further", 
                     "aureus", "although", "compounds", "phytochemical", 
                     "ethnopharmacological", "current",  "relevance", 
                     "escherichia", "have", "coli", "at", "rats",  "research",
                     "essential", "traditional", "activity", "treatment", 
                     "chemical",  "showed", "both", "two", "using", "after", 
                     "main",  "different", "according", "region", "use", "its",
                     "work", "vitro",  "leaves",  "bacillus", 'molecular', 
                     "effects", "they", "expression",  "pseudomonas",  "method", 
                     "due", "three", "their", "herbal", "many", "high",
                     "western","one", "several", "obtained", "together", 
                     "samples", "despite", "cell","major", "objective", "some", 
                     "phenolic", "order", "taken", "leaf",  "effect", "indian",
                     "maximum", "found", "genus", "china", "content",
                     "production", "can", "recent", "antimicrobial", "cd",
                     "natural", "while", "antibacterial", "anti", "compared",
                     "pharmacological",  "concentrations", "structures", "that",
                     "african", "aspergillus",  "significant", "herb", "herbs",
                     "four", "docking", "since", "methanol",  "extraction", 
                     "purpose", "no", "contrast", "during", "murashige", 
                     "various",  "india", "levels", "pervious", "potential", 
                     "root", "water", "identified", "crude", "application", 
                     "result", "quantitative", "investigation",  "methanolic", 
                     "such", "used", "oil", "presence", "aqueous", "blot", 
                     "more", "information", "experimental", "asian", "review",
                     "arabidopsis", "genes", "ethonobotanical", "here", "new",
                     "active", "cytotoxicity", "under", "paper", "them",
                     "inhibition", "protein", "when", "analyses", "people",
                     "screening", "identification", "cells", "chromatography",
                     "fusarium", "ayurvedic", "mice", "seeds", "stress",
                     "methods", "salmonella", "five", "considering", "key",
                     "province",  "test", "countries", "contents", "ethyl",
                     "other", "administration", "genetic", "through", "isolated",
                     "toxicity", "vivo","gene", "concentration", "developed",
                     "best", "district", "growth", "flavonoids", "or", "acute",
                     "could", "cancer" , "out", "medicines", "seed", "biological",
                     "africa", "bioactive", "general", "combination", "knowledge",
                     "oral", "then", "first", "brazilian", "changes", "comparison",
                     "diabetes", "terminalia", "alkaloids", "beacuse", "material",
                     "following", "applications", "particular", "average", 
                     "compound", "increased", "each", "preliminary", "regarding",
                     "six", "correlation", "had", "investigated", "european",
                     "structure", "observed", "system", "design", "food", "fruits",
                     "korean", "mean", "mass", "show", "collected", "control",
                     "group", "accumulation", "report", "against", "complete",
                     "into", "inhibits", "survey", "transcriptome", "via")




# Create a regular expression pattern to match standalone words
pattern <- paste0("\\b", paste(words_to_remove, collapse = "\\b|\\b"), "\\b")


# Remove rows containing the specified words as standalone words

species_clean <- species_raw[!grepl(pattern, species_raw$species, ignore.case = TRUE), ]%>%
  unique()






# Identify and standardize the scientific names using the NCBI database
species_identified <- species_clean %>% 
  mutate(uid = get_uid(species, db = "ncbi", ask = T))%>%
  na.omit()%>%
  mutate(sci_name = taxid2name(uid, db = "ncbi", verbose = TRUE, warn = TRUE))%>%
  select(-c(2))








#_______

# FILTERING ONLY THE PLANT KINGDOM

# Use the `classification` function from the 'taxize' package to retrieve the classification
classification <- taxize::classification(species_identified$sci_name, db = "ncbi", get = "all")
  


# Extract the kingdom for each species from the classification list
kingdomx <- map_chr(xx, ~ {
  rank_index <- which(.x$rank == "kingdom")
  if (length(rank_index) > 0) {
    .x$id[rank_index]
  } else {
    NA
  }
})





species_identified$kingdom <- kingdom


species_plant <- subset(species_identified, kingdom == 33090)



#____




species_kingdom_plant <- read_csv("species_kingdom_plant.csv")





# Total study counts of each species
frequency_counts <- species_kingdom_plant %>%
  group_by(sci_name) %>%
  summarise(count = n())%>%
  arrange(desc(count))


write.csv(frequency_counts, "species_count.csv")


#______








# Supplementary script used to identify the most common words for elimination 
df_tokens <- species_clean %>%
  unnest_tokens(word, species)


word_counts <- df_tokens %>%
  count(word, sort = TRUE)


word_list <- word_counts %>%
  arrange(desc(n))

write_csv(word_list, "commonwords.csv")

#----
  
  

  