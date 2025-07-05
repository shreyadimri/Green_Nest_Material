pacman::p_load(here,tidyverse, readr)
data_GNM_001_rep_table3 <- read_csv("data/02_data_extraction/extracted_raw_data/data_GNM_001_rep_table3.csv")

# The dataset was provided in the paper as a table. It is not available in the 
# published dataset so I am using it from the paper as a saved .csv file

# Info from Table 3: abundance of nest-dwelling ectoparasitic arthropods between 
# the 2 experimental groups of collected nests (treatment: control—0 and test—1)

# Common names of all the taxonomic groups...
common_names <- tibble(
  TaxonomicGroup = c("Siphonaptera", "Diptera", "Protocalliphora", "Acari", "Ixodida",
                     "Coleoptera", "Dermestidae", "Histeridae", "Staphylinidae", "Hymenoptera",
                     "Lepidoptera", "Collembola", "Psocodea", "Hemiptera"),
  CommonName = c("Fleas", "Flies", "Blowfly Parasites", "Mites", "Ticks",
                 "Beetles", "Skin Beetles", "Clown Beetles", "Rove Beetles", "Wasps, Bees, and Ants",
                 "Butterflies and Moths", "Springtails", "Booklice and Barklice", "True Bugs")
)

# Convert multiple columns into long format
summary_data <- data_GNM_001_rep_table3 %>%
  pivot_longer(cols = c(Siphonaptera, Diptera, Protocalliphora, Acari, Ixodida,
                        Coleoptera, Dermestidae, Histeridae, Staphylinidae, Hymenoptera,
                        Lepidoptera, Collembola, Psocodea, Hemiptera),
               names_to = "TaxonomicGroup",
               values_to = "Count") %>%
# To calculate mean, SD and sample size by treatment for all measured taxonomic groups
  group_by(Treatment, TaxonomicGroup) %>%
  summarise(mean = mean(Count, na.rm = TRUE),
            sd = sd(Count, na.rm = TRUE),
            samplesize = n(),
            .groups = "drop") %>%
  left_join(common_names, by = "TaxonomicGroup") # Add common names

# Print the final dataframe
print(summary_data)


 # Replace 0 with "Control" and 1 with "Treatment" in the Treatment column as given in table 3 summary
summary_data <- summary_data %>%
  mutate(Treatment = recode(Treatment, `0` = "Control", `1` = "Treatment"))


 # write.csv(summary_data,"data/02_data_extraction/extracted_raw_data/GNM_001_rep_table3_estimates.csv")
