##############################################################
# Description of script and Instructions
##############################################################

# This script is to:
# 1. import the .bib files from Web of Science (WOS) and Scopus for the repeated search on 12th August 2024
# 2. combine the 2 .bib files (I have used one .csv for scopus since .bib was giving encoding errors while loading) 
# to produce a full reference list (encoding errors may occur depending 
# on the system and "UTF-16LE" was used in this case for writing and reading the full_reference_before_deduplication)
# 3. deduplicate the full reference list and produce a unique list of 
# papers (unique_reference_repeat_120824)
# 4. Adding a unique identified (Paper_ID) for each article for this section
# 5. 

# INPUT files: 
# GNM_wos.bib
# GNM_scopus.bib

# OUTPUT files:
# full_reference_before_deduplication.csv
# unique_reference_list.csv
# Packages used: dplyr , revtools


# created on 12.08.24 for green nest material project 
# Code written by: SD and revised by: NA

##############################################################
# Packages needed
##############################################################

# load pacakges
# If pacman is not installed already use
# install.packages("pacman")
pacman::p_load(dplyr, revtools, readr)

# cleaning up
rm(list=ls())


# set seed for reproducibility (for consistent random number generation)
set.seed(7)

################################

# importing the .bib files from WEB OF SCIENCE and SCOPUS

reference_data_wos <- read_bibliography("data/01_systematic_search/04_search_repeat_12082024/GNM_repeat_wos.bib")
reference_data_scopus <- read_bibliography("data/01_systematic_search/04_search_repeat_12082024/GNM_repeat_scopus.csv")

# reducing fields to the minimum number of fields
# so that all databases have the same columns. Also, these fields
# are the important ones for the screening.
reducing_fields <- c("label","title","author","journal",
                     "volume","number","pages","year",
                     "doi","abstract") 

## Since I decided to use .csv for scopus, the column names are different and 
# I will have to edit it.

reference_data_scopus <- reference_data_scopus%>%rename(journal=source_title,
                                                        number=issue,
                                                        doi=DOI)%>%
  mutate(pages=paste(reference_data_scopus$page_start,reference_data_scopus$page_end,sep = "-"))
  


reference_data_wos_reduced <- reference_data_wos[,reducing_fields]
reference_data_scopus_reduced <- reference_data_scopus[,reducing_fields]

##############################################################
# Full reference list without de-duplication
##############################################################

# building the full reference list before de-duplication
full_reference_repeat_12082024 <- rbind(reference_data_wos_reduced,
                             reference_data_scopus_reduced)

# write.csv(full_reference_repeat_12082024,"data/01_systematic_search/04_search_repeat_12082024/full_reference_repeat_12082024.csv",row.names=FALSE, fileEncoding = "UTF-16LE")
# full_reference_repeat_120824 <- read.table("data/01_systematic_search/04_search_repeat_12082024/full_reference_repeat_12082024.csv",
#                           header=T,sep=",",fileEncoding = "UTF-16LE")

##############################################################
# Full reference data: after de-duplication
##############################################################

#searching duplicates using revtools: 

#we decided to go with fuzzy match at default of 0.1 to remain consistent with 
# what we had done for the main deduplication before
search_duplicated <- find_duplicates(data = full_reference_repeat_12082024,
                                     match_variable = "title",
                                     group_variable = NULL,
                                     match_function = "fuzzdist",
                                     method = "fuzz_m_ratio",
                                     remove_punctuation = T) 
# extracting duplicates
unique_reference_repeat_12082024 <- extract_unique_references(full_reference_repeat_12082024,search_duplicated)

# manually identified some duplicates by quick look at the list, filtering them out
unique_reference_repeat_12082024<-unique_reference_repeat_12082024%>%
  filter(label!="ref_45",
         label!="ref_40",
         label!="ref_06")



## Adding a unique identifier to the papers
Paper_ID=sprintf("GNM_%03d_rep", 1:nrow(unique_reference_repeat_12082024))
unique_reference_repeat_12082024=cbind(Paper_ID,unique_reference_repeat_12082024)

## Screening done by SD
unique_reference_repeat_12082024$screener_ID="SD"

# write.csv(unique_reference_repeat_12082024,"data/01_systematic_search/04_search_repeat_12082024/unique_reference_repeat_12082024.csv",row.names=FALSE)
# unique_reference_repeat_12082024 <- read.table("data/01_systematic_search/04_search_repeat_12082024/unique_reference_repeat_12082024.csv",
                                 # header=T,sep=",")


##############################################################
# Abstract screening done by SD
##############################################################

screen_abstracts(unique_reference_repeat_12082024)


##############################################################
# loading dataset saved after abstract screening
##############################################################

completed_abstract_screening_repeat_12082024 <- read_csv("data/01_systematic_search/04_search_repeat_12082024/completed_abstract_screening_repeat_12082024.csv")


## Dataset that was excluded
excluded_abstract_screening_repeat <- completed_abstract_screening_repeat_12082024%>%filter(screened_abstracts=="excluded")

excluded_abstract_screening_repeat$notes=as.factor(excluded_abstract_screening_repeat$notes)
summary(excluded_abstract_screening_repeat$notes)
## Additional numbers for PRISMA


selected_abstract_screening_repeat <- completed_abstract_screening_repeat_12082024%>%filter(screened_abstracts=="selected")

##############################################################
# Adding additional studies that we identified that could fit our meta-analysis
##############################################################

reference_data_additional <- read_bibliography("data/01_systematic_search/04_search_repeat_12082024/GNM_additional_studies.bib")

reference_data_additional_reduced <- reference_data_additional[,reducing_fields]

## Adding a unique identifier to the papers


Paper_ID=sprintf("GNM_%03d_add", 1:nrow(reference_data_additional_reduced))
reference_data_additional_reduced=cbind(Paper_ID,reference_data_additional_reduced)


## Changing volume to double instead of characters to combine with selected_abstract_screening_repeat
reference_data_additional_reduced$volume=as.double(reference_data_additional_reduced$volume)
reference_data_additional_reduced$year=as.double(reference_data_additional_reduced$year)

selected_abstract_screening_repeat <- selected_abstract_screening_repeat%>%bind_rows(reference_data_additional_reduced)
# write.csv(selected_abstract_screening_repeat,"data/01_systematic_search/04_search_repeat_12082024/selected_abstract_screening_repeat.csv",row.names=FALSE)


