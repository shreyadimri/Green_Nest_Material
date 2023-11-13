##############################################################
# Description of script and Instructions
##############################################################

# This script is to:
# 1. import the .bib files from Web of Science (WOS) and Scopus
# 2. combine the 2 .bib files to produce a full reference list (encoding errors may occur depending 
# on the system and "UTF-16LE" was used in this case for writing and reading the full_reference_before_deduplication)
# 3. deduplicate the full reference list and produce a unique list of 
# papers

# INPUT files: 
# GNM_wos.bib
# GNM_scopus.bib

# OUTPUT files:
# full_reference_before_deduplication.csv
# unique_reference_list.csv
# Packages used: dplyr , revtools


# created on 12.09.22 for green nest material project 
# Code written by: SD and revised by: NA

##############################################################
# Packages needed
##############################################################

# load pacakges
# If pacman is not installed already use
# install.packages("pacman")
pacman::p_load(dplyr, revtools)

# cleaning up
rm(list=ls())


################################

# importing the .bib files from WEB OF SCIENCE and SCOPUS

reference_data_wos <- read_bibliography("data/01_systematic_search/02_reference_data/web_of_science/GNM_wos.bib")
reference_data_scopus <- read_bibliography("data/01_systematic_search/02_reference_data/scopus/GNM_scopus.bib")

# reducing fields to the minimum number of fields
# so that all databases have the same columns. Also, these fields
# are the important ones for the screening.
reducing_fields <- c("label","title","author","journal",
                         "volume","number","pages","year",
                         "doi","abstract") 


reference_data_wos_reduced <- reference_data_wos[,reducing_fields]
reference_data_scopus_reduced <- reference_data_scopus[,reducing_fields]

##############################################################
# Full reference list without de-duplication
##############################################################

# building the full reference list before de-duplication
full_reference_data <- rbind(reference_data_wos_reduced,
                             reference_data_scopus_reduced)

#write.csv(full_reference_data,"data/01_systematic_search/02_reference_data/full_reference_before_deduplication.csv",row.names=FALSE, fileEncoding = "UTF-16LE")
#full_reference_data <- read.table("data/01_systematic_search/02_reference_data/full_reference_before_deduplication.csv",
#                           header=T,sep=",",fileEncoding = "UTF-16LE")


##############################################################
# Full reference data: after de-duplication
##############################################################

#searching duplicates using revtools: 

#using fuzzy matching with threshold default(0.1) = 383, 0.2= 379 
#we decided to go with fuzzy match at default of 0.1 to be on the conservative side 
#and not lose any papers that might not be duplicates
search_duplicated <- find_duplicates(data = full_reference_data,
match_variable = "title",
group_variable = NULL,
match_function = "fuzzdist",
method = "fuzz_m_ratio",
remove_punctuation = T) 
# extracting duplicates
unique_reference_list <- extract_unique_references(full_reference_data,search_duplicated)


#write.csv(unique_reference_list,"data/01_systematic_search/02_reference_data/unique_reference_list.csv",row.names=FALSE)
#unique_reference_list <- read.table("data/01_systematic_search/02_reference_data/unique_reference_list.csv",
#                                 header=T,sep=",")

################################################################