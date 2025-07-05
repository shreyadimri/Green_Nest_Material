##############################################################
# Description of script and Instructions
##############################################################
# This script is to:
# 1. import the selected_abstract_screening
# 2. Creating subsets for data-extraction for the 
# three screeners (SD, JS and TR) for preliminary and final phase of extraction



# Packages used: readr

# created on 09.08.23 for green nest material project
# Code written by: SD and revised by: NA

##############################################################
##############################################################
# For creating the dataset for full-text screening 
# and data-extraction. Full-text screening and data-extraction
# were performed together in Microsoft Excel
##############################################################
##############################################################

##############################################################
# Packages needed
##############################################################

# Load required packages
# install.packages("pacman")
pacman::p_load(readr, readxls)

# environment clean up
rm(list=ls())

# set seed for reproducibility (essential for consistency of random number generation)
set.seed(1)

##############################################################
# To load selected papers dataset after the abstract screening
##############################################################

selected_abstract_screening <- read_csv("data/01_systematic_search/03_title_and_abstract_screening/selected_abstract_screening.csv")

##################################################################
# Data-extraction subsets for multiple screenings 
##################################################################

##############################################################
# Data-extraction (performed by SD, JS and TR) 
# To randomly select 5 papers (~12.5%) for pilot data-extraction
# Data-extraction performed on excel
##############################################################

random_ID1<-sample(1:nrow(selected_abstract_screening),5)
prelim_extraction=selected_abstract_screening[random_ID1,]

# write.csv(prelim_extraction,"data/02_data_extraction/prelim_extraction.csv",row.names=FALSE)

##############################################################
# Rest of the data-extraction performed by SD, JS and TR 
# Each data-extraction is performed twice by two data-extractors 
##############################################################

# To create three sets with all the records for which the data 
# extraction should be performed

##############################################################

#screened by SD and checked by TR
random_ID2=sample(setdiff(1:nrow(selected_abstract_screening), random_ID1), 12)
data_extraction_setSD=selected_abstract_screening[random_ID2,]

# write.csv(data_extraction_setSD,"data/02_data_extraction/data_extraction_setSD.csv",row.names=FALSE)
##############################################################

#screened by TR and checked by JS
random_ID3=sample(setdiff(1:nrow(selected_abstract_screening), c(random_ID1,random_ID2)), 12)
data_extraction_setTR=selected_abstract_screening[random_ID3,]

# write.csv(data_extraction_setTR,"data/02_data_extraction/data_extraction_setTR.csv",row.names=FALSE)
##############################################################

#screened by JS and checked by SD
random_ID4= c(random_ID1,random_ID2,random_ID3)
data_extraction_setJS=selected_abstract_screening[-random_ID4,]

# write.csv(data_extraction_setJS,"data/02_data_extraction/data_extraction_setJS.csv",row.names=FALSE)
##############################################################


##############################################################
##############################################################
# Once the screening was done by SD, JS, TR.. in excel
# We combine the dataset into a single file..
##############################################################
##############################################################

## We use column_types to make the loading variable datatype of all the datafiles same.

column_types = c(
  "text", "text", "text", "text", "text", "numeric", 
  "text", "text", "text", "text", "text", "text", 
  "text", "text", "text", "text", "numeric", "text", 
  "numeric", "text", "numeric", "numeric", "text", "numeric", 
  "text", "numeric", "text", "text", "text", "text", 
  "numeric", "text", "text", "numeric", "numeric", "numeric", 
  "numeric", "numeric", "text", "text", "text", "text", 
  "text", "text", "text", "text", "text", "text", 
  "numeric", "numeric"
)


# Data extracted from the selected papers after abstract screening

set_JMGS <- read_excel(here::here(
  "data/02_data_extraction/data_extraction_setJMGS_checkedSD.xlsx"),
  col_types =column_types)
set_SD <- read_excel(here::here(
  "data/02_data_extraction/data_extraction_setSD_checkedTR.xlsx")
  ,col_types =column_types)
set_TR <- read_excel(here::here(
  "data/02_data_extraction/data_extraction_setTR_checkedJS_checkedSD.xlsx"),
  col_types =column_types)

after_data_extraction<-full_join(set_JMGS,set_SD)%>%full_join(set_TR)

##############################################################

# How many articles were excluded during fulltext screening?
excluded_fulltext<-after_data_extraction%>%
  filter(fulltext_screening != "included")

# What were the reasons for exclusion?

excluded_fulltext$fulltext_notes=as.factor(excluded_fulltext$fulltext_notes)
summary(excluded_fulltext$fulltext_notes)
