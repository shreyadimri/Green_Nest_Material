# Description of script and Instructions ---------------------
##############################################################
# This script is to:
# 1. import the selected_abstract_screening
# 2. Creating subsets for data-extraction for the 
# three screeners (SD, JS and TR) for preliminary and final phase of extraction

# Packages used: readr

# created on 09.08.23 for green nest material project
# Code written by: SD and revised by: MO [2025/08/24]

##############################################################
##############################################################
# For creating the dataset for full-text screening 
# and data-extraction. Full-text screening and data-extraction
# were performed together in Microsoft Excel
##############################################################
##############################################################

# Required packages ------------------------------------------

# Required packages are load and managed through pacman
# Install package pacman if not installed already
if (!"pacman" %in% rownames(installed.packages())) {
  install.packages("pacman")
}

# Load required packages
pacman::p_load(
  here, #v1.0.1
  readr, #v2.1.5
  readxl, #v1.4.5
  tidyverse #v2.0.0
)

# set seed for reproducibility -------------------------------
# (ensures consistent random number generation)
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

random_ID1 <- sample(1:nrow(selected_abstract_screening),5)
prelim_extraction <- selected_abstract_screening[random_ID1,]

## export to csv file (already saved as part of this repository)
# write.csv(prelim_extraction,"data/02_data_extraction/prelim_extraction.csv",row.names=FALSE)

##############################################################
# Rest of the data-extraction performed by SD, JS and TR 
# Each data-extraction is performed twice by two data-extractors 
##############################################################

# To create three sets with all the records for which the data 
# extraction should be performed

##############################################################

#screened by SD and checked by TR ----------------------------
random_ID2 <- sample(setdiff(1:nrow(selected_abstract_screening), random_ID1), 12)
data_extraction_setSD <- selected_abstract_screening[random_ID2,]

## export to csv file (already saved as part of this repository)
# write.csv(data_extraction_setSD,"data/02_data_extraction/data_extraction_setSD.csv",row.names=FALSE)
##############################################################

#screened by TR and checked by JS ----------------------------
random_ID3 <- sample(setdiff(1:nrow(selected_abstract_screening), c(random_ID1,random_ID2)), 12)
data_extraction_setTR <- selected_abstract_screening[random_ID3,]

## export to csv file (already saved as part of this repository)
# write.csv(data_extraction_setTR,"data/02_data_extraction/data_extraction_setTR.csv",row.names=FALSE)
##############################################################

#screened by JS and checked by SD ----------------------------
random_ID4 <- c(random_ID1,random_ID2,random_ID3)
data_extraction_setJS <- selected_abstract_screening[-random_ID4,]

## export to csv file (already saved as part of this repository)
# write.csv(data_extraction_setJS,"data/02_data_extraction/data_extraction_setJS.csv",row.names=FALSE)
##############################################################


##############################################################
##############################################################
# Once the screening was done by SD, JS, TR.. in excel
# We combine the dataset into a single file..
##############################################################
##############################################################

## We use column_types to make the loading variable datatype of all the data files same.

column_types = c(
  "text", "text", "text", "text", "text", "numeric", 
  "text", "text", "text", "text", "text", "text", 
  "text", "text", "text", "text", "numeric", "text", 
  "numeric", "text", "numeric", "numeric", "text", "numeric", 
  "text", "numeric", "text", "text", "text", "text", 
  "numeric", "text", "text", "numeric", "numeric", "numeric", 
  "numeric", "numeric", "text", "text", "text", "text", 
  "text", "text", "text", "text", "text", "text", 
  "numeric", "numeric")


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
excluded_fulltext <- after_data_extraction %>%
  filter(fulltext_screening != "included")

# What were the reasons for exclusion?
excluded_fulltext$fulltext_notes=as.factor(excluded_fulltext$fulltext_notes)
summary(excluded_fulltext$fulltext_notes)

# # ----------------------- Session info -----------------------
# R version 4.4.1 (2024-06-14 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 26100)
# 
# Matrix products: default
# 
# 
# locale:
#   [1] LC_COLLATE=English_Europe.utf8  LC_CTYPE=English_Europe.utf8    LC_MONETARY=English_Europe.utf8 LC_NUMERIC=C                   
# [5] LC_TIME=English_Europe.utf8    
# 
# time zone: Europe/Berlin
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] lubridate_1.9.4 forcats_1.0.0   stringr_1.5.1   dplyr_1.1.4     purrr_1.1.0     tidyr_1.3.1     tibble_3.3.0    ggplot2_3.5.2  
# [9] tidyverse_2.0.0 here_1.0.1      readxl_1.4.5    readr_2.1.5    
# 
# loaded via a namespace (and not attached):
#   [1] bit_4.6.0          gtable_0.3.6       compiler_4.4.1     crayon_1.5.3       tidyselect_1.2.1   parallel_4.4.1     scales_1.4.0      
# [8] R6_2.6.1           generics_0.1.4     rprojroot_2.1.0    pillar_1.11.0      RColorBrewer_1.1-3 tzdb_0.5.0         rlang_1.1.6       
# [15] stringi_1.8.7      bit64_4.6.0-1      timechange_0.3.0   cli_3.6.5          withr_3.0.2        magrittr_2.0.3     grid_4.4.1        
# [22] vroom_1.6.5        rstudioapi_0.16.0  hms_1.1.3          lifecycle_1.0.4    vctrs_0.6.5        glue_1.8.0         farver_2.1.2      
# [29] cellranger_1.1.0   pacman_0.5.1       tools_4.4.1        pkgconfig_2.0.3   