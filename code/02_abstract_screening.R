##############################################################
# Description of script and Instructions ---------------------
##############################################################

# This script is to:
# 1. import the unique reference list after de-duplication
# 2. Adding a unique identified (Paper_ID) for each article
# 3. Creating subsets for title and abstract screening for the 
# three screeners (SD, JS and TR) for preliminary and final phase of screening

# Second part of this script was created to import the files after 
# title and abstract screening to..

# 4. import the files after title and abstract screening (after resolving any differences)
# 5. Creating a final dataset_after_abstract_screening with only final decisions
# 6. Creating a file with selected_abstract_screening for further steps

# INPUT files required
# 
# differences_prelim_set1_2022_10_07
# differences_prelim_set2_2022_10_14
# differences_abstract_screening_set1
# differences_abstract_screening_set2
# differences_abstract_screening_set3

# Title and abstract screening was conducted using revtools package- screen_abstracts()

# Packages used: 
# dplyr , 
# revtools , 
# readr, 
# tidyverse, 
# stringr


# first created on 04.10.22 for green nest material project
# Code written by: SD and revised by: MO [2025/08/24]

##############################################################
##############################################################
# Before title and abstract screening was performed
##############################################################
##############################################################

# Required packages ------------------------------------------

# Required packages are load and managed through pacman
# Install package pacman if not installed already
if (!"pacman" %in% rownames(installed.packages())) {
  install.packages("pacman")
}

# load packages or install if not found
pacman::p_load(dplyr, #v1.1.4
               revtools, #v0.4.1
               readr, #v2.1.5
               tidyverse, #v2.0.0
               stringr #v1.5.1
               )
# set seed for reproducibility -------------------------------
# (ensures consistent random number generation)
set.seed(1)

# Importing data after de-duplication -------------------------
# table containing 383 papers
unique_reference_list <- read.table(
  file = "data/01_systematic_search/02_reference_data/unique_reference_list.csv",
  header = T, 
  sep = ",")

# Giving individual paper ID for screening
# in the form GNM_001 to GNM_383
Paper_ID <- sprintf("GNM_%03d", 1:nrow(unique_reference_list))

# ad Paper_ID column to data frame 
unique_reference_list <- cbind(Paper_ID,unique_reference_list)

# Abstract screening subsets for multiple screenings ---------

# Multiple abstract screening (performed by SD, JS and TR) 
# To randomly select 40 papers (~10%) in two sub-sets
# Abstract screening performed using revtools package
# function screen_abstracts opens a GUI as shiny app

# randomly select 40 references out of all 383
random_ID1 <- sample(1:nrow(unique_reference_list), 40)

# assign 20 selected references to set1 and set2 respectively

prelim_screen_set1 <- unique_reference_list[random_ID1[1:20],]
# screen_abstracts(prelim_screen_set1)

prelim_screen_set2 <- unique_reference_list[random_ID1[21:40],]
# screen_abstracts(prelim_screen_set2)

# Please uncomment the line with screen_abstracts() if you want to 
# open the GUI to do the screening yourself..
# To see the results of the abstract screening conducted for this project,
# see below..

##############################################################
# Rest of the abstract screening performed by SD, JS and TR 
# Each paper screened twice
##############################################################

# Create three sets with all the papers that have not been screened yet ----
# each set comprises 114 or 115 papers

# screened by SD and TR --------------------------------------
random_ID2 <- sample(setdiff(1:nrow(unique_reference_list), random_ID1), 115)
abstract_screening_set1 <- unique_reference_list[random_ID2,]
# screen_abstracts(abstract_screening_set1)

# screened by TR and JS --------------------------------------
random_ID3 <- sample(setdiff(1:nrow(unique_reference_list), c(random_ID1,random_ID2)), 114)
abstract_screening_set2 <- unique_reference_list[random_ID3,]
# screen_abstracts(abstract_screening_set2)

## export to csv file (already saved as part of this repository)
# write.csv(abstract_screening_set2,"data/01_systematic_search/03_title_and_abstract_screening/abstract_screening_set2.csv",row.names=FALSE)
##############################################################

#screened by SD and JS
random_ID4 <- c(random_ID1,random_ID2,random_ID3)
abstract_screening_set3 <- unique_reference_list[-random_ID4,]
# screen_abstracts(abstract_screening_set3)

## export to csv file (already saved as part of this repository)
# write.csv(abstract_screening_set3,"data/01_systematic_search/03_title_and_abstract_screening/abstract_screening_set3.csv",row.names=FALSE)


##############################################################
##############################################################
# To merge three sets of title and abstract screening after 
# screening was performed
##############################################################
##############################################################


## Load datasets after screening was performed ---------------

differences_prelim_set1_2022_10_07 <- read_delim(
  file = "data/01_systematic_search/03_title_and_abstract_screening/differences_prelim_set1_2022_10_07.csv", 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE)

differences_prelim_set2_2022_10_14 <- read_delim(
  file = "data/01_systematic_search/03_title_and_abstract_screening/differences_prelim_set2_2022_10_14.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE)

differences_abstract_screening_set1 <- read_delim(
  file = "data/01_systematic_search/03_title_and_abstract_screening/differences_abstract_screening_set1.csv", 
  delim = ";",
  escape_double = FALSE, 
  trim_ws = TRUE)

differences_abstract_screening_set2 <- read_delim(
  file = "data/01_systematic_search/03_title_and_abstract_screening/differences_abstract_screening_set2.csv", 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE)

differences_abstract_screening_set3 <- read_delim(
  file = "data/01_systematic_search/03_title_and_abstract_screening/differences_abstract_screening_set3.csv",
  delim = ";",
  escape_double = FALSE, 
  trim_ws = TRUE)

# To keep only the final decisions after the discussions between screeners ----
differences_prelim_set1_2022_10_07 <- differences_prelim_set1_2022_10_07 %>% 
  select(1:13,screened_final,notes_final)

differences_prelim_set2_2022_10_14=differences_prelim_set2_2022_10_14 %>%
  select(1:13,screened_final,notes_final)

differences_abstract_screening_set1=differences_abstract_screening_set1 %>% 
  select(1:13,screened_final,notes_final)

differences_abstract_screening_set2=differences_abstract_screening_set2 %>% 
  select(1:13,screened_final,notes_final)

differences_abstract_screening_set3=differences_abstract_screening_set3 %>% 
  select(1:13,screened_final,notes_final)

## Final merged data from abstract screening -----------------
dataset_after_abstract_screening <- rbind(
  differences_prelim_set1_2022_10_07, 
  differences_prelim_set2_2022_10_14,
  differences_abstract_screening_set1,
  differences_abstract_screening_set2,
  differences_abstract_screening_set3)

# To remove any rows with NAs
dataset_after_abstract_screening <- dataset_after_abstract_screening %>% 
  filter_all(any_vars(!is.na(.)))

## standardise strings used to denote exclusion reasons ------
#  make sure all exclusion reasons are named similarly 
#  (spelling differences in how different screeners entered the reasons)

dataset_after_abstract_screening$notes_final <- str_replace_all(
  dataset_after_abstract_screening$notes_final, 
  c("Ouf of field" = "Out of field", 
    "Out of field \r\n" = "Out of field", 
    "No birds" = "Not birds",
    "Not birds\n" = "Not birds",
    "Not birds\r\n" = "Not birds",
    "Not birds \r\n" = "Not birds",
    "No green nest materials?" = "No green nest material",
    "No green nest material\r\n" = "No green nest material",
    "No green nest materials" = "No green nest material",
    "Not experimental\r\n" = "Not experimental",
    "Observational" = "Not experimental"))

## Spotted duplicates manually -------------------------------
# GNM_218 is a duplicate of GNM_010, GNM_198 is a duplicate of GNM_050
# GNM_184 is a duplicate of GNM_359, GNM_075 is a duplicate of GNM_239
# GNM_210 is a duplicate of GNM_056, GNM_076 is a duplicate of GNM_233
# GNM_280 is a duplicate of GNM_111, GNM_183 is a duplicate of GNM_351
# GNM_179 is a duplicate of GNM_339, GNM_241 is a duplicate of GNM_080
# GNM_209 is a duplicate of GNM_057, GNM_320 is a duplicate of GNM_155
# GNM_202 is a duplicate of GNM_008, GNM_186 is a duplicate of GNM_043
# GNM_326 is a duplicate of GNM_170, GNM_283 is a duplicate of GNM_114
# GNM_191 is a duplicate of GNM_047

# To exclude the spotted duplicates and assign "duplicate" as the reason for exclusion
duplicated_ID <- c("GNM_218","GNM_198","GNM_184","GNM_075","GNM_210","GNM_076",
                   "GNM_280","GNM_183","GNM_179","GNM_241","GNM_209","GNM_320",
                   "GNM_202","GNM_186","GNM_326","GNM_283","GNM_191")

dataset_after_abstract_screening <- dataset_after_abstract_screening %>% 
  mutate(notes_final = if_else(paper_id %in% duplicated_ID, "duplicate", notes_final),
         screened_final = if_else(paper_id %in% duplicated_ID, "excluded", screened_final))

## export to csv file (already saved as part of this repository)
# write.csv(dataset_after_abstract_screening,"data/01_systematic_search/03_title_and_abstract_screening/complete_dataset_after_abstract_screening.csv",row.names=FALSE)

## Summarise screening decisions to exclude articles ---------
excluded_abstract_screening <- dataset_after_abstract_screening %>% 
  filter(screened_final == "excluded")

excluded_abstract_screening$notes_final = as.factor(excluded_abstract_screening$notes_final)
summary(excluded_abstract_screening$notes_final)

## retrieve selected abstacted (n=40) ------------------------
selected_abstract_screening <- dataset_after_abstract_screening %>% 
  filter(screened_final == "selected")

## export to csv file (already saved as part of this repository)
# write.csv(selected_abstract_screening,"data/01_systematic_search/03_title_and_abstract_screening/selected_abstract_screening.csv",row.names=FALSE)

# ----------------------- Session info -----------------------
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
#   [1] shiny_1.11.1    lubridate_1.9.4 forcats_1.0.0   stringr_1.5.1   purrr_1.1.0     tidyr_1.3.1     tibble_3.3.0    ggplot2_3.5.2  
# [9] tidyverse_2.0.0 readr_2.1.5     revtools_0.4.1  dplyr_1.1.4    
# 
# loaded via a namespace (and not attached):
#   [1] stringdist_0.9.15    gtable_0.3.6         bslib_0.9.0          htmlwidgets_1.6.4    tzdb_0.5.0           vctrs_0.6.5         
# [7] tools_4.4.1          generics_0.1.4       stats4_4.4.1         parallel_4.4.1       pacman_0.5.1         pkgconfig_2.0.3     
# [13] data.table_1.17.8    RColorBrewer_1.1-3   lifecycle_1.0.4      compiler_4.4.1       farver_2.1.2         textshaping_1.0.1   
# [19] fontawesome_0.5.3    httpuv_1.6.16        htmltools_0.5.8.1    sass_0.4.10          lazyeval_0.2.2       plotly_4.11.0       
# [25] crayon_1.5.3         later_1.4.3          pillar_1.11.0        jquerylib_0.1.4      MASS_7.3-60.2        cachem_1.1.0        
# [31] mime_0.13            tidyselect_1.2.1     digest_0.6.37        stringi_1.8.7        slam_0.1-55          topicmodels_0.2-17  
# [37] ade4_1.7-23          fastmap_1.2.0        grid_4.4.1           cli_3.6.5            magrittr_2.0.3       withr_3.0.2         
# [43] scales_1.4.0         promises_1.3.3       bit64_4.6.0-1        timechange_0.3.0     httr_1.4.7           bit_4.6.0           
# [49] ragg_1.4.0           modeltools_0.2-24    hms_1.1.3            NLP_0.3-2            memoise_2.0.1        viridisLite_0.4.2   
# [55] tm_0.7-16            rlang_1.1.6          Rcpp_1.1.0           xtable_1.8-4         glue_1.8.0           xml2_1.4.0          
# [61] shinydashboard_0.7.3 vroom_1.6.5          rstudioapi_0.16.0    jsonlite_2.0.0       R6_2.6.1             systemfonts_1.2.3 