# Description of script and Instructions ---------------------

# This script is to:
# 1. import the .bib files from Web of Science (WOS) and Scopus
# 2. combine the 2 .bib files to produce a full reference list (encoding errors may occur depending 
# on the system and "UTF-16LE" was used in this case for writing and reading the full_reference_before_deduplication)
# 3. deduplicate the full reference list and produce a unique list of 
# papers

# INPUT files: 
# GNM_wos.bib
# GNM_scopus.bib
# full_reference_before_deduplication.csv should be read to see the exact data we 
# used before de-duplication.Error in loading with revtools::read_bibliography
# function when re-checked the code now added lines of code to read bib files with synthesisr

# OUTPUT files:
# full_reference_before_deduplication.csv
# unique_reference_list.csv

# Packages used: 
# dplyr (v1.1.4)
# revtools (v0.4.1)

# created on 12.09.22 for green nest material project and updated on 02.06.2025
# Code written by: SD and revised by: MO [2025/08/18]

# Required packages ------------------------------------------

# Required packages are load and managed through pacman
# Install package pacman if not installed already
if (!"pacman" %in% rownames(installed.packages())) {
  install.packages("pacman")
}
pacman::p_load(dplyr, revtools, readr)

# importing the .bib files from WEB OF SCIENCE and SCOPUS ----

# Initially this code used revtools::read_bibliography to read the .bib files from
# WOS and Scopus. On re-running the code during check, this gives loading error. 
# A good fix is to use the synthesisr package so I am switching to that now even though 
# in the project I used revtools (and report the same)..

# Mon Aug 18 17:32:54 2025 ------------------------------
# I tested this script by running all code with a clean (empty) R library to ensure 
# all packages are installed based on current releases on CRAN:
# revtools::read_bibliography seems to work just fine, only prompting a warning message. 
# The content is equal to synthesisr::read_ref, except for minor 
# encoding differences in certain identifiers, i.e. strings denoting researcher names etc. 
# 
# Therefore, changing back to revtools::read_bibliography [MO]

# Mon Aug 18 16:27:39 2025 ------------------------------
# pacman::p_load(synthesisr)

reference_data_wos <- 
  revtools::read_bibliography("data/01_systematic_search/02_reference_data/web_of_science/GNM_wos.bib")
reference_data_scopus <- 
  revtools::read_bibliography("data/01_systematic_search/02_reference_data/scopus/GNM_scopus.bib")

# Mon Aug 18 15:43:41 2025 ------------------------------
# reference_data_wos <- synthesisr::read_ref("data/01_systematic_search/02_reference_data/web_of_science/GNM_wos.bib")
# reference_data_scopus <- synthesisr::read_ref("data/01_systematic_search/02_reference_data/scopus/GNM_scopus.bib")

# merging the .bib files from WEB OF SCIENCE and SCOPUS ------

# selecting fields required for further processing 
# and combine data frames 
reducing_fields <- c("label","title","author","journal",
                     "volume","number","pages","year",
                     "doi","abstract")

# Assemble full reference list without de-duplication --------
full_reference_data <- rbind(
  reference_data_wos[,reducing_fields],
  reference_data_scopus[,reducing_fields])

# export as csv file (saved as part of this repository)
# write.csv(x = full_reference_data, 
#           file = "data/01_systematic_search/02_reference_data/full_reference_before_deduplication.csv",
#           row.names = FALSE,
#           fileEncoding = "UTF-16LE")


# Mon Aug 18 17:36:49 2025 ------------------------------
# read csv file using readr package to circumvent some encoding issues, 
# that are potentially system-specific?

# full_reference_data <- read.table(
#   file = "data/01_systematic_search/02_reference_data/full_reference_before_deduplication.csv",
#   header = T, sep = ",", fileEncoding = "UTF-16LE")

# Read file as text with ISO-8859-1 encoding 
full_reference_data <- read_file(
  file = "data/01_systematic_search/02_reference_data/full_reference_before_deduplication.csv", 
  locale = locale(encoding = "ISO-8859-1"))

# Then parse as CSV
full_reference_data <- read_csv(I(full_reference_data), col_select = 1:10)

# convert to simple data frame
full_reference_data <- as.data.frame(unclass(full_reference_data))

# Full reference data: after de-duplication ------------------

# searching duplicates using revtools: 
# using fuzzy matching with threshold default(0.1) = 383, 0.2= 379 
# we decided to go with fuzzy match at default of 0.1 to be on the conservative side 
# and not lose any papers that might not be duplicates
search_duplicated <- revtools::find_duplicates(data = full_reference_data,
                                               match_variable = "title",
                                               group_variable = NULL,
                                               match_function = "fuzzdist",
                                               method = "fuzz_m_ratio",
                                               remove_punctuation = T) 
# extracting duplicates
unique_reference_list <- extract_unique_references(full_reference_data, search_duplicated)

# export unique_reference_list to csv file 
# write.csv(x = unique_reference_list, 
#           file = "data/01_systematic_search/02_reference_data/unique_reference_list.csv",
#           row.names=FALSE)

# read unique references from csv file saved as part of the repository. 
# Note, due to minor deviations in the encoding of strings it is possible that for 
# a given pair of duplicated (ie identical) references one or the other entry is selected. 
unique_reference_list <- 
  readr::read_csv("data/01_systematic_search/02_reference_data/unique_reference_list.csv") 

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
#   [1] readr_2.1.5    revtools_0.4.1 dplyr_1.1.4   
# 
# loaded via a namespace (and not attached):
#   [1] plotly_4.11.0        generics_0.1.4       tidyr_1.3.1          xml2_1.3.8           slam_0.1-55          hms_1.1.3           
# [7] digest_0.6.36        magrittr_2.0.3       evaluate_1.0.4       grid_4.4.1           RColorBrewer_1.1-3   fastmap_1.2.0       
# [13] jsonlite_2.0.0       tm_0.7-16            topicmodels_0.2-17   promises_1.3.3       httr_1.4.7           purrr_1.1.0         
# [19] viridisLite_0.4.2    scales_1.4.0         stringdist_0.9.15    lazyeval_0.2.2       modeltools_0.2-24    ade4_1.7-23         
# [25] shinydashboard_0.7.3 cli_3.6.3            shiny_1.11.1         crayon_1.5.3         rlang_1.1.4          bit64_4.6.0-1       
# [31] withr_3.0.2          yaml_2.3.10          tools_4.4.1          NLP_0.3-2            parallel_4.4.1       tzdb_0.5.0          
# [37] ggplot2_3.5.2        httpuv_1.6.16        pacman_0.5.1         vctrs_0.6.5          R6_2.6.1             mime_0.13           
# [43] stats4_4.4.1         lifecycle_1.0.4      bit_4.6.0            htmlwidgets_1.6.4    vroom_1.6.5          MASS_7.3-60.2       
# [49] pkgconfig_2.0.3      pillar_1.11.0        later_1.4.2          gtable_0.3.6         data.table_1.17.8    glue_1.8.0          
# [55] Rcpp_1.1.0           xfun_0.52            tibble_3.3.0         tidyselect_1.2.1     knitr_1.50           rstudioapi_0.16.0   
# [61] farver_2.1.2         xtable_1.8-4         htmltools_0.5.8.1    rmarkdown_2.29       compiler_4.4.1     
