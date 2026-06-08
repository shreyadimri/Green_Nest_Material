# ============================================================================== 
# Script: Keyword validation using litsearchr 
# Project: Green nest material systematic review and meta-analysis 
# 
# Purpose: 
# This script uses the R package litsearchr to validate the search terms used in 
# the systematic literature search. It imports an initial set of 
# known relevant references (which we call initial_library in our paper 
# File: data/01_systematic_search/01_search_strategy/own_library_references.bib), 
# extracts candidate terms from their titles, abstracts 
# and keywords, builds a keyword co-occurrence network, and identifies terms with 
# high network strength. 
# The purpose of this step is to validate the search string we designed, to 
# check whether the initial search strategy omitted any obvious additional terms.
# ==============================================================================


# ------------------------------------------------------------------------------ 
# 1. Install and load required packages 
# ------------------------------------------------------------------------------

# The remotes package is required to install litsearchr package from Github because 
# litsearchr is not currently installed from CRAN in this workflow. 
# Installation is only run if litsearchr is not already available.

if (!requireNamespace("remotes", quietly = TRUE)) 
{ install.packages("remotes") } 
if (!requireNamespace("litsearchr", quietly = TRUE)) 
{ remotes::install_github("elizagrames/litsearchr", ref = "main") }

# Load packages required for this script

pacman::p_load(litsearchr, 
               igraph, 
               ggplot2, 
               dplyr)

#---------------------------------- Naive search file import and deduplication---------------------------------------------
 
# Before using litsearchr to refine your search terms, we can perform a naive search on the topic on WOS
# The references generated from this naive search can then be used to refine the search terms using litsearchr
# In this case we used references that were present in our own library (through previous searches)

# We can import .bib, .ris or .txt files using 
# import_results function of litsearchr (which is from synthesizer package)
# The function can also be used in case the searches were performed in more the one data base...
# ...e.g.  =c("savedrecs.bib","scopus.bib"))


# In our case we use .bib file "own_library_references.bib" from our own set of references
search_directory="data/01_systematic_search/01_search_strategy"
naiveimport<- litsearchr::import_results(directory=search_directory,
                                         file = "own_library_references.bib")


#remove_duplicates function can be used to deduplicate the list based on titles 
# One can use different methods to deduplicate such as exact search or fuzzy search

# In our case, we do not need to deduplicate since we have a short list of references 
# of 15 papers that we have looked at manually already.. 
# However we perform deduplication to show as an example with exact title comparision..
deduplicated_naiveimport<- litsearchr::remove_duplicates(naiveimport, field= "title", method = "exact")

#----------------------------to extract/search for keywords for the naive search------------------------------------------

#extract_terms is used to extract the search string words from our data base
#here we look for search string words in the title, abstract and keywords of the paper
#we use the fakerake method which is the default method of litsearchr that is similar to rapid automatic keyword extraction
#min_freq is the minimum number of times these words much occur in each paper
#min_n sets the minimum phrase that search string can be.. 
#in our case it can be a single word at minimum and maximum a two phrase word

rakedkeywords<-
  litsearchr::extract_terms(text = c(deduplicated_naiveimport$title,
                                     deduplicated_naiveimport$abstract,
                                     deduplicated_naiveimport$keywords),
    method = "fakerake",
    min_freq = 1,
    ngrams = TRUE,
    min_n=1,
    max_n = 2,
    language = "English"
  )

#--------------------counting the occurance of searched keywords across studies in our database----------------------------

#create_dfm counts the occurrence of the searched keywords across the studies and produces a matrix
naive_dfm<-litsearchr::create_dfm(elements = paste(deduplicated_naiveimport$keywords,
                                                   deduplicated_naiveimport$title,
                                                   deduplicated_naiveimport$abstracts),
                                 features = rakedkeywords)



#creating the network graph showing co-occurrence between the words (note that one can change
#the numbers of studies and minimal occurence)
  naivegraph<-litsearchr:: create_network(search_dfm = naive_dfm, min_studies = 3, 
                                         min_occ = 2)

#------------------------------------plots to visualise the searched keywords----------------------------------------------

#it shows the graph with the co-occurence of words
# plot(naivegraph)

#The ‘strength’ of each term in the network is the number of other terms that it appears together with
strengths <- strength(naivegraph)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths

# term_strengths

# plot to show the strength of each term to set a cutoff and include the important terms that occur multiple times
cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

cutoff_fig

#find_cutoff defines the cutoff. i.e.changing the percentage will return more or less...
#... keywords (the highter the percentage the more keywords)
cutoff_cum <- find_cutoff(naivegraph, method="cumulative", percent=0.90)
cutoff_cum
cutoff_fig<-cutoff_fig +
  geom_hline(yintercept=cutoff_cum, linetype="dashed")


#this line simplifies the graph
 reducedgraph<- litsearchr::reduce_graph(naivegraph, 
                                         cutoff_strength = cutoff_cum)
#shows the simplified graph
plot(reducedgraph)

# searchterms <- litsearchr::get_keywords(reducedgraph)
# write.csv(searchterms, "data/01_systematic_search/01_search_strategy/litsearchr_search_terms.csv")

# ----------------------- Session info -----------------------
# This script was last run for a check during data and code review stage
# This session info is from the last run!

# R version 4.5.2 (2025-10-31)
# Platform: aarch64-apple-darwin20
# Running under: macOS Tahoe 26.3.1
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
# 
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# time zone: Europe/Berlin
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] igraph_2.3.1     litsearchr_1.0.0 lubridate_1.9.5  forcats_1.0.1    stringr_1.6.0    dplyr_1.2.1      purrr_1.2.2     
# [8] readr_2.2.0      tidyr_1.3.2      tibble_3.3.1     ggplot2_4.0.3    tidyverse_2.0.0  remotes_2.5.0   
# 
# loaded via a namespace (and not attached):
# [1] stringdist_0.9.17    gtable_0.3.6         htmlwidgets_1.6.4    processx_3.9.0       callr_3.7.6         
# [6] tzdb_0.5.0           revtools_0.4.1       vctrs_0.7.3          tools_4.5.2          generics_0.1.4      
# [11] stats4_4.5.2         curl_7.1.0           parallel_4.5.2       pacman_0.5.1         pkgconfig_2.0.3     
# [16] data.table_1.18.4    RColorBrewer_1.1-3   S7_0.2.2             desc_1.4.3           lifecycle_1.0.5     
# [21] compiler_4.5.2       farver_2.1.2         httpuv_1.6.17        htmltools_0.5.9      lazyeval_0.2.3      
# [26] plotly_4.12.0        later_1.4.8          pillar_1.11.1        crayon_1.5.3         MASS_7.3-65         
# [31] mime_0.13            synthesisr_0.4.1     stopwords_2.3        tidyselect_1.2.1     digest_0.6.39       
# [36] slam_0.1-55          stringi_1.8.7        topicmodels_0.2-17   labeling_0.4.3       ade4_1.7-24         
# [41] fastmap_1.2.0        grid_4.5.2           cli_3.6.6            magrittr_2.0.5       pkgbuild_1.4.8      
# [46] withr_3.0.2          promises_1.5.0       scales_1.4.0         bit64_4.8.2          timechange_0.4.0    
# [51] httr_1.4.8           otel_0.2.0           bit_4.6.0            ngram_3.2.3          modeltools_0.2-24   
# [56] hms_1.1.4            NLP_0.3-2            shiny_1.13.0         tm_0.7-16            viridisLite_0.4.3   
# [61] rlang_1.2.0          Rcpp_1.1.1-1.1       xtable_1.8-8         glue_1.8.1           xml2_1.5.2          
# [66] shinydashboard_0.7.3 rstudioapi_0.18.0    vroom_1.7.1          jsonlite_2.0.0       R6_2.6.1           
