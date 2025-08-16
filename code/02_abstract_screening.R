##############################################################
# Description of script and Instructions
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

# Packages used: dplyr , revtools , readr, tidyverse, stringr


# first created on 04.10.22 for green nest material project
# Code written by: SD and revised by: NA

##############################################################
##############################################################
# Before title and abstract screening was performed
##############################################################
##############################################################

##############################################################
# Packages needed
##############################################################

# load pacakges
pacman::p_load(dplyr, revtools, readr, 
               tidyverse, stringr)

# cleaning up
rm(list=ls())

# set seed for reproducibility (for consistent random number generation)
set.seed(1)

############################################################
# Importing data after de-duplication
############################################################

unique_reference_list <- read.table("data/01_systematic_search/02_reference_data/unique_reference_list.csv",
                                    header=T,sep=",")

# Giving individual paper ID for screening

Paper_ID=sprintf("GNM_%03d", 1:nrow(unique_reference_list))
unique_reference_list=cbind(Paper_ID,unique_reference_list)


##################################################################
# Abstract screening subsets for multiple screenings 
##################################################################

##############################################################
# Multiple abstract screening (performed by SD, JS and TR) 
# To randomly select 40 papers (~10%) in two sub-sets
# Abstract screening performed using revtools package
##############################################################

random_ID1<-sample(1:nrow(unique_reference_list),40)
prelim_screen_set1=unique_reference_list[random_ID1[1:20],]
# screen_abstracts(prelim_screen_set1)
prelim_screen_set2=unique_reference_list[random_ID1[21:40],]
# screen_abstracts(prelim_screen_set2)

# Please uncomment the line with screen_abstracts() if you want to 
# open the GUI to do the screening yourself..
# To see the results of the abstract screening conducted for this project,
# see below..


##############################################################
# Rest of the abstract screening performed by SD, JS and TR 
# Each paper screened twice
##############################################################

# To create three sets with all the papers that have not been screened yet

##############################################################

#screened by SD and TR
random_ID2=sample(setdiff(1:nrow(unique_reference_list), random_ID1), 115)
abstract_screening_set1=unique_reference_list[random_ID2,]
# screen_abstracts(abstract_screening_set1)

##############################################################

#screened by TR and JS
random_ID3=sample(setdiff(1:nrow(unique_reference_list), c(random_ID1,random_ID2)), 114)
abstract_screening_set2=unique_reference_list[random_ID3,]
# screen_abstracts(abstract_screening_set2)

# write.csv(abstract_screening_set2,"data/01_systematic_search/03_title_and_abstract_screening/abstract_screening_set2.csv",row.names=FALSE)
##############################################################

#screened by SD and JS
random_ID4= c(random_ID1,random_ID2,random_ID3)
abstract_screening_set3=unique_reference_list[-random_ID4,]
# screen_abstracts(abstract_screening_set3)

# write.csv(abstract_screening_set3,"data/01_systematic_search/03_title_and_abstract_screening/abstract_screening_set3.csv",row.names=FALSE)


##############################################################
##############################################################
# To merge three sets of title and abstract screening after 
# screening was performed
##############################################################
##############################################################


## Load datasets after screening

differences_prelim_set1_2022_10_07 <- read_delim("data/01_systematic_search/03_title_and_abstract_screening/differences_prelim_set1_2022_10_07.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
differences_prelim_set2_2022_10_14 <- read_delim("data/01_systematic_search/03_title_and_abstract_screening/differences_prelim_set2_2022_10_14.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
differences_abstract_screening_set1 <- read_delim("data/01_systematic_search/03_title_and_abstract_screening/differences_abstract_screening_set1.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
differences_abstract_screening_set2 <- read_delim("data/01_systematic_search/03_title_and_abstract_screening/differences_abstract_screening_set2.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
differences_abstract_screening_set3 <- read_delim("data/01_systematic_search/03_title_and_abstract_screening/differences_abstract_screening_set3.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# To keep only the final decisions after the discussions between screeners 

differences_prelim_set1_2022_10_07=differences_prelim_set1_2022_10_07%>%select(1:13,screened_final,notes_final)
differences_prelim_set2_2022_10_14=differences_prelim_set2_2022_10_14%>%select(1:13,screened_final,notes_final)
differences_abstract_screening_set1=differences_abstract_screening_set1%>%select(1:13,screened_final,notes_final)
differences_abstract_screening_set2=differences_abstract_screening_set2%>%select(1:13,screened_final,notes_final)
differences_abstract_screening_set3=differences_abstract_screening_set3%>%select(1:13,screened_final,notes_final)



##Final merged data from Abstract Screening
dataset_after_abstract_screening <- rbind(differences_prelim_set1_2022_10_07,differences_prelim_set2_2022_10_14,
                                          differences_abstract_screening_set1 ,differences_abstract_screening_set2 ,
                                          differences_abstract_screening_set3 )

# To remove any rows with NAs
dataset_after_abstract_screening<-dataset_after_abstract_screening%>%filter_all(any_vars(!is.na(.)))


# To make sure all exclusion reasons are named similarly (spelling differences in how different screeners entered the reasons)

dataset_after_abstract_screening$notes_final <- str_replace_all(dataset_after_abstract_screening$notes_final, 
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

## Spotted duplicates manually
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
duplicated_ID=c("GNM_218","GNM_198","GNM_184","GNM_075","GNM_210","GNM_076",
                "GNM_280","GNM_183","GNM_179","GNM_241","GNM_209","GNM_320",
                "GNM_202","GNM_186","GNM_326","GNM_283","GNM_191")

dataset_after_abstract_screening <- dataset_after_abstract_screening %>% 
  mutate(notes_final = if_else(paper_id %in% duplicated_ID, "duplicate", notes_final),
         screened_final = if_else(paper_id %in% duplicated_ID, "excluded", screened_final))


# write.csv(dataset_after_abstract_screening,"data/01_systematic_search/03_title_and_abstract_screening/complete_dataset_after_abstract_screening.csv",row.names=FALSE)


excluded_abstract_screening <- dataset_after_abstract_screening%>%filter(screened_final=="excluded")

excluded_abstract_screening$notes_final=as.factor(excluded_abstract_screening$notes_final)
summary(excluded_abstract_screening$notes_final)

selected_abstract_screening <- dataset_after_abstract_screening%>%filter(screened_final=="selected")

# write.csv(selected_abstract_screening,"data/01_systematic_search/03_title_and_abstract_screening/selected_abstract_screening.csv",row.names=FALSE)

#################################################################


