##############################################################
# Description of script and Instructions
##############################################################

# This script is to:
# 1. import the unique reference list after de-duplication
# 2. Adding a unique identified (Paper_ID) for each article
# 3. Creating subsets for title and abstract screening for the 
# three screeners (SD, JS and TR) for preliminary and final phase of screening

# Title and abstract screening was conducted using revtools package- screen_abstracts()

# Packages used: dplyr , revtools , readr

# created on 04.10.22 for green nest material project
# Code written by: SD and revised by: 

##############################################################
# Packages needed
##############################################################

# load pacakges
pacman::p_load(dplyr, revtools, readr)

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
screen_abstracts(prelim_screen_set1)
prelim_screen_set2=unique_reference_list[random_ID1[21:40],]
screen_abstracts(prelim_screen_set2)

##############################################################
# Rest of the abstract screening performed by SD, JS and TR 
# Each paper screened twice
##############################################################

# To create three sets with all the papers that have not been screened yet

##############################################################

#screened by SD and TR
random_ID2=sample(setdiff(1:nrow(unique_reference_list), random_ID1), 115)
abstract_screening_set1=unique_reference_list[random_ID2,]
screen_abstracts(abstract_screening_set1)

##############################################################

#screened by TR and JS
random_ID3=sample(setdiff(1:nrow(unique_reference_list), c(random_ID1,random_ID2)), 114)
abstract_screening_set2=unique_reference_list[random_ID3,]
screen_abstracts(abstract_screening_set2)

# write.csv(abstract_screening_set2,"data/01_systematic_search/03_title_and_abstract_screening/abstract_screening_set2.csv",row.names=FALSE)
##############################################################

#screened by SD and JS
random_ID4= c(random_ID1,random_ID2,random_ID3)
abstract_screening_set3=unique_reference_list[-random_ID4,]
screen_abstracts(abstract_screening_set3)

# write.csv(abstract_screening_set3,"data/01_systematic_search/03_title_and_abstract_screening/abstract_screening_set3.csv",row.names=FALSE)



