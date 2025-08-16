pacman::p_load(here,tidyverse, readr)
data_GNM_010 <- read_csv("data/02_data_extraction/extracted_raw_data/data_GNM_010_clutch_overview.csv")

head(data_GNM_010)
summary(data_GNM_010)
names(data_GNM_010)

# Variables to extract: eggslaid;	chickshatchday;	maxchicks;	hatchsuccess;	massday7;	meandevoscore

#Extracting data (SD;MEAN;sample size) eggslaid

data_GNM_010 %>%
  group_by(nesttype, year) %>%
  summarise(mean=mean(na.omit(eggslaid)),
            sd=(sd(na.omit(eggslaid))),
            samplesize=(n()))

#Extracting data (SD;MEAN;sample size) chickshatchday

data_GNM_010 %>%
  group_by(nesttype, year) %>%
  summarise(mean=mean(na.omit(chickshatchday)),
            sd=(sd(na.omit(chickshatchday))),
            samplesize=(n()))

#Extracting data (SD;MEAN;sample size) maxchicks 
data_GNM_010 %>%
  group_by(nesttype, year) %>%
  summarise(mean=mean(na.omit(maxchicks)),
            sd=(sd(na.omit(maxchicks))),
            samplesize=(n()))



#Extracting data (SD;MEAN;sample size) hatchsuccess  

data_GNM_010 %>%
  group_by(nesttype, year) %>%
  summarise(mean=mean(na.omit(hatchsuccess)),
            sd=(sd(na.omit(hatchsuccess))),
            samplesize=(n()))

#Extracting data (SD;MEAN;sample size) massday7 

data_GNM_010 %>%
  group_by(nesttype, year) %>%
  summarise(mean=mean(na.omit(massday7)),
            sd=(sd(na.omit(massday7))),
            samplesize=(n()))

#Extracting data (SD;MEAN;sample size) meandevoscore 
data_GNM_010 %>%
  group_by(nesttype, year) %>%
  summarise(mean=mean(na.omit(meandevoscore)),
            sd=(sd(na.omit(meandevoscore))),
            samplesize=(n()))

