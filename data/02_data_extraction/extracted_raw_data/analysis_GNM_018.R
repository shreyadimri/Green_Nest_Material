## Setting up workspace

pacman::p_load(dplyr, readxl) #Packages needed
rm(list=ls()) ## cleaning up
set.seed(251) #set seed for reproducibility (for random number generation)

data_GNM_018 <- read_excel("data/02_data_extraction/extracted_raw_data/GNM_018_data-by-authors.xlsx")
head(data_GNM_018)
summary(data_GNM_018)
names(data_GNM_018)


laying_date<-data_GNM_018 %>%
  group_by(expPlants) %>%
  summarise(mean=mean(LDATE),
            sd=sd(LDATE),
            samplesize=(n()))

clutch_size<-data_GNM_018 %>%
  group_by(expPlants) %>%
  summarise(mean=mean(CS),
            sd=sd(CS),
            samplesize=(n()))

hatching_date<-data_GNM_018 %>%
  group_by(expPlants) %>%
  summarise(mean=mean(HDATE),
            sd=sd(HDATE),
            samplesize=(n()))

brood_size_day6<-data_GNM_018 %>%
  group_by(expPlants) %>%
  summarise(mean=mean(BS6),
            sd=sd(BS6),
            samplesize=(n()))

hatching_success<-data_GNM_018 %>%
  group_by(expPlants) %>%
  summarise(mean=mean(HS),
            sd=sd(HS),
            samplesize=(n()))

reproductive_success<-data_GNM_018 %>%
  group_by(expPlants) %>%
  summarise(mean=mean(na.omit(RS)),
            sd=sd(na.omit(RS)),
            samplesize=(na.omit(n())))

