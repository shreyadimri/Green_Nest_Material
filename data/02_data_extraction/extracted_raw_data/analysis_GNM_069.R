
## Setting up workspace

pacman::p_load(dplyr, readr) #Packages needed
rm(list=ls()) ## cleaning up
set.seed(1) #set seed for reproducibility (for random number generation)

data_GNM_069 <- read_csv("data/02_data_extraction/extracted_raw_data/data_GNM_069.csv")

head(data_GNM_069)
summary(data_GNM_069)
names(data_GNM_069)


names(data_GNM_069) <- make.names(names(data_GNM_069), unique=TRUE)


# Variables to extract: 
# Tarsus length  3 days old  /	 Tarsus.length..14.days.old./ Body mas  3 days old. /	
# log10.mesophilic.bacteria..3.days.old./
# log10.Enterococcus..3.days.old. /# log.Carnus..day.8. / 
#log10.mesophilic.bacteria..8days.old. / 
# /Body.mas..14.days.old. /
#/ Rank.Telomere.length..day.14. / 
#Residuals.Rank.Telomere.length..day.14. / Dif.rank..14.3.days. / 
# Residuals.Dif.rank..14.3.days.

#Extracting data (SD;MEAN;sample size) Tarsus.length..3.days.old.

data_GNM_069 %>%
  group_by(Feather.treatments, Plant.treatments, Location, Nest) %>%
  summarise(Tarsus.length..3.days.old.=mean(Tarsus.length..3.days.old.),
            samplesize=(n())) %>%
  group_by(Feather.treatments, Plant.treatments, Location) %>%
     summarise(mean=mean(Tarsus.length..3.days.old.),
                  sd=sd(Tarsus.length..3.days.old.),
                  samplesize=(n()))



#Extracting data (SD;MEAN;sample size) Tarsus.length..14.days.old.
data_GNM_069 %>%
  group_by(Feather.treatments, Plant.treatments, Location, Nest) %>%
  summarise(Tarsus.length..14.days.old.=mean(Tarsus.length..14.days.old.),
            samplesize=(n()))%>%
  group_by(Feather.treatments, Plant.treatments, Location) %>%
  summarise(mean=mean(Tarsus.length..14.days.old.),
            sd=sd(Tarsus.length..14.days.old.),
            samplesize=(n()))

#Extracting data (SD;MEAN;sample size) Body.mas..3.days.old.
data_GNM_069 %>%
  group_by(Feather.treatments, Plant.treatments, Location, Nest) %>%
  summarise(Body.mas..3.days.old.=mean(Body.mas..3.days.old.),
            samplesize=(n()))%>%
  group_by(Feather.treatments, Plant.treatments, Location) %>%
  summarise(mean=mean(Body.mas..3.days.old.),
            sd=sd(Body.mas..3.days.old.),
            samplesize=(n()))



#Extracting data (SD;MEAN;sample size) Body.mas..14.days.old.
data_GNM_069 %>%
  group_by(Feather.treatments, Plant.treatments, Location, Nest) %>%
  summarise(Body.mas..14.days.old.=mean(Body.mas..14.days.old.),
            samplesize=(n()))%>%
  group_by(Feather.treatments, Plant.treatments, Location) %>%
  summarise(mean=mean(Body.mas..14.days.old.),
            sd=sd(Body.mas..14.days.old.),
            samplesize=(n()))


#Extracting data (SD;MEAN;sample size) log10.mesophilic.bacteria..3.days.old.
data_GNM_069 %>%
  group_by(Plant.treatments, Location, Nest) %>%
  summarise(log10.mesophilic.bacteria..3.days.old.= 
              mean(na.omit(log10.mesophilic.bacteria..3.days.old.)),
            samplesize=(n()))%>%
  group_by(Plant.treatments, Location) %>%
  summarise(mean=mean(na.omit(log10.mesophilic.bacteria..3.days.old.)),
            sd=sd(na.omit(log10.mesophilic.bacteria..3.days.old.)),
            samplesize=(n()))

## feather plant calahorra group has one nest with NA therefore the sample size 
## extracted is 13 and not 14 as here. 
# data_GNM_069 %>% filter(is.na(log10.mesophilic.bacteria..3.days.old.))

#Extracting data (SD;MEAN;sample size) log10.mesophilic.bacteria..8days.old.
data_GNM_069 %>%
  group_by(Plant.treatments, Location, Nest) %>%
  summarise(log10.mesophilic.bacteria..8days.old.= 
              mean(na.omit(log10.mesophilic.bacteria..8days.old.)),
            samplesize=(n()))%>%
  group_by(Plant.treatments, Location) %>%
  summarise(mean=mean(na.omit(log10.mesophilic.bacteria..8days.old.)),
            sd=sd(na.omit(log10.mesophilic.bacteria..8days.old.)),
            samplesize=(n()))


#Extracting data (SD;MEAN;sample size) log10.Enterococcus..3.days.old.
data_GNM_069 %>%
  group_by(Plant.treatments, Location, Nest) %>%
  summarise(log10.Enterococcus..3.days.old.= 
              mean(na.omit(log10.Enterococcus..3.days.old.)),
            samplesize=(n())) %>%
  group_by(Plant.treatments, Location) %>%
  summarise(mean=mean(na.omit(log10.Enterococcus..3.days.old.)),
            sd=(sd(na.omit(log10.Enterococcus..3.days.old.))),
            samplesize=(n()))

## feather plant calahorra group has one nest with NA therefore the sample size 
## extracted is 13 and not 14 as here. Also feather no-plant Calahorra group has 
# one nest with NA therefore sample size extracted for that group is 14 and not 15. 

# data_GNM_069 %>%
# filter(is.na(log10.Enterococcus..3.days.old.))

#Extracting data (SD;MEAN;sample size) Enterococcus..8.days.old.
data_GNM_069 %>%
  group_by(Plant.treatments, Location, Nest) %>%
  summarise(Enterococcus..8.days.old.= mean(na.omit(Enterococcus..8.days.old.)),
            samplesize=(n()))%>%
  group_by(Plant.treatments, Location) %>%
  summarise(mean=mean(na.omit(Enterococcus..8.days.old.)),
            sd=(sd(na.omit(Enterococcus..8.days.old.))),
            samplesize=(n()))


#Extracting data (SD;MEAN;sample size) log.Carnus..day.8.
data_GNM_069 %>%
  group_by(Plant.treatments, Location, Nest) %>%
  summarise(log.Carnus..day.8.=mean(na.omit(log.Carnus..day.8.)),
            samplesize=(n())) %>%
  group_by(Plant.treatments, Location) %>%
  summarise(mean=mean(na.omit(log.Carnus..day.8.)),
            sd=(sd(na.omit(log.Carnus..day.8.))),
            samplesize=(n()))

## no-feather no-plant Hu\xe9neja group has one nest with NA therefore the sample size 
## extracted is 2 and not 3 as here.

# data_GNM_069 %>%
# filter(is.na(log.Carnus..day.8.))

#Extracting data (SD;MEAN;sample size) Rank.Telomere.length..day.14. We also have 
# residuals available in the data (as provided and discussed in the article by the authors)
# We chose to use raw means instead. 

data_GNM_069 %>%
  group_by(Plant.treatments, Location, Nest) %>%
  summarise(Rank.Telomere.length..day.14.=mean(na.omit(Rank.Telomere.length..day.14.)),
            samplesize=(n()))%>%
  group_by( Plant.treatments, Location) %>%
  summarise(mean=mean(na.omit(Rank.Telomere.length..day.14.)),
            sd=(sd(na.omit(Rank.Telomere.length..day.14.))),
            samplesize=(n()))

#Extracting data (SD;MEAN;sample size) Dif.rank..14.3.days. 
# This is what authors call Telomere Dynamics


data_GNM_069 %>%
  group_by(Plant.treatments, Location, Nest) %>%
  summarise(Dif.rank..14.3.days.=mean(na.omit(Dif.rank..14.3.days.)),
            samplesize=(n()))%>%
  group_by(Plant.treatments, Location) %>%
  summarise(mean=mean(na.omit(Dif.rank..14.3.days.)),
            sd=(sd(na.omit(Dif.rank..14.3.days.))),
            samplesize=(n()))
