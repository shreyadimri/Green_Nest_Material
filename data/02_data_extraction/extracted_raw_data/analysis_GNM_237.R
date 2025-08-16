
# The dataset provided by the authors was split in 3 files 
#(to be handled by R)# namely:  
#natural_nests; 
#artificial_nests_2012;
# and artificial_nests_2013


library(readr)
# Extracting data from the file "natural_nests_GNM_237";
data<-read_csv("data/02_data_extraction/extracted_raw_data/natural_nests_GNM_237.csv")

head(data)
tail(data)
summary(data)
names(data)

library(dplyr)

#Extracting data (SD;MEAN;sample size) llog.mesophilic.bacterial.density..1st.
data %>%
  group_by(Feather.treatment, Plant.treatment,Contamination, Year) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..1st.)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..1st.))),
            samplesize=(n()))

# creating csv file to help the data extraction
natlogmes1st<-data %>%
  group_by(Feather.treatment, Plant.treatment,Contamination, Year) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..1st.)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..1st.))),
            samplesize=(n()))

write.csv(natlogmes1st, "natlogmes1st.csv")

getwd()
#Extracting data (SD;MEAN;sample size) log.mesophilic.bacterial.density..2nd.

data %>%
  group_by(Feather.treatment, Plant.treatment,Contamination, Year) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..2nd.)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..2nd.))),
            samplesize=(n()))

# creating csv file to help the data extraction 
natlogmes2nd<-data %>%
  group_by(Feather.treatment, Plant.treatment,Contamination, Year) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..2nd.)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..2nd.))),
            samplesize=(n()))

write.csv(natlogmes2nd, "natlogmes2nd.csv")




# Extracting data from the file "artificial_nests_2012_GNM_237"

rm(list = ls()) #clearing global environment
data_2012<-read.csv(file.choose(), head = TRUE, sep=";") #calling the variable artificial_nests_2012"

head(data_2012) # data checking
tail(data_2012) # data checking
summary(data_2012)
names(data_2012) #checking names of variables

# extracting Log.mesophilic.bacterial.density..1st.sampling.

data_2012 %>%
  group_by(Feather.treatment, Plants.treatment,Year, Area) %>%
  summarise(mean=mean(na.omit(Log.mesophilic.bacterial.density..1st.sampling.)),
            sd=(sd(na.omit(Log.mesophilic.bacterial.density..1st.sampling.))),
            samplesize=(n()))

# creating csv file to help the data extraction
art_2012_logmes_1st<- data_2012 %>%
  group_by(Feather.treatment, Plants.treatment,Year, Area) %>%
  summarise(mean=mean(na.omit(Log.mesophilic.bacterial.density..1st.sampling.)),
            sd=(sd(na.omit(Log.mesophilic.bacterial.density..1st.sampling.))),
            samplesize=(n()))

write.csv(art_2012_logmes_1st, "art_2012_logmes_1st.csv")

# extracting og.mesophilic.bacterial.density..2nd.sampling.
data_2012 %>%
  group_by(Feather.treatment, Plants.treatment,Year, Area) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..2nd.sampling.)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..2nd.sampling.))),
            samplesize=(n()))
# creating csv file to help the data extraction
art_2012_logmes_2nd<- data_2012 %>%
  group_by(Feather.treatment, Plants.treatment,Year, Area) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..2nd.sampling.)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..2nd.sampling.))),
            samplesize=(n()))


write.csv(art_2012_logmes_2nd, "art_2012_logmes_2nd.csv")

# Extracting data from the file "artificial_nests_2013_GNM_237"


rm(list = ls()) #clearing global environment
data_2013<-read.csv(file.choose(), head = TRUE, sep=";") #calling the variable artificial_nests_2012"

head(data_2013) # data checking
tail(data_2013) # data checking
summary(data_2013)
names(data_2013) #checking names of variables

# extracting log.mesophilic.bacterial.density..1st.sampling..cont.egg

data_2013 %>%
  group_by(Feather.treatment,Plant.treatment,Year, Area) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..1st.sampling..cont.egg)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..1st.sampling..cont.egg))),
            samplesize=(n()))
# creating csv file to help the data extraction
art_2013_logmes_1st_cont <- data_2013 %>%
  group_by(Feather.treatment,Plant.treatment,Year, Area) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..1st.sampling..cont.egg)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..1st.sampling..cont.egg))),
            samplesize=(n()))

write.csv(art_2013_logmes_1st_cont, "art_2013_logmes_1st_cont.csv")
# extracting log.mesophilic.bacterial.density..2nd.sampling..cont.egg
data_2013 %>%
  group_by(Feather.treatment,Plant.treatment,Year, Area) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..2nd.sampling..cont.egg)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..2nd.sampling..cont.egg))),
            samplesize=(n()))

# creating csv file to help the data extraction
art_2013_logmes_2nd_cont <- data_2013 %>%
  group_by(Feather.treatment,Plant.treatment,Year, Area) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..2nd.sampling..cont.egg)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..2nd.sampling..cont.egg))),
            samplesize=(n()))


write.csv(art_2013_logmes_2nd_cont, "art_2013_logmes_2nd_cont.csv")

# extracting log.mesophilic.bacterial.density..3rd.sampling..cont.egg 
data_2013 %>%
  group_by(Feather.treatment,Plant.treatment,Year, Area) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..3rd.sampling..cont.egg)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..3rd.sampling..cont.egg))),
            samplesize=(n()))


# creating csv file to help the data extraction
art_2013_logmes_3rd_cont <- data_2013 %>%
  group_by(Feather.treatment,Plant.treatment,Year, Area) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..3rd.sampling..cont.egg)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..3rd.sampling..cont.egg))),
            samplesize=(n()))

write.csv(art_2013_logmes_3rd_cont, "art_2013_logmes_3rd_cont.csv")

#_______________________________________________________________________


# extracting log.mesophilic.bacterial.density..1st.sampling..nocont.egg

data_2013 %>%
  group_by(Feather.treatment,Plant.treatment,Year, Area) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..1st.sampling..nocont.egg)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..1st.sampling..nocont.egg))),
            samplesize=(n()))

# creating csv file to help the data extraction
art_2013_logmes_1st_nocont <- data_2013 %>%
  group_by(Feather.treatment,Plant.treatment,Year, Area) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..1st.sampling..nocont.egg)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..1st.sampling..nocont.egg))),
            samplesize=(n()))

write.csv(art_2013_logmes_1st_nocont, "art_2013_logmes_1st_nocont.csv")


# extracting log.mesophilic.bacterial.density..2nd.sampling..nocont.egg
data_2013 %>%
  group_by(Feather.treatment,Plant.treatment,Year, Area) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..2nd.sampling..nocont.egg)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..2nd.sampling..nocont.egg))),
            samplesize=(n()))

# creating csv file to help the data extraction
art_2013_logmes_2nd_nocont <- data_2013 %>%
  group_by(Feather.treatment,Plant.treatment,Year, Area) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..2nd.sampling..nocont.egg)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..2nd.sampling..nocont.egg))),
            samplesize=(n()))

write.csv(art_2013_logmes_2nd_nocont, "art_2013_logmes_2nd_nocont.csv")


# extracting log.mesophilic.bacterial.density..3rd.sampling..nocont.egg

data_2013 %>%
  group_by(Feather.treatment,Plant.treatment,Year, Area) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..3rd.sampling..nocont.egg)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..3rd.sampling..nocont.egg))),
            samplesize=(n()))

# creating csv file to help the data extraction
art_2013_logmes_3rd_nocont <- data_2013 %>%
  group_by(Feather.treatment,Plant.treatment,Year, Area) %>%
  summarise(mean=mean(na.omit(log.mesophilic.bacterial.density..3rd.sampling..nocont.egg)),
            sd=(sd(na.omit(log.mesophilic.bacterial.density..3rd.sampling..nocont.egg))),
            samplesize=(n()))

write.csv(art_2013_logmes_3rd_nocont, "art_2013_logmes_3rd_nocont.csv")


# The number of nests for 2013 provided in the dataset,
#differs from the ones mentioned un the paper by (-60)papers




 