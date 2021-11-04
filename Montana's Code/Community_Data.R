# Communities for Summer 2021
# Defining Data Sheets ----- 
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

fish = read.csv("Data/2021_Measurement.csv")
sample = read.csv("Data/2021_Sample.csv")
#sites = read.csv("SITES.csv")

# Join/Filter ---------
# Join/Filter ---------
all_data = left_join(fish, sample, by = "YSAMP_N") %>% 
   
  filter(WATER == "LOP",
         ### single out or remove specific species
         (SPECIES != "NF" & SPECIES != ""),
         SPECIES != "PAINTED TURTLE",
         SPECIES != "NEWT", 
         SPECIES != "CRAY", 
         SPECIES != "TADPOLE", 
         SPECIES != "BELLISTOMADID ", 
         SPECIES != "DRAGONFLY", 
         SPECIES != "LEECH", 
         SPECIES != "MOLE SALAMANDER LARVAE", 
         SPECIES != "ODONATA") %>%
  select(YSAMP_N, SPECIES, LENGTH, DATE_COL) %>%
  separate(DATE_COL, into = c("MONTH","DAY","YEAR"), sep = "/") %>%
  filter(MONTH ==8)

TPP = all_data %>%
  group_by(SPECIES, MONTH) %>%
  count()
TPP %>% ggplot(aes(x = SPECIES, y = n)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme(axis.text.x=element_text(angle=90,hjust=1),
        text = element_text(size=18)) + 
  ggtitle("Tom Peck Pond") + 
  ylab("# Individuals Captured")+
  ylim(0,250)



LOP = all_data %>%
  group_by(SPECIES, MONTH) %>%
  count()
LOP%>% ggplot(aes(x = SPECIES, y =n)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme(axis.text.x=element_text(angle=90,hjust=1),
        text = element_text(size=18)) + 
  ggtitle("Long Pond")+ 
  ylab("# Individuals Captured")+
  ylim(0,250)



PRL = all_data %>%
  group_by(SPECIES, MONTH) %>%
  count()
PRL %>% ggplot(aes(x = SPECIES, y = n)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme(axis.text.x=element_text(angle=90,hjust=1),
        text = element_text(size=18)) + 
  ggtitle("Panther Lake")+ 
  ylab("# Individuals Captured") +
  ylim(0,250)



all_data = left_join(fish, sample, by = "YSAMP_N") %>% 
  
  filter(### single out or remove specific species
         (SPECIES != "NF" & SPECIES != ""),
         SPECIES != "PAINTED TURTLE",
         SPECIES != "NEWT", 
         SPECIES != "CRAY", 
         SPECIES != "TADPOLE", 
         SPECIES != "BELLISTOMADID ", 
         SPECIES != "DRAGONFLY", 
         SPECIES != "LEECH", 
         SPECIES != "MOLE SALAMANDER LARVAE", 
         SPECIES != "ODONATA", 
         WATER != "COM") %>%
  select(YSAMP_N, SPECIES, LENGTH, DATE_COL, WATER) %>%
  separate(DATE_COL, into = c("MONTH","DAY","YEAR"), sep = "/") %>%
  filter(MONTH ==8)

all = all_data %>%
  group_by(SPECIES,WATER) %>%
  count()
all %>% ggplot(aes(x = SPECIES, y = n, fill = WATER)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme(axis.text.x=element_text(angle=90,hjust=1),
        text = element_text(size=18)) + 
  ggtitle("Panther Lake")+ 
  ylab("# Individuals Captured")

