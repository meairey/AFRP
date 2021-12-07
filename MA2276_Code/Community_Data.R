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
   
  filter(WATER == "HRM",
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
  select(YSAMP_N, SPECIES, LENGTH, DATE_COL, SITE_N) %>%
  separate(DATE_COL, into = c("MONTH","DAY","YEAR"), sep = "/") %>%
  filter(MONTH ==9)

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
  filter(MONTH ==8) %>% as.data.frame() %>%
  complete(SPECIES, WATER) %>% 
  replace_na(list(CPUE_seconds = 0, n = 0))
  

all = all_data %>%
  group_by(SPECIES,WATER) %>%
  count()
all %>% ggplot(aes(x = SPECIES, y = n, fill = WATER)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme(axis.text.x=element_text(angle=90,hjust=1),
        text = element_text(size=18)) + 
  ggtitle("Summer 21")+ 
  ylab("# Individuals Captured")

all %>% ggplot(aes(x = WATER, y = n, fill = SPECIES)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme(axis.text.x=element_text(angle=90,hjust=1),
        text = element_text(size=18)) + 
  ggtitle("Summer 21")+ 
  ylab("# Individuals Captured") + 
  labs(fill = "Species")



HRM = all_data %>%
  group_by(SPECIES, SITE_N) %>%
  count() %>% as.data.frame() %>%
  complete(SPECIES, SITE_N) %>% 
  replace_na(list(CPUE_seconds = 0, n = 0))

HRM %>% ggplot(aes(x = SPECIES, y = n, fill = SITE_N)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme(axis.text.x=element_text(angle=90,hjust=1),
        text = element_text(size=18)) + 
  ggtitle("Heron Marsh")+ 
  ylab("# Individuals Captured") + 
  labs(fill = 'Site') + 
  xlab("Species")

HRM %>% ggplot(aes(y = n, x = SITE_N, fill = SPECIES)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme(axis.text.x=element_text(angle=90,hjust=1),
        text = element_text(size=18)) + 
  ggtitle("Heron Marsh")+ 
  ylab("# Individuals Captured") + 
  labs(fill = 'Site') + 
  xlab("Species")
