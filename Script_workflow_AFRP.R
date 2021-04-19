## Installing Library ------ 
library(dplyr)
library(tidyr)
library(tidyverse)


# Defining Data Sheets ----- 
fish = read.csv("FISH_MEASUREMENT_whole.csv")
sample = read.csv("FISH_SAMPLE.csv")
sites = read.csv("SITES.csv")

# Join/Filter ---------
# Join/Filter ---------
all_data = left_join(fish, sample, by = "YSAMP_N") %>% 
  left_join(sites, by = "SITE_N") %>% 
  separate(SITE_N,  into = c("GEAR", "WATER","SITE")) %>% 
  filter(WATER == "LML",
         GEAR == "BEF", 
         ### single out or remove specific species
         (SPECIES != "NF" & SPECIES != ""),
         ### remove sites where no one described habitat
         (HAB_1 != "NA" & HAB_1 != ""),
         ### remove sites with no described site 
         SITE != "NA", 
         GEAR_CODE == "NAF", 
         YEAR > 1999
  )




## Look to see what is going on with effort is there one per day  
## Calculate CPUE information ------
CPUE = all_data %>%
  select(YSAMP_N, DAY_N, YEAR, SEASON, WATER, SITE, SPECIES,
         FISH_N, WEIGHT, LENGTH, HAB_1, GEAR, EFFORT) %>%
  group_by(WATER, DAY_N, YEAR, SITE, SPECIES, EFFORT) %>%
  count() %>% ## Abundance per year, site, species
  mutate(CPUE_seconds = n / EFFORT) %>%
  ungroup() %>%
  complete(WATER, YEAR,DAY_N, SITE,SPECIES) %>%
  replace_na(list(CPUE_seconds = 0, n = 0))

### Plot CPUE information --------
## With CPUE data not logged
CPUE %>%
  ggplot(aes(x = YEAR, y = (CPUE_seconds))) +
  geom_point() +
  geom_smooth(method = "lm") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +   facet_wrap(~SPECIES)





#### Habitat Classifiers -------------------------------

# Hab classifiers needs a table with CPUE standardized by # sites / habitat 


## Adding in HabNumbs to standardize catch across sites 

Hab_numbs = all_data %>%
  select(HAB_1, SITE) %>%
  unique() %>%
  count(HAB_1) %>%
  rename(HAB_NUMB = n)

all_data = left_join(all_data, Hab_numbs, by = "HAB_1")

# Calculate CPUE_std 

CPUE = all_data %>%
  select(YSAMP_N, DAY_N, YEAR, SEASON, WATER, SITE, SPECIES,
         FISH_N, WEIGHT, LENGTH, HAB_1, GEAR, EFFORT, HAB_NUMB) %>%
  group_by(WATER, DAY_N, YEAR, SITE, SPECIES, EFFORT, HAB_1, HAB_NUMB) %>%
  count() %>% ## Abundance per year, site, species
  mutate(CPUE_std = n / (EFFORT*HAB_NUMB)) %>%
  ungroup() %>%
  complete(WATER, YEAR,DAY_N, SITE,SPECIES) %>%
  replace_na(list(CPUE_seconds = 0, n = 0))



## Plot CPUE information with HAB classifiers 
## Standardized 
CPUE %>%
  filter(SPECIES %in% c("CC", "CS","LLS","LT","MM", "PS","RS","SMB","SS","ST","WS")) %>%
  ggplot(aes(x = YEAR, y =CPUE_std,col = HAB_1)) +
  geom_smooth() +
  scale_y_continuous(trans='log2') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~SPECIES)

## Length Information -------

all_data %>%
  filter(SEASON == "F") %>%
  group_by(YEAR, SPECIES) %>%
  ggplot(aes(YEAR, y = LENGTH)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  scale_y_continuous(trans='log2') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~SPECIES) + 
  ggtitle("Length in Fall")

## Average Length information with habitat classifiers

all_data %>%
  group_by(YEAR, SPECIES, HAB_1, SEASON) %>%
  mutate(avg.length = mean(LENGTH, na.rm=T)) %>%
  ggplot(aes(YEAR, y = avg.length)) + 
  geom_point(aes(color = HAB_1)) + 
  geom_smooth() + 
  scale_y_continuous(trans='log2') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~SPECIES)




