library(dplyr)
library(tidyr)
library(ggplot2)
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/Montana's Code")
source("../AFRP Master Code/functions.R")

all_data = filter_data(water = "LML", gear = "BEF", gear_code = "NAF", species = species,max_month = 7)
Hab_numbs = hab_numbs(all_data)
CPUE.lit = CPUE_long_seconds_habitat(all_data)


CPUE.lit %>%
  filter(SPECIES %in% c("CS","CC","SMB","PS")) %>%
  mutate(CPUE_std = round(CPUE_std,4)) %>%
  ggplot(aes(x = YEAR, y = CPUE_std, col = HAB_1)) +
  ggtitle("CPUE") +
  #geom_point(size = 4) +
  geom_smooth(se=F, method ='loess')  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ylab("CPUE") + 
  theme(text  = element_text(size = 18)) +
  scale_y_continuous(trans = 'log2') + 
  facet_wrap(~SPECIES)
