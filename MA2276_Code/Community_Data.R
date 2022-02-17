# Communities for Summer 2021
# Load Libraries ----
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

# Defining Data Sheets ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP")
fish = read.csv("MA2276_Code/Data/2021_Measurement.csv")
sample = read.csv("MA2276_Code/Data/2021_Sample.csv") %>%
  mutate(DATE_COL = mdy(DATE_COL)) %>%
  mutate(DATE_SET = mdy(DATE_SET)) %>%
  mutate(DAYS = DATE_COL - DATE_SET) %>%
  separate(TIME_START, into = c("hour","min")) %>%
  mutate(min = round(as.numeric(min)/60*100)) %>%
  unite(TIME_START, c(hour, min), sep = ".") %>%
  separate(TIME_END, into = c("hour","min")) %>%
  mutate(min = round(as.numeric(min)/60*100)) %>%
  unite(TIME_END, c(hour, min), sep = ".") %>%
  mutate(TIME_START = as.numeric(TIME_START)) %>%
  mutate(TIME_END = as.numeric(TIME_END)) %>%
  mutate(HOURS = TIME_END - TIME_START) %>% 
  mutate(DAYS = case_when(
    .$DAYS == 1 ~ 24,
    .$DAYS == 0 ~ 0)) # for some reason this line of code appears to break the pipeline?
sample = sample %>% mutate(
  effort_soak = DAYS + HOURS
)
# Join/Filter ----

# Data is filtered to select only fish and split out several columns
all_data = left_join(fish, sample, by = "YSAMP_N") %>% 
  filter(WATER %in% c("TPP","LOP","HRM_1","HRM_2", "HRM_3","HRM_4","WFL", "COM","PRL","ETL"),
         ### single out or remove specific species
         (SPECIES != ""),
         SPECIES != "PAINTED TURTLE",
         SPECIES != "NEWT", 
         SPECIES != "CRAY", 
         SPECIES != "TADPOLE", 
         SPECIES != "BELLISTOMADID ", 
         SPECIES != "DRAGONFLY", 
         SPECIES != "LEECH", 
         SPECIES != "MOLE SALAMANDER LARVAE", 
         SPECIES != "ODONATA", 
         SPECIES != "STICKBUG") %>%
  select(YSAMP_N, SPECIES, LENGTH, DATE_COL, SITE_N, DAYS, effort_soak, GEAR_CODE) %>%
  # Split out date to get at month collected
  separate(DATE_COL, into = c("YEAR","MONTH","DAY"), sep = "-") %>%
  # Split out YSAMP_N to be able to select year and water
  separate(YSAMP_N, into = c("GEAR","WATER","YEAR","YSAMP")) %>%
  # Aggregate months into seasons
  mutate(SEASON = as.factor(.bincode(MONTH, c(0,7,8, 11))))  %>%
  # Recode the bins for visualizations
  mutate(SEASON = recode_factor(.$SEASON, "1" = "spring", "2"="summer","3" = "fall")) %>%
  mutate(SITE = parse_number(SITE_N)) %>%
  filter(SITE != 4) %>% 
  mutate(LENGTH = as.numeric(LENGTH))
  

## CPUE by SITE -----


## Need to add in D_SAMP

night_sets_sites = all_data %>%
  filter(DAYS >= 24) %>%
  select(MONTH, DAY, YEAR,
         SEASON, WATER, 
         SITE, SPECIES,
         LENGTH,  effort_soak,YSAMP, GEAR_CODE) %>%
  group_by(WATER, SEASON, MONTH, DAY, YEAR, SITE, SPECIES, YSAMP,effort_soak, GEAR_CODE) %>%
  count() %>%
  ungroup() %>% 
  select(-MONTH) %>%
  group_by(WATER, SITE, YEAR) %>%
  complete(.,nesting(SEASON, GEAR_CODE, effort_soak, DAY, YSAMP), SPECIES) %>%
  replace_na(list(n = 0, n = 0)) %>%
  group_by(WATER, SEASON,YEAR, SITE, SPECIES) %>% 
  summarise_at(c("n", "effort_soak"), sum) %>%
  mutate(CPUE_hour = n / effort_soak) %>%
  select(c(-n, -effort_soak))

## ++ Visualization CPUE SITE ----

for(i in unique(all_data$WATER)){
  
  subset_data = night_sets_sites %>% filter(WATER == i) 
  
  for(h in unique(subset_data$SEASON)){
    c = subset_data %>%
      filter(SEASON == h) %>%
      ggplot(aes(x = SPECIES, y = CPUE_hour)) + 
      geom_bar(stat="identity", position=position_dodge()) + 
      theme(axis.text.x=element_text(angle=90,hjust=1),
            text = element_text(size=12)) + 
      ylim(0,.45) +
      ylab(paste(i,h))+
      facet_wrap(~ SITE)
    
    print((c))
  }
}


## AVG CPUE ----- 

# YSAMP
night_sets_cpueavg =  all_data %>%
  filter(DAYS >= 24) %>%
  select(MONTH, DAY, YEAR,
         SEASON, WATER, 
         SITE, YSAMP,SPECIES,
         LENGTH,  effort_soak, GEAR_CODE) %>%
  group_by(WATER, SEASON,
           MONTH, DAY, YSAMP, 
           YEAR, SITE,
           SPECIES, effort_soak, 
           GEAR_CODE) %>%
  count() %>%
  ungroup() %>% 
  select(-MONTH) %>%
  group_by(WATER, YEAR) %>%
  complete(.,nesting(SEASON, SITE, GEAR_CODE, effort_soak, DAY, YSAMP), SPECIES) %>%
  replace_na(list(n = 0, n = 0)) %>%
  group_by(WATER, SEASON,YEAR, SPECIES) %>% 
  summarise_at(c("n", "effort_soak"), sum) %>%
  mutate(CPUE_hour = n / effort_soak) %>%
  select(c(-n, -effort_soak)) %>%
  filter(SPECIES != "NF")


# ++ Visualize the communities from each lake by season ----
for(i in unique(night_sets_cpueavg$WATER)){
  g = night_sets_cpueavg %>% 
    filter(WATER == i) %>%
    group_by(SEASON, YEAR, SPECIES) %>%
    summarise(CPUE = sum(CPUE_hour)) %>%
    ggplot(aes(x = SPECIES, y = CPUE)) + 
    geom_bar(stat="identity", position=position_dodge()) + 
    theme(axis.text.x=element_text(angle=90,hjust=1),
          text = element_text(size=12)) + 
    ylim(0,.45) +
    facet_wrap(~ SEASON) + 
    ggtitle(paste(i, "Community"))
  print(g)
}




## all_data visualizations ---- 

  
# Visualize the communities from each lake by season 
# This is the raw count of abundance across all traps
for(i in unique(all_data$WATER)){
  g = all_data %>% 
    filter(WATER == i) %>%
    group_by(SEASON, YEAR, SPECIES) %>%
    count() %>%
    ggplot(aes(x = SPECIES, y = n)) + 
    geom_bar(stat="identity", position=position_dodge()) + 
    theme(axis.text.x=element_text(angle=90,hjust=1),
          text = element_text(size=12)) + 
    facet_wrap(~ SEASON) + 
    ggtitle(paste(i, "Community"))
  print(g)
}
  
## with night_sets 



# Visualize the communities from each site + lake by season 
for(i in unique(all_data$WATER)){
  
  subset_data = all_data %>% filter(WATER == i) %>% 
    mutate(SITE_N = parse_number(SITE_N))
    
    for(h in unique(subset_data$SEASON)){
      c = subset_data %>%
        filter(SEASON == h) %>%
        group_by(WATER, SEASON,SPECIES, SITE_N) %>%
        count()%>%
        ggplot(aes(x = SPECIES, y = n)) + 
        geom_bar(stat="identity", position=position_dodge()) + 
        theme(axis.text.x=element_text(angle=90,hjust=1),
              text = element_text(size=12)) + 
        ggtitle(paste(i, "Community", h)) + 
        facet_wrap(~ SITE_N)
      
      print((c))
  }
}


## Analysis of size structure -------


all_data %>% 
  ggplot(aes(x = SPECIES, y = as.numeric(LENGTH))) + 
  geom_point() +
  theme(axis.text.x=element_text(angle=90,hjust=01),
                     text = element_text(size=12)) +
  stat_summary(
    geom = "point",
    fun= "median",
    col = "red",
    size = 2,
    shape = 16,
    fill = "red") +
  facet_wrap(~WATER) + 
  ylab("Length (mm)") + 
  xlab("Species")
  

all_data %>% filter(SPECIES == "CC") %>% 
  ggplot(aes(x = SPECIES, y = as.numeric(LENGTH))) + 
  geom_point() + 
  stat_summary(
    geom = "point",
    fun= "median",
    col = "red",
    size = 2,
    shape = 16,
    fill = "red") +
  ylab("Length (mm)")
  facet_wrap(~WATER)

median_length = all_data %>%
  group_by(SPECIES, WATER) %>%
  summarise(median_length = median(as.numeric(LENGTH),na.rm=T))

species_richness = all_data %>%
  select(WATER, SPECIES) %>%
  group_by(WATER) %>%
  unique() %>% 
  summarize(Richness = n())
  
left_join(median_length, species_richness) %>%
  ggplot(aes(x = Richness, y = median_length)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~SPECIES)

median_length_z = all_data %>%
  group_by(SPECIES) %>%
  mutate(zscore = (LENGTH - median(LENGTH, na.rm=T))/sd(LENGTH, na.rm=T))

left_join(median_length_z, species_richness) %>%
  ggplot(aes(x = Richness, y = zscore)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~SPECIES)


dat = left_join(median_length_z, species_richness) %>%
  filter(SPECIES == "NRD")
  select(Richness, zscore, SPECIES)

plot(dat$zscore~ dat$Richness)
abline(lm(dat$zscore ~ dat$Richness))
summary(lm(dat$zscore ~ dat$Richness))



