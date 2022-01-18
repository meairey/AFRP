# Communities for Summer 2021
# Load Libraries ----
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

# Defining Data Sheets ----- 
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
    .$DAYS == 0 ~ 0))


sample = sample %>% 
  mutate(effort_soak = DAYS + HOURS)

## add 
# Trying to work through CPUE 

(sample$TIME_END) - sample$TIME_START


sample = sample %>% 

  


  
  

#sites = read.csv("SITES.csv")

# Join/Filter ----

# Data is filtered to select only fish and split out several columns
all_data = left_join(fish, sample, by = "YSAMP_N") %>% 
  filter(WATER %in% c("TPP","LOP","HRM_FOR","PRL","HRM_BW2"),
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
  # Split out date to get at month collected
  separate(DATE_COL, into = c("MONTH","DAY","YEAR"), sep = "/") %>%
  # Split out YSAMP_N to be able to select year and water
  separate(YSAMP_N, into = c("GEAR","WATER","YEAR","DSAMP")) %>%
  # Aggregate months into seasons
  mutate(SEASON = as.factor(.bincode(MONTH, c(0,7,8, 11))))  %>%
  # Recode the bins for visualizations
  mutate(SEASON = recode_factor(.$SEASON, "1" = "spring", "2"="summer","3" = "fall")) %>%
  mutate(SITE = parse_number(SITE_N)) %>%
  filter(SITE != 4)
  
  
## Visualizations ---- 
  
# Visualize the communities from each lake by season 
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




  

## Old code - probably delete -----------------

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
