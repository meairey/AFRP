# Communities for Summer 2021
# Load Libraries ----
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

# Defining Data Sheets ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP")
fish = read.csv("MA2276_Code/Data/Measurement_FINAL_mostuptodate.csv")
sample = read.csv("MA2276_Code/Data/SAMP_NEW_FINAL_MAID.csv") %>%
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
  filter(WATER %in% c("TPP","LOP","HRM_BW2","HRM_SHI", "HRM_FOR","HRM_BAR","WFL", "COM","PRL","ETL"),
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
  select(YSAMP_N,WATER, SPECIES, LENGTH, WEIGHT, DATE_COL, SITE_N, DAYS, effort_soak, GEAR_CODE) %>%
  # Split out date to get at month collected
  separate(DATE_COL, into = c("YEAR","MONTH","DAY"), sep = "-") %>%
  # Split out YSAMP_N to be able to select year and water
  separate(YSAMP_N, into = c("GEAR","WATER_ID","YEAR","YSAMP")) %>%
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

## This below was used to make some graphs for ARF comparing overlap, CPUE, and richness across waters. You'll need to load in the other scripts. 
night_sets_sites_lake = all_data %>%
  filter(DAYS >= 24) %>%
  select(MONTH, DAY, YEAR,
         SEASON, WATER, 
         SITE, SPECIES,
         LENGTH,  effort_soak,YSAMP, GEAR_CODE) %>%
  group_by(WATER, SEASON, MONTH, DAY, YEAR, SITE, SPECIES, YSAMP,effort_soak, GEAR_CODE) %>%
  count() %>%
  mutate(n = replace(n,SPECIES=="NF",0)) %>%
  ungroup() %>% 
  select(-MONTH) %>%
  group_by(WATER, SITE, YEAR) %>%
  complete(.,nesting(SEASON, GEAR_CODE, effort_soak, DAY, YSAMP), SPECIES) %>%
  replace_na(list(n = 0, n = 0)) %>%
  group_by(WATER, SEASON,YEAR, SITE, effort_soak, YSAMP) %>% 
  summarise(total_fish = sum (n)) %>%
  group_by(WATER,SEASON, YEAR) %>%
  summarise_at(c("total_fish", "effort_soak"), sum) %>%
  mutate(LAKE_CPUE = total_fish / effort_soak)%>%
  select(c(-total_fish, -effort_soak))


total_CPUE_lakes = night_sets_sites_lake %>% 
  filter(SEASON == "summer",
         WATER != "COM")

total_CPUE_HRM  = night_sets_sites_lake %>%
  filter(SEASON == "fall", 
         WATER != "WFL",
         WATER != "PRL") 
  
CPUE_graph = night_sets_sites_lake %>%
  filter(WATER != "COM", 
         WATER != "WFL", 
         SEASON != "spring")
CPUE_graph = CPUE_graph[-7,]



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

night_sets_cpueavg[dim(night_sets_cpueavg)[1]+1,] = list("ETL", "summer","2021","BND",0)
night_sets_cpueavg[dim(night_sets_cpueavg)[1]+1,] = list("WFL", "summer","2021","BND",0)
night_sets_cpueavg[dim(night_sets_cpueavg)[1]+1,] = list("HRM_SHI", "summer","2021","BB",0)
night_sets_cpueavg[dim(night_sets_cpueavg)[1]+1,] = list("HRM_FOR", "summer","2021","NRD",0)
night_sets_cpueavg[dim(night_sets_cpueavg)[1]+1,] = list("HRM_BW2", "summer","2021","PS",0)
night_sets_cpueavg[dim(night_sets_cpueavg)[1]+1,] = list("HRM_BAR", "summer","2021","CC",0)
night_sets_cpueavg[dim(night_sets_cpueavg)[1]+1,] = list("ETL", "fall","2021","BND",0)
night_sets_cpueavg[dim(night_sets_cpueavg)[1]+1,] = list("WFL", "spring","2021","BND",0)
night_sets_cpueavg[dim(night_sets_cpueavg)[1]+1,] = list("HRM_SHI", "spring","2021","BB",0)


night_sets_cpueavg[dim(night_sets_cpueavg)[1]+1,] = list("HRM_FOR", "spring","2021","NRD",0)
night_sets_cpueavg[dim(night_sets_cpueavg)[1]+1,] = list("HRM_BW2", "spring","2021","PS",0)
night_sets_cpueavg[dim(night_sets_cpueavg)[1]+1,] = list("HRM_BAR", "spring","2021","CC",0)
night_sets_cpueavg[dim(night_sets_cpueavg)[1]+1,] = list("TPP", "fall","2021","BND",0)
night_sets_cpueavg[dim(night_sets_cpueavg)[1]+1,] = list("LOP", "fall","2021","NRD",0)
night_sets_cpueavg[dim(night_sets_cpueavg)[1]+1,] = list("COM", "fall","2021","NRD",0)


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

## Size structure historgrams across lakes 
all_data %>% ggplot(aes(x = LENGTH)) + geom_histogram(bins = 25) + 
  theme_minimal() + xlab("Total Length (mm)") + 
  ylab("Frequency") +
  facet_wrap(~WATER, scales="free_y") 

## Specifically looking at size structure in COM
all_data %>% filter(WATER == "COM", SPECIES %in% c("CC", "NRD")) %>%
  ggplot(aes(x = LENGTH)) + geom_histogram(bins = 30) + 
  facet_wrap(~MONTH + SPECIES, scales="free_y") +
  xlab("Combs Length (mm)")




## Difference in median total length across lakes 
all_data %>% 
  ggplot(aes(x = WATER, y = as.numeric(LENGTH))) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=90,hjust=01),
        text = element_text(size=12)) +
  #stat_summary(
  #  geom = "point",
   # fun= "median",
  #  col = "red",
   # size = 2,
  #  shape = 16,
  #  fill = "red") +
  facet_wrap(~SPECIES, scales = "free_y") + 
  ylab("Length (mm)") + 
  xlab("Species")
  


## Abundance of minnows 



## Age:Weight curves -----------------------------------------

# I just picked a couple of species that I had enough observations of weight for 
all_data %>% 
  filter(SPECIES %in% c("BB","CC","FHM","GS","NRD")) %>%
  ggplot(aes(y = LENGTH, x = as.numeric(WEIGHT), col = WATER)) + geom_point(alpha = .7) + 
  facet_wrap(~SPECIES, scales = "free") + 
  geom_smooth(se = F, method = "lm") + 
  theme_minimal() +
  xlab("Weight (g)") + ylab("Length (mm)")

all_data %>% filter(SPECIES == "CC") %>% 
  ggplot(aes(x = SPECIES, y = as.numeric(LENGTH))) + 
  geom_point() + 
  stat_summary(
    geom = "point",
    fun= "median",
    col = "red",
    size = 10,
    shape = "_",
    fill = "red") +
  ylab("Length (mm)") +
  facet_wrap(~WATER)


all_data %>% filter(SPECIES == "NRD") %>% 
  ggplot(aes(x = SPECIES, y = as.numeric(LENGTH))) + 
  geom_boxplot() +
  ylab("Length (mm)") +
  facet_wrap(~WATER)

median_length = all_data %>%
  group_by(SPECIES, WATER) %>%
  summarise(median_length = median(as.numeric(LENGTH),na.rm=T))

species_richness = all_data %>%
  filter(SPECIES != "NF") %>%
  select(WATER, SPECIES) %>%
  group_by(WATER) %>%
  unique() %>% 
  summarize(Richness = n())



minnow_richness = all_data %>% 
  filter(SPECIES %in% c("GS", "NRD","CC","BND","FHM","FSD-NRD","NPD","BM","CS","CLM","FSD")) %>% 
  group_by(WATER) %>%
  select(WATER, SPECIES) %>%
  unique() %>% 
  summarize(min_rich = n())

added = data.frame(WATER = c("LML","HRM_UB"), Richness = c(15, 7), min_rich = c(2,4))
prop_minnow = left_join(species_richness, minnow_richness) %>% 
  rbind(added) %>%
  mutate(prop.min = min_rich / Richness) 



median_length_z = all_data %>%
  group_by(SPECIES) %>%
  mutate(zscore = (LENGTH - median(LENGTH, na.rm=T))/sd(LENGTH, na.rm=T))

left_join(median_length_z, species_richness) %>%
  filter(SPECIES %in% c("BB","BND","CC","FHM","FSD-NRD","GS","NPD","NRD","PS","ST","WS")) %>% 
  ggplot(aes(x = Richness, y = zscore)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "black") +
  ylab("zscore Length (mm)") + 
  theme_minimal() + 
  facet_wrap(~SPECIES, scales = "free_y")


dat = left_join(median_length_z, species_richness) %>%
  filter(SPECIES == "BB")
  select(Richness, zscore, SPECIES)

plot(dat$zscore~ dat$Richness)
abline(lm(dat$zscore ~ dat$Richness))
summary(lm(dat$zscore ~ dat$Richness))

## Length analysis 

lengths = all_data %>% select(WATER, SPECIES, LENGTH, SEASON) %>%
  group_by(SEASON, WATER, SPECIES) %>%
  summarise(mean_length = mean(LENGTH, na.rm = T))

length_graph = lengths %>% 
  filter(SEASON != "spring",
         WATER != "COM",
         WATER != "WFL")

## Pulling out 2020 and 2021 length/weight data -------
## This is different than what I want to use for the standardized MWT sampling 

#all_fish = read.csv(file = "MA2276_Code/Data/Dissertation_Isotopes/SIA_updated_FINAL.csv") 

#all_fish %>% select(CODE,LENGTH,WEIGHT, Sample_ID)

#### --------------------- Questions to be answered ---------------------------

## Does the size of a species depend on how rich a community is? 
## I took ST out of this because i haven't included the sizes from the gillnets 
left_join(median_length, species_richness) %>%
  filter(SPECIES %in% c("BB","CC","BND","FHM", "WS", "NRD","GS")) %>%
  ggplot(aes(y = Richness, x = median_length)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, color = 1) +
  theme_minimal() +
  facet_wrap(~SPECIES)


## Difference in median total length across lakes separated by species 
all_data %>% 
  ggplot(aes(x = WATER, y = as.numeric(LENGTH))) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=90,hjust=01),
        text = element_text(size=12)) +
  facet_wrap(~SPECIES, scales = "free_y") + 
  ylab("Length (mm)") + 
  xlab("Species")

## Difference in median total length across lakes 
all_data %>% 
  ggplot(aes(x = WATER, y = as.numeric(LENGTH))) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=90,hjust=01),
        text = element_text(size=12)) +
  ylab("Length (mm)") + 
  xlab("Species")

## Difference in median total length across lakes (Long Pond and TPP have higher ranges of lengths)
all_data %>% 
  group_by(WATER) %>% 
  summarize(ran = max(LENGTH, na.rm = T) - min(LENGTH, na.rm =T)) %>%
  ggplot(aes(x = WATER, y = ran)) + geom_point() + 
  theme_minimal()



## Does the size of a species depend on the abundance of other (interspecific) fish present 

night_sets_cpueavg 


## NMDS ------------------


library(vegan)
nmds.comm = night_sets_cpueavg %>% pivot_wider(names_from = SPECIES, values_from = CPUE_hour) %>%
  mutate_all(~replace(., is.na(.), 0)) %>% 
  unite("ID", c(WATER, YEAR, SEASON)) %>%
  column_to_rownames("ID") %>% mutate(rowsum = rowSums(.)) %>% 
  filter(rowsum > 0) %>% 
  select(-rowsum)

comm.data.nmds = metaMDS(nmds.comm, k = 2, try = 20)

goodness(comm.data.nmds)
stressplot(comm.data.nmds)

data = comm.data.nmds$points  %>% as.data.frame() %>% rownames_to_column(var = "ID")
species = comm.data.nmds$species %>% as.data.frame() %>% rownames_to_column(var = "ID")


ggplot() + geom_text(data = data,
                     aes(x = MDS1, y = MDS2, label = ID), size = 2) + 
  geom_text(data = species,
            aes(x = MDS1, y = MDS2, label = ID), 
            position=position_jitter(width=.2,height=.4), size = 3) +
  xlim(-3,2) + 
  theme_minimal() 
  

## ----------------------

# Dataframe of native vs. invasive 
invasive_native = data.frame(SPECIES = c("CC","NRD","BND","BB","FSD-NRD","PS", "FHM","FSD","NPD","BK",     
                                       "BM"     , "CS"   ,   "GS"   ,   "ST"   ,   "WS",     
                                       "CLM"  , "cray" , "RBS"),
                             status = c("native", "native", "native","native","native","native","nonnative","native",
                                        "native","nonnative" ,"native",  "native","nonnative", "native","native","native","native","native"))


## Summarized ratio of non-native:native species --------------
night_sets_cpueavg %>% left_join(invasive_native) %>% 
  group_by(WATER, SEASON, YEAR, status) %>% 
  summarize(CPUE = sum(CPUE_hour)) %>% 
  
  pivot_wider(values_from = CPUE, names_from = status) %>%
  mutate_all(~replace(., is.na(.), 0)) %>% 
  filter(native != 0 ) %>% 
  mutate(native_nonnative =nonnative/native) %>%
  ggplot(aes(x = WATER, y = native_nonnative, color =SEASON)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90))+
  ylab("Non-native : Native") + 
  geom_point() 

## Facet wrap version of above graph - without ratios   
night_sets_cpueavg %>% left_join(invasive_native) %>% 
  mutate() %>%
  group_by(WATER, SEASON, YEAR, status) %>% 
  summarize(CPUE = sum(CPUE_hour)) %>% 
  unite("ID", c(SEASON, YEAR)) %>%
  filter(CPUE > 0) %>% 
  ggplot(aes(x = ID, y = CPUE )) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) + 
  geom_point(aes(color = status)) + 
  facet_wrap(~WATER, scales = "free_y")

## species richness -----------

species_richness = all_data %>%
  select(WATER, SPECIES) %>%
  unique() %>%
  filter(SPECIES !="NF") %>%
  group_by(WATER) %>% 
  summarize(richness = n())
species_richness[2,2] = 2 ## Adding in ETL species richness
species_richness[8,2] = 5 ## Adding in PRL species richness
invasive_richness = all_data %>%
  select(WATER, SPECIES) %>% 
  unique() %>%
  left_join(invasive_native) %>% 
  group_by(WATER, status) %>% summarize(richness = n()) %>%
  na.omit() 
invasive_richness[2,3] = 2
invasive_richness[15,1] = "PRL"
invasive_richness[15,2] = "nonnative"
invasive_richness[15,3] = 2

species_richness %>% ggplot(aes(x = WATER, y = richness)) + 
  geom_point() + 
  theme_minimal()

# ++ Visualize the communities from each lake by season ----




## all_data visualizations ---- 

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

## I don't like that the nightsamps have the completed seasons
## Now I think this works but if we catch a species in spring but not fall it doesn't add that species in... should go through and see if thats necessary to add

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
  group_by(WATER, YEAR, SEASON) %>%
  complete(.,nesting(SITE, GEAR_CODE, effort_soak, DAY, YSAMP), SPECIES) %>%
  replace_na(list(n = 0, n = 0)) %>%
  group_by(WATER, SEASON,YEAR, SPECIES) %>% 
  summarise_at(c("n", "effort_soak"), sum) %>%
  mutate(CPUE_hour = n / effort_soak) %>%
  select(c(-n, -effort_soak)) %>%
  filter(SPECIES != "NF") 


for(i in unique(night_sets_cpueavg$WATER)){
  g = night_sets_cpueavg %>% 
    filter(WATER == i) %>%
    group_by(SEASON, YEAR, SPECIES) %>%
    summarise(CPUE = sum(CPUE_hour)) %>%
    ggplot(aes(x = SPECIES, y = CPUE)) + 
    geom_bar(stat="identity", width = .4, position=position_dodge()) + 
    theme(axis.text.x=element_text(angle=90,hjust=1),
          text = element_text(size=16)) + 
    ylim(0,.45) +
    facet_wrap(~ SEASON) + 
    ggtitle(paste(i, "Community"))
  print(g)
}

occurance_data = nmds.comm %>%
  replace(.>0,1) %>% colSums() %>% as.data.frame() %>% rownames_to_column(var = "Species") %>% rename(., "occur"= .)

## trying to get proportion and species co-occurrence within 

all_data %>%
  filter(WATER != "ETL") %>%
  select(WATER) %>% unique()

iso_proportion = all_data %>%
  filter(WATER != "ETL") %>%
  select(WATER, SPECIES) %>%
  unique() %>%
  filter(SPECIES !="NF") %>%
  mutate(SPECIES = str_replace(SPECIES, "NPD", "PD")) %>%
  mutate(present = 1) %>%
  complete(WATER, SPECIES) %>%
  mutate(present = replace_na(present,0)) %>%
  group_by(SPECIES) %>%
  summarise(lakes = sum(present)) %>%
  mutate(lakes = case_when(SPECIES == "ST" ~ 6, SPECIES != "ST" ~ lakes))%>%
  mutate(lakes = case_when(SPECIES == "WS" ~ 4, SPECIES != "WS" ~ lakes)) %>%
  mutate(lakes = case_when(SPECIES == "PS" ~ 4, SPECIES != "PS" ~ lakes))%>%
  mutate(lakes = case_when(SPECIES == "CC" ~ lakes + 1, SPECIES != "CC" ~ lakes))%>%
  mutate(lakes = case_when(SPECIES == "CS" ~ lakes + 1, SPECIES != "CS" ~ lakes))%>%
  mutate(lakes = case_when(SPECIES == "NRD" ~ lakes + 1, SPECIES != "NRD" ~ lakes))%>%
  mutate(lakes = case_when(SPECIES == "BND" ~ lakes + 1, SPECIES != "BND" ~ lakes))%>%
  mutate(proportion_iso = lakes / 11) %>%
  rename(Species = SPECIES)

LML_comm = data.frame(Species = c("SMB", "MM","LLS","RS"), 
                      lakes = c(1,1,2,2)) %>%
  mutate(proportion_iso = lakes / 11)
  

iso_proportion = rbind(iso_proportion, LML_comm)

## co-occurrence in sampled lakes 


cooccurr_iso =all_data %>%
  filter(WATER != "ETL") %>%
  select(WATER, SPECIES) %>%
  unique() %>%
  filter(SPECIES !="NF") %>%
  mutate(SPECIES = str_replace(SPECIES, "NPD", "PD")) %>%
  mutate(present = 1) %>%
  complete(WATER, SPECIES) %>%
  mutate(present = replace_na(present,0)) %>%
  mutate(species = SPECIES) %>%
  ungroup() %>%
  filter(present>0) %>%
  group_by(WATER) %>%
  complete(SPECIES, species) %>%
  mutate(present = replace_na(present, 1)) %>%
  filter(SPECIES != species) %>%
  ungroup() %>%
  group_by(SPECIES, species) %>%
  summarize(cooccurr = sum(present)) %>%
  rename(Species = SPECIES, s2 = species)
