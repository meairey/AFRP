## Below code is used for looking at size at age for SMB

# Load in libraries 
library(dplyr)
library(ggplot2)

# Load in data
age = read.csv("MA2276_Code/Data/FISH_AGE_GROWTH.csv", header = TRUE) # age data
fish = read.csv("Data/FISH_MEASUREMENT.csv", header=TRUE) # individual measurements
samp = read.csv("Data/FISH_SAMPLE.csv", header = TRUE) # sampling events

# Select lake - make sure to use acronym for each lake
lake = "LML" ## FBL = First Bisby Lake, LML = Little Moose Lake 
species = "SMB" ## This is set up for SMB, but could be changed by altering this line

# Join data using individual fish IDs (FISH_N)
smb = left_join(age,fish, by = "FISH_N") %>% 
  left_join(samp) %>%
  select( YSAMP_N, FISH_N, SPECIES, LENGTH, WEIGHT, FINAL_AGE, WATER, YEAR) %>%
  mutate(ratio = FINAL_AGE/LENGTH) %>%
  filter(SPECIES == species, 
         WATER == lake)

# Histogram of lengths 
hist(smb$FINAL_AGE, 
     main = paste("Hist of Age",lake))

# Size at age through time
smb %>%
  filter(FINAL_AGE < 8) %>%
  mutate(FINAL_AGE = paste("YEAR", as.factor(FINAL_AGE))) %>%
  ggplot(aes(x = YEAR, LENGTH)) + geom_point() + 
  geom_smooth(method = "lm") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  xlim(2002, 2015) +
  facet_wrap(~FINAL_AGE) + # Creates the panes for each year class
  ggtitle(paste(species, "Size at Age",lake))

# Data table to look at how many otoliths of young year classes 
year_age = smb %>% filter(FINAL_AGE < 2) %>%
  group_by(FINAL_AGE, YEAR) %>%
  summarise(n())
year_age

# Median length at age data table
age_length = smb %>% 
  filter(FINAL_AGE != "NA") %>%
  group_by(FINAL_AGE) %>%
  summarise(median_L = median(LENGTH))

# Graphing above data table
age_length %>% ggplot(aes(x = FINAL_AGE, y = median_L)) + geom_point() + 
  ggtitle(paste(species,"Length ~ Age", lake)) + xlim(0,13) + ylab("Median Length") + 
  xlab("Age")
