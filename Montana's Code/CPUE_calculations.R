## Libraries -------------------------------
library (ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(gridExtra)

## Data ------------------------------------
fish = read.csv("FISH_MEASUREMENT_whole.csv")
sample = read.csv("FISH_SAMPLE.csv")
sites = read.csv("SITES.csv")
shoreline_length = read.csv("BEFsites_LengthAndHabitat.csv")

## Filter and Join Data 
all_data = left_join(fish, sample, by = "YSAMP_N") %>% 
  left_join(sites, by = "SITE_N") %>% 
  left_join(shoreline_length, by = "SITE_N") %>%
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
         YEAR > 1999, 
         MONTH < 8
  )

## CPUE calculations (seconds) -------------------------------

CPUE_wide = all_data %>%
  select(YSAMP_N, DSAMP_N, YEAR, SEASON, WATER, SITE, SPECIES,
         FISH_N, WEIGHT, LENGTH, HAB_1, GEAR, EFFORT) %>%
  group_by(WATER, DSAMP_N, YEAR, SITE, SPECIES, EFFORT, HAB_1) %>%
  count() %>% ## Abundance per year, site, species
  mutate(CPUE_seconds = n / EFFORT) %>%
  select(-n) %>%
  pivot_wider(names_from = SPECIES, values_from = CPUE_seconds) %>%
  mutate(across(everything(), ~replace_na(.x,0)))



CPUE_long = CPUE_wide %>%
  pivot_longer(cols = LLS:length(CPUE_wide),names_to = "SPECIES", values_to = "CPUE_seconds" )
head(CPUE_long)


## CPUE calculations (shoreline length) -----------------------
CPUE_wide_shore = all_data %>%
  select(YSAMP_N, DSAMP_N, YEAR, SEASON, WATER, SITE, SPECIES,
         WEIGHT, LENGTH, HAB_1, GEAR, EFFORT, Shape_Length) %>%
  group_by(WATER, DSAMP_N, YEAR, SITE, SPECIES, EFFORT, HAB_1, Shape_Length) %>%
  count() %>% ## Abundance per year, site, species
  mutate(CPUE_shoreline = n / Shape_Length) %>%
  select(-n) %>%
  pivot_wider(names_from = SPECIES, values_from = CPUE_shoreline) %>%
  mutate(across(everything(), ~replace_na(.x,0)))

CPUE_long_shore = CPUE_wide_shore %>%
  pivot_longer(cols = LLS:length(CPUE_wide_shore),names_to = "SPECIES", values_to = "CPUE_shoreline" )

