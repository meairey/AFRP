library(lattice)
library(MASS)
library(dplyr)
#install.packages("pscl")
require(pscl) # alternatively can use package ZIM for zero-inflated 
library(lmtest)
library(dplyr)
library(tidyr)
library(tidyverse)
library(vegan)
library(RColorBrewer)
library(ggridges)
library(ecp)


setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP")

source("AFRP_Master_Code/AFRP_Functions.R")
sample = read.csv("Data/FISH_SAMPLE_edited.csv") %>% select(YSAMP_N, WATER, YEAR, MONTH, EFFORT, EFFORT_UNIT, GEAR, SEASON)

fish_measurement = read.csv("Data/FISH_MEASUREMENT.csv") %>% select(YSAMP_N, SPECIES, LENGTH)

data = left_join(fish_measurement, sample) 

data %>% 
  filter(WATER %in% c("LSP", "USP"), GEAR == "TPN")%>% 
  group_by(YEAR, WATER, MONTH, SPECIES, EFFORT, SEASON) %>%
  filter(MONTH < 7 ) %>%
  summarize(count = n()) %>% filter(SPECIES != "NF") %>%
  mutate(CPUE_min = count/EFFORT) %>%
  ungroup() %>%
  group_by(YEAR, WATER, MONTH, SPECIES, SEASON) %>% 
  summarize(CPUE_min = sum(CPUE_min)) %>%
  ggplot(aes(x = YEAR, y = CPUE_min, color = SPECIES)) + 
  geom_point() + 
 # geom_smooth( se = F) + 
  facet_wrap(~WATER) + 
  ggtitle("Spring TPN")

data %>% 
  filter(WATER %in% c("LSP", "USP") , GEAR == "TPN")%>% 
  group_by(YEAR, WATER, MONTH, SPECIES, EFFORT, SEASON) %>%
  filter(MONTH > 7 ) %>%
  summarize(count = n()) %>% filter(SPECIES != "NF") %>%
  mutate(CPUE_min = count/EFFORT) %>%
  ungroup() %>%
  group_by(YEAR, WATER, MONTH, SPECIES, SEASON) %>% 
  summarize(CPUE_min = sum(CPUE_min)) %>%
  ggplot(aes(x = YEAR, y = CPUE_min, color = SPECIES)) + 
  geom_point() + 
  geom_smooth( se = F) + 
  facet_wrap(~WATER) + 
  ggtitle("Fall TPN")


data %>% filter(WATER == "USP") %>% 
  group_by(YEAR, MONTH, GEAR, SPECIES) %>% 
  summarize(count =n()) %>%
  ggplot(aes(x = YEAR, y = count)) + 
  geom_point()
