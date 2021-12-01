### Libraries --------------

library(dplyr)
library(tidyr)
library(tidyverse)
library(vegan)
library(RColorBrewer)

### Data -------------------

fish = read.csv("../Data/FISH_MEASUREMENT.csv")
sample = read.csv("../Data/FISH_SAMPLE.csv")
sites = read.csv("../Data/SITES.csv")
shoreline_length = read.csv("../Data/BEFsites_LengthAndHabitat.csv")

## Functions source -----------

source(file = "../AFRP Master Code/AFRP_Functions.R")

## Graphics setup -------

colors = viridis(20)

## Filter data -----------
## Filter data for Little Moose, Boat electrofishing, in the spring. I've also filtered it to be yr 2000+ because earlier years had strange site labels. 

all_data = filter_data(water = "LML", gear = "BEF",
                       gear_code = "NAF", 
                       species = species) %>% 
  filter(MONTH %in% c(4,5,6,7,8), YEAR >= 2000)

## Removing rare + stocked taxa ------------ 
## Taxa get removed if they are rare or if they are stocked given that stocking would be an artificial manipulation of populations not relating to bass 

`%nin%` = Negate(`%in%`)
rare_threashold = 50 ## change this based on preference
rare = all_data %>% group_by(SPECIES) %>% 
  summarise(frequency = n()) %>% 
  filter(frequency < rare_threashold)
stocked = c("LLS", "RT") ## Stocked fish in little moose
remove = c(stocked, rare$SPECIES)
all_data = all_data %>% filter(SPECIES %nin% remove)

# Site averaged NMDS across sites -----------------
## This averages CPUE across sites and days within a given year

CPUE.w.sec.a = CPUE_wide_seconds_avg(all_data) %>% 
  column_to_rownames(., var = "YEAR") # Data setup
NMDS=metaMDS(CPUE.w.sec.a, # Our community-by-species matrix
                     k=2, try = 100) 

stressplot(NMDS) 
plot(NMDS)

ordiplot(NMDS,type="n", xlim = c(-1, 1), main = 'Site Averaged - Year')
orditorp(NMDS,display="sites",cex=.5,air=0.01)

ordiplot(NMDS,type="n", ylim = c(-1.2, .5), main = 'Site Averaged - Species')
orditorp(NMDS,display="species",col="red",air=0.01,)

 
# Day averaged NMDS across sites --------------
CPUE.w.sec = CPUE_wide_seconds(all_data) %>%
  unite("Group", c(YEAR, SITE)) %>% 
  column_to_rownames(., var = "Group") %>% 
  mutate(sumrow = rowSums(.)) %>%
  filter(sumrow>0) %>%
  select(-sumrow)


## Year Bins for NMDS plotting -------------
bins = c(1999, 2000, 2006, 2011, 2017, 2019) ## Define this yourself
names = bins[-1] ## This goes into the legend 
year_bin =  (data.frame(treat = rownames(CPUE.w.sec)) %>%
  separate(treat, into = c("Year", "Site")) %>% 
  mutate(Year = .bincode(as.numeric(.$Year),
                         ## Define desired breaks below
                         breaks = bins)) %>% 
  select(Year))$Year
#c(1999,2000, 2005, 2012, 2019)))
#breaks = c(1999,2000, 2009, 2011, 2019)))

color = as.character(year_bin) %>% as.factor() %>% as.numeric()

NMDS.d=metaMDS(CPUE.w.sec, # Our community-by-species matrix
             k=3) 

stressplot(NMDS.d)
plot(NMDS.d)

## The plot for years here is by site and it looks terrible. Not worth looking at. 


ordiplot(NMDS.d,type="n", main = 'Day Averaged - Species')
for(i in unique(year_bin)) {
  ordiellipse(NMDS.d$point[grep(i,year_bin),],draw="polygon",kind=c("sd"),
           groups=year_bin[year_bin==i],col=color[grep(i,year_bin)],label=F) } 

orditorp(NMDS.d,display="species",col="black",air=0.01)
legend("bottomright", legend = names, col = unique(color), lty= rep(1, 5), title = "Year Breaks")




## Group by 5-6 yr intervals? Make those groupings larger 


## change points on nmds based on location. Dots are sites - change color 
## IF we havent measured the size of a fish - they measure every year, but they don't measure every fish - look at a way to resample that size distribution for the year. Use distribution and randomly sample it. 

## NMDS for size structure - bin by size class, across time (communities) - size class of each fish species would be the species - no actualy, do the whole community, so each species is a size bin (abundance within the bin) tells you something about ecotrophic efficiency. Do for both lake trout from the gill net and also the whole community from the BEF. 
## mess around with the CPUE through time analysis, is there some first derivitave we could do? look that up 
## response time - think about it but what is the inflection point in the curve where the sppecies start to show some response. 
## Then could we plot response timing and body size/age at maturation 
## Gillnet or trapnets 


### Removed from AFRP functions keeping in case i need it again...

%>% 
  pivot_wider(names_from = SPECIES, values_from = CPUE_seconds) %>%
  filter(HAB_1 != 0)%>%
  mutate(across(everything(), ~replace_na(.x,0)))
