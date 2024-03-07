library(rfishbase)
library(tidyr)
library(dplyr)
library(ggplot2)

## Working Directory 




## Data setup ----------------------------
## Define species of interest
## ALSC Dataa
setwd("C:/Users/monta/OneDrive - Airey Family/Cornell/ALSC Analysis")
alsc = read.csv("ALSCDATA_fish_altered.csv", header=T)%>%
  replace(is.na(.),0 ) %>%
  mutate(Richness = rowSums(.[,1:54])) %>%
  cbind(.,read.csv("ALSCDATA_chem_altered.csv",header=T)) %>%
  select(-NF) %>%
  #rename(Long = LONG..NAD83.DD.)%>%
  mutate(LONG = LONG*-1)%>%
  filter(Richness != 0 )

# Fishbase data
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP")
fish_species = c("Margariscus nachtriebi", "Margariscus margarita","Luxilus cornutus","Chrosomus eos","Notemigonus crysoleucas","Pimephales promelas","Rhinichthys atratulus","Semotilus atromaculatus" ,"Couesius plumbeus","Rhinichthys cataractae","Hybognathus hankinsoni","Hybognathus regius","Pimephales notatus","Notropis bifrenatus", "Exoglossum maxillingua","Chrosomus neogaeus","Notropis volucellus","Semotilus corporalis")
length(fish_species)
## Define fish codes
fish_codes = c("PD", "PD","CS", "NRD","GS","FHM","BND","CC","LC","LND","BM","ESM","BRM","BS","CLM","FSD","MCS","FF") %>% cbind(fish_species) %>% as.data.frame() %>% rename(Species = fish_species) %>% rename(code = '.')
cbind(fish_codes, fish_species)
codes = cbind(fish_codes, fish_species)

# Read in fish base data 
whole_frame = read.csv("minnow_data.csv")

## Put together the fish base data and the ALSC data 
df = alsc %>% 
  select(SO4_mgL, Volume.m3., DIC, DOC, P_total, Littoral.Area..ha., Elevation..m., Volume.m3., LAB_pH, codes$code,) %>% 
  mutate(lakeID = row_number()) %>%
  select(lakeID, everything()) %>% 
  pivot_longer(10:(length(codes$code)+8), names_to = "code", values_to = "ABUNDANCE") %>%
  left_join(whole_frame)

## Which are the most common herbivory group? 

## Omnivores are the most diverse group of minnows in the Adirondacks 
whole_frame %>% group_by(Herbivory2) %>%
  summarize(n = n())  %>%
  ggplot(aes(x = Herbivory2, y = n)) + geom_bar(stat = 'identity') + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("Species Richness") 
  
## Omnivores are often highest abundance as well 
df %>% filter(ABUNDANCE > 0) %>%
  select(Herbivory2, lakeID, ABUNDANCE) %>%
  group_by(Herbivory2, lakeID) %>%
  summarize(total_abund  = sum(ABUNDANCE)) %>% 
  ggplot(aes(x = Herbivory2, y = total_abund)) + geom_boxplot() +theme(axis.text.x = element_text(angle = 90))


## Does elevation influence the abundance of different guilds of fish (based on herbivory)

# yes, there is a higher abundance of omnivore at higher elevations
# Figure
df %>% 
  filter(ABUNDANCE > 0) %>% 
  filter(Herbivory2 != "NA") %>% 
  ggplot(aes( y = ABUNDANCE, 
              x = Elevation..m.)) + geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(~Herbivory2)

# basic LM model - should be something non-linear if you eve publish 
df %>% 
  filter(ABUNDANCE > 0) %>% 
  filter(Herbivory2 == "plants/detritus+animals (troph. 2.2-2.79)") -> herb.animal

summary(lm(data = herb.animal, ABUNDANCE~Elevation..m. ))
summary(lm(data = herb.animal, ABUNDANCE~Littoral.Area..ha.))

#### Fishing 
## I've tried:
## Interesting - Elevation, maybe labPH, littoral area, volume (I can't figure out the right regression)
## Not interesting -- SO4

df %>% 
  filter(ABUNDANCE > 0) %>% 
  filter(Herbivory2 != "NA") %>% 
  group_by(Volume.m3., Herbivory2) %>%
  summarize(mean = median(ABUNDANCE)) %>%
  ggplot(aes( y = mean, 
              x = Volume.m3.)) + geom_point() +
  geom_smooth(method = 'lm',  se=FALSE, color=2) + 
  facet_wrap(~Herbivory2)

df %>% 
  #filter(ABUNDANCE > 0) %>% 
  group_by(Littoral.Area..ha., Herbivory2) %>%
  summarize(mean = mean(ABUNDANCE)) %>%
  filter(Herbivory2 == "plants/detritus+animals (troph. 2.2-2.79)") -> herb.animal

summary(lm(data = herb.animal, mean~Littoral.Area..ha.))
summary(loess(data = herb.animal, mean~Littoral.Area..ha.))


## Perhaps - larger individuals are more abundant at greater elevations?
length_breaks = c(0,10,20,30,60)
df %>% 
  filter(ABUNDANCE > 0) %>%
  group_by(TL_cm, Elevation..m.) %>%
  summarize(ABUNDANCE = median(ABUNDANCE)) %>%
  mutate(TL_cm= .bincode(TL_cm, length_breaks)) %>%
  ggplot(aes(y = ABUNDANCE, x = Elevation..m.)) + 
  geom_point() + 
  xlim(0,750) +
  facet_wrap(~TL_cm) + 
  geom_smooth(method = "lm") 


## Trying richness of species 
cat = df %>% 
  filter(ABUNDANCE> 0) %>%
  group_by(Herbivory2, Elevation..m.,LAB_pH, lakeID) %>% 
  summarize(richness = n())

cat %>% ggplot(aes(x = Elevation..m., y = richness)) + geom_point() + 
  geom_smooth(method = lm) + 
  facet_wrap(~Herbivory2)

cat %>% filter(Herbivory2 == "plants/detritus+animals (troph. 2.2-2.79)") -> frog

summary(lm(frog$richness ~ frog$Elevation..m.))


dd = df %>% filter(ABUNDANCE> 0) %>% 
  select(lakeID, Elevation..m.,ABUNDANCE, Herbivory2, LAB_pH) %>% 
  group_by(lakeID, Elevation..m.,Herbivory2, LAB_pH) %>% 
  summarize(sum = sum(ABUNDANCE))


dd %>% 
  filter(Herbivory2 != "NA") %>% 
  ggplot(aes(col = LAB_pH, 
             y = sum, 
             x = Elevation..m.)) + geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(~)


filter(Herbivory2 == "plants/detritus+animals (troph. 2.2-2.79)") -> cat

summary(lm(cat$ABUNDANCE ~ cat$Elevation..m.))