## Libraries -------
library(dplyr)
library(tidyr)
library(tidyverse)
# Loading in data ---------
fish = read.csv("FISH_MEASUREMENT_whole.csv")
sample = read.csv("FISH_SAMPLE.csv")
sites = read.csv("SITES.csv")
forage_nat  = c("NRD","BND", "CS", "SS", "MM","CC", "WS")
forage_inv = "RS"
smb = "SMB"


### CPUE is zero not nonexistent when no individuals are sampled 

# Joining all data together ---------
all_data = left_join(fish, sample, by = "YSAMP_N") %>%
  left_join(sites, by = "SITE_N") %>%
  select(EFFORT, YSAMP_N,  SPECIES,  WATER,
          FISH_N,  WEIGHT, LENGTH,  SITE_N,  
          DEPTH,  YEAR, HAB_1, SEASON, GEAR) %>%
  separate(SITE_N,  into = c("gear", "water","site")) %>%
  mutate(site = as.numeric(site)) %>%
  filter(WATER == "LML",
         HAB_1 != "NA",
         HAB_1 != "", 
         YEAR >1996,
         site != "NA", 
         site < 33) 

# Filtering data for forage fish and NAs ---------------
filtered = all_data %>%
  filter(SPECIES %in% forage_nat)
  

## Calculating number of sites/habitat -------
Hab_numbs = all_data %>%
  select(HAB_1, site) %>%
  unique() %>%
  count(HAB_1)


## Creating data frame with habitat, year, # individuals -----
year_hab = filtered %>%
  group_by(YEAR, HAB_1) %>%
  count()

## Standardize # individuals/site/year by # sites per habitat ----
standard.yr.hab = left_join(year_hab, Hab_numbs, by = "HAB_1") %>%
  mutate(standard = n.x / n.y) 


## Plotting number of individuals per site 
filtered %>%
  group_by(YEAR, SPECIES, site) %>%
  count() %>%
  ggplot(aes(x = YEAR, y = (n))) + 
  geom_point() +
  geom_smooth(aes(x = YEAR, n)) +
  scale_y_continuous(trans='log2')

## Same as above but standardized 
standard.yr.hab %>%
  ggplot(aes(x = HAB_1, y = standard, fill = as.character(YEAR))) + geom_bar(stat = 'identity') + 
  ylab("standardized indv/hab") + 
  xlab("Habitat") + 
  labs(fill = "Year") + 
  ggtitle("Use of Habitat in Little Moose")


# Change in habitat use across time
filtered %>%
  select(site, YEAR, HAB_1) %>%
  unique() %>%
  group_by(HAB_1, YEAR) %>%
  count() %>%
  left_join(Hab_numbs, by = "HAB_1") %>%
  mutate(std = n.x/n.y*100) %>%
  ggplot(aes(y = std, x = YEAR, color = HAB_1)) + geom_point() + 
  geom_line(aes(color = HAB_1)) + 
  ylab("Percent of sampled sites occupied") + 
  xlab("Year") +
  labs(col = "Habitat") + 
  ggtitle("Habitats occupied across years")

##### Multiple Species Code -------------------------
# Filtering for desired species 

filtered = all_data %>%
  filter(SPECIES %in% c(forage_nat, smb, "PS", "LT", "ST", "LLS", "GNS"))

filtered_spring = filtered %>%
  filter(SEASON=="S")
filtered_fall = filtered %>%
  filter(SEASON == "F")

# Individuals a year 
filtered %>% 
  group_by(YEAR, SPECIES, HAB_1, EFFORT) %>%
  count() %>%
  left_join(Hab_numbs, by = "HAB_1") %>%
  mutate(n.standardized = n.x/n.y) %>%
  ggplot(aes(x = YEAR,y = n.standardized)) + 
  geom_point(aes(x = YEAR,  y = n.standardized, color = HAB_1))  +   geom_line(aes(color = HAB_1)) +
  geom_smooth(aes(x = YEAR, y = n.standardized), method = "lm") +
  scale_y_continuous(trans='log2') + 
  facet_wrap(~SPECIES) + 
  ggtitle("Abundance by habitat")


filtered %>% 
  group_by(YEAR, SPECIES, HAB_1, EFFORT) %>%
  count() %>%
  mutate(N.CPUE = n / EFFORT) %>%
  ggplot(aes(x = YEAR, y = N.CPUE)) +
  geom_point(aes(x = YEAR, y = N.CPUE,color = HAB_1)) +
  geom_smooth(aes(z = YEAR, y = N.CPUE)) + 
  scale_y_continuous(trans='log2') +
  facet_wrap(~SPECIES)








filtered %>%
  group_by(YEAR, SPECIES, HAB_1, SEASON) %>%
  mutate(avg.length = mean(LENGTH, na.rm=T)) %>%
  ggplot(aes(YEAR, y = avg.length, color = HAB_1)) + 
  geom_point() + 
  geom_smooth() + 
  scale_y_continuous(trans='log2') +
  facet_wrap(~SPECIES)


 filtered_spring %>%
  group_by(YEAR, SPECIES, HAB_1, SEASON) %>%
  mutate(avg.length = mean(LENGTH, na.rm=T)) %>%
  ggplot(aes(YEAR, y = avg.length)) + 
  geom_point(aes(color = HAB_1)) + 
  geom_smooth(method = "lm") + 
  scale_y_continuous(trans='log2') +
  facet_wrap(~SPECIES) + 
  ggtitle("Average length in Spring")

filtered_fall %>%
  group_by(YEAR, SPECIES, HAB_1, SEASON) %>%
  mutate(avg.length = mean(LENGTH, na.rm=T)) %>%
  ggplot(aes(YEAR, y = avg.length, color = HAB_1)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  scale_y_continuous(trans='log2') +
  facet_wrap(~SPECIES)+ 
  ggtitle("Average length in Fall")

species = "LT"
cat = filtered_fall %>% filter(SPECIES == species)

summary(lm(cat$LENGTH ~ cat$YEAR))

dog = filtered_spring %>% filter(SPECIES == species)
summary(lm(dog$LENGTH ~ dog$YEAR))

filtered_count = filtered %>% 
  group_by(SPECIES, YEAR, SEASON)

cat = filtered_count %>% filter(SPECIES == species, SEASON  == "F") %>% count()
summary(lm(cat$n ~ cat$YEAR))
dog = filtered_count %>% filter(SPECIES == species, SEASON  == "S") %>% count()
summary(lm(dog$n ~ dog$YEAR))


filtered %>%
  select(site, YEAR, HAB_1, SPECIES) %>%
  unique() %>%
  group_by(HAB_1, YEAR, SPECIES) %>%
  count() %>%
  left_join(Hab_numbs, by = "HAB_1") %>%
  mutate(std = n.x/n.y*100) %>%
  ggplot(aes(y = std, x = YEAR, col = HAB_1)) + geom_jitter() + 
  geom_smooth(aes(x = YEAR, y = std))+ 
  facet_wrap(~SPECIES) + 
  ylab("Percent Sampled Habitat Occupied")

filtered %>%
  select(site, YEAR, HAB_1, SPECIES) %>%
  unique() %>%
  group_by(HAB_1, YEAR, SPECIES) %>%
  count() %>%
  left_join(Hab_numbs, by = "HAB_1") %>%
  mutate(std = n.x/n.y*100) %>%
  ggplot(aes(y = std, x = YEAR)) + 
  geom_line(aes(color = HAB_1)) +
  ylab("Percent Habitat Use") +
  geom_smooth(aes(x = YEAR, y = std), se=FALSE, method = "lm") + 
  facet_wrap(~SPECIES)

## Effort modified ------------------------------


all_data$









## Scratch code with other components ------ 

## Data frame with habitats 
habitats = filtered %>%
  group_by(YEAR,SPECIES,site, HAB_1) %>%
  count()
## sites sampled across years 
all_data %>%
  select(site, YEAR, HAB_1) %>%
  unique() %>% 
  group_by(YEAR, HAB_1) %>%
  count() %>%
  ggplot(aes(x = HAB_1, y = n, fill = as.character(YEAR))) + geom_bar(stat = "identity") +
  labs(fill = "Year")
all_data %>% 
  select(site, YEAR, HAB_1) %>%
  unique() %>%
  ggplot(aes(x = as.character(YEAR),  y = site)) + geom_point() + 
  xlab("Year") + 
  ggtitle("Site Sampling Through Time")
## Plotting number of individuals per habitat across years 
### Note that there are some sites that might be there just we need to conenc GPS points 
filtered %>%
  group_by(YEAR,SPECIES,site, HAB_1) %>%
  count() %>%
  ggplot(aes(x = reorder(HAB_1, n), y = n, fill = as.character(YEAR))) +
  geom_bar(stat = "identity") + 
  ggtitle("Use of Habitat in Little Moose") + 
  labs(fill = "Year") +
  ylab("# of Fish") +
  xlab("Habitat Type")#+
#scale_x_discrete(labels = c("Rocky", "Rocky/Woody", "Silt", "Silty/Woody"))
## Mean number of individuals per site
filtered %>%  
  group_by(YEAR, SPECIES, site) %>%
  count() %>%
  group_by(YEAR) %>%
  summarise(mean = mean(n))
## sites per habitat type
all_data %>%
  select(HAB_1, site) %>%
  unique() %>%
  count(HAB_1) %>%
  ggplot(aes(x = reorder(HAB_1, n), y = n)) + geom_bar(stat = "identity")+
  scale_x_discrete(labels = c("Rocky/Woody", "Silty/Woody", "Silt", "Rock")) + 
  xlab("Habitat Type") + 
  ylab("Number of Sites") + 
  ggtitle("Number of Sites per Habitat")

## Increase in sites through years
# I think this is flawed because we would need this to be subset from all data which is what I did above but keeping just in case I was wrong 
all = filtered %>%
  select(site, YEAR, HAB_1) %>%
  unique() %>%
  group_by(HAB_1, YEAR) 

habitats = c("RW", "SW", "R","S")
for(i in habitats){
  p = all %>%
    filter(HAB_1 == i) %>%
    ggplot(aes(x = as.character(YEAR), y=site)) + geom_point()+
    ylab("site") + xlab("Year") + ggtitle(paste("Habitat",i))
  print(p)
}

t.test(
  habitats$n[which((habitats$YEAR == 2000) & (habitats$HAB_1 == "R"))],
  habitats$n[which((habitats$YEAR == 2001) & (habitats$HAB_1 == "R"))])

