### Libraries --------------

library(dplyr)
library(tidyr)
library(tidyverse)
library(vegan)
library(RColorBrewer)

### Data -------------------
# This is just setup at the project working directory. Use option in upper right corner of R to get into project directory. For example, on my computer ,its stored in my family one-drive
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP")

## I think these can go they should pop up from the source line
#fish = read.csv("Data/FISH_MEASUREMENT.csv")
#sample = read.csv("Data/FISH_SAMPLE.csv")
#sites = read.csv("Data/SITES.csv")
#shoreline_length = read.csv("Data/BEFsites_LengthAndHabitat.csv")

## Functions source -----------
source("AFRP_Master_Code/AFRP_Functions.R")


## Graphics setup -------

#colors = viridis(20) detele this if code runs no problem

## Year Bins for NMDS plotting 
### At this point - playing around with 5-6yr bins
bins = c(1999, 2000, 2005, 2010, 2015, 2019) ## Define this yourself
names = bins[-1] ## This goes into legends for visualization 
year_bin =  (data.frame(treat = c(2000:2019)) %>%
               separate(treat, into = c("Year", "Site")) %>% 
               mutate(Year = .bincode(as.numeric(.$Year),
                                      ## Define desired breaks below
                                      breaks = bins)) %>% 
               select(Year))$Year
#c(1999,2000, 2005, 2012, 2019))) old options for breaks
#breaks = c(1999,2000, 2009, 2011, 2019))) old options for breaks

## Filter data -----------

## Filter data for Little Moose, Boat electrofishing, in the spring. I've also filtered it to be yr 2000+ because earlier years had strange site labels. 

#tpn_data = filter_data(water = "LML", gear = "TPN" ,
                       
                       #species = species) ### You need to figure out why this isnt workin in the filter data function!! 

# Boat Electrofishing Data
BEF_data_unfiltered = filter_data(water = "LML", gear = "BEF",
                       gear_code = "NAF", 
                       species = species) %>% 
  filter(MONTH %in% c(4,5,6,7,8), YEAR >= 2000)

# Trap Net Data
tpn_data = left_join(fish, sample, by = "YSAMP_N") %>% 
  left_join(sites, by = "SITE_N") %>% 
  left_join(shoreline_length, by = "SITE_N") %>%
  separate(SITE_N,  into = c("GEAR", "WATER","SITE")) %>% 
  filter(WATER == "LML",
         GEAR == "TPN",
         YEAR >= 2000)

# Gillnet Data
gln_data = left_join(fish, sample, by = "YSAMP_N") %>% 
  left_join(sites, by = "SITE_N") %>% 
  left_join(shoreline_length, by = "SITE_N") %>%
  separate(SITE_N,  into = c("GEAR", "WATER","SITE")) %>% 
  filter(WATER == "LML", 
         GEAR == "GLN", 
         YEAR >= 2000)

# Removing rare + stocked taxa ------------ 
## Taxa get removed if they are rare or if they are stocked given 
### that stocking would be an artificial manipulation of populations 
### not relating to bass. SMB are also removed to reduce how they swamp community analysis 

`%nin%` = Negate(`%in%`) # sets up a way to exclude if in a string
rare_threashold = 50 ## change this based on preference
rare = BEF_data %>% group_by(SPECIES) %>% 
  summarise(frequency = n()) %>% 
  filter(frequency < rare_threashold)
stocked = c("LLS", "RT") ## Stocked fish in little moose
remove = c(stocked, rare$SPECIES, "RWF", "SMB") ## Remove smallmouth bass + RWF (targetted in early 2000s)
BEF_data = BEF_data_unfiltered %>% filter(SPECIES %nin% remove)

# Site averaged NMDS across sites -----------------
## This averages CPUE across sites and days within a given year
### Notes
#### NMDS$species = species
#### NMDS$points = years

CPUE.w.sec.a = ((CPUE_wide_seconds_avg(BEF_data) %>% 
  column_to_rownames(., var = "YEAR"))) # Data setup
  
NMDS.cpue_year=metaMDS(CPUE.w.sec.a, # Our community-by-species matrix
                     k=2, try = 100) 

stressplot(NMDS.cpue_year) 
plot(NMDS.cpue_year)

NMDS.cpue_year$species %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  as.data.frame() %>%
  ggplot(aes(x = MDS1, y = MDS2, label = rownames(NMDS.cpue_year$species))) + 
  geom_text(size =4)+ 
  ggtitle("Species - averaged across sites")

NMDS.cpue_year$points %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  cbind(., Year= as.numeric(rownames(.))) %>%
  as.data.frame() %>%
  arrange(Year) %>% 
  ggplot(aes(x = MDS1, y = MDS2, label = Year)) + 
  geom_text(aes(color = as.numeric(Year)), size =4) + 
  scale_color_gradientn(colours = rainbow(5)) +
  labs(color = "Year") + 
  ggtitle("Years - averaged across sites")

NMDS.cpue_year$points %>% as.data.frame() %>% mutate(Year = row.names(NMDS.cpue_year$points)) %>% write_csv(file = "NMDS_points.csv") ## CVS Tommy asked for 

NMDS.cpue_year$points %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  cbind(., Year= as.numeric(rownames(.))) %>%
  as.data.frame() %>%
  arrange(Year) %>%
  mutate(Year_bin = .bincode(Year, bins)) %>% 
  ggplot(aes(x = MDS1, y = MDS2, label = Year, color = as.character(Year_bin))) + 
  geom_text(size =4) + 
  stat_ellipse() +
  labs(color = "Year") + 
  ggtitle("LOG+1: Years - averaged across sites")  + 
  scale_color_manual(labels =  bins[2:6], values= unique(year_bin))

# Day averaged NMDS across sites --------------

# Data setup
CPUE.w.sec = ((CPUE_wide_seconds(BEF_data) %>%
  unite("Group", c(YEAR, SITE)) %>% 
  column_to_rownames(., var = "Group") %>% 
  mutate(sumrow = rowSums(.)) %>%
  filter(sumrow>0) %>%
  select(-sumrow)))

# Running NMDS
NMDS.cpue_siteyear=metaMDS(CPUE.w.sec, # Our community-by-species matrix
             k=5) 

# Diagnostic plots 
stressplot(NMDS.cpue_siteyear)
plot(NMDS.cpue_siteyear)

NMDS.cpue_siteyear$species %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  as.data.frame() %>%
  ggplot(aes(x = MDS1, y = MDS2, label = rownames(NMDS.cpue_year$species))) + 
  geom_text(size =4)+ 
  ggtitle("No SMB")


NMDS.cpue_siteyear$points %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  cbind(., Year= rownames(.)) %>%
  separate(Year, into = c("Year","Site"), remove = F) %>%
  as.data.frame() %>%
  arrange(Year) %>% 
  mutate(Year = .bincode(Year, bins )) %>%
  ggplot(aes(x = MDS1, y = MDS2, color = as.character(Year))) + 
  geom_point(size =1) + 
  stat_ellipse() +
  labs(color = "Year") + 
  ggtitle("Years: by site") + 
  scale_color_manual(labels =  bins[2:6], values= unique(year_bin))



## Size structure NMDS --------------------------------------------------------

l = BEF_data %>% select(SPECIES, LENGTH, SITE, YEAR) %>% 
  na.omit() %>% mutate(LENGTH = (LENGTH)) %>% filter(YEAR != 2002) ## Modified to remove 2002 and transform length by the sqrt 

hist(log(l$LENGTH)) ##** 
hist(log10(l$LENGTH+1))
hist(sqrt(l$LENGTH))
hist(l$LENGTH)

# Define weight/length matrix, remove NAs

# .  Binning data -----------

## Create bins


## New bins 
bin_size = 20
l.r = seq(from =range(l$LENGTH)[1]-.1, to =  range(l$LENGTH)[2], by = bin_size)
l.r[length(l.r)+1] = range(l$LENGTH)[2] + .1

## Bin data frame 

length_frame = l %>%  
  mutate(length_bin = .bincode(LENGTH, l.r)) %>% 
  select(YEAR, length_bin) %>% 
  group_by(length_bin, YEAR) %>%
  summarise(length_count = n())

length_frame_site = l %>%  
  mutate(length_bin = .bincode(LENGTH, l.r)) %>% 
  select(YEAR, length_bin, SITE) %>% 
  group_by(length_bin, YEAR, SITE) %>% 
  summarise(length_count = n()) %>%
  unite("ID", c(YEAR, SITE)) %>% 
  select(ID, length_count) %>%
  pivot_wider(names_from = ID,
              values_from = length_count) %>% 
  column_to_rownames(., var = "length_bin") %>%
  mutate_all( ~replace(., is.na(.), 0)) %>%
  as.data.frame()
  
## Size structure NMDS -------------------------------
# community is year

totals = length_frame %>% 
  group_by(YEAR) %>%
  summarise(totals = sum(length_count))

length_NMDS = left_join(length_frame, totals) %>% 
  mutate(Proportion = length_count / totals) %>% 
  select(YEAR, Proportion) %>%
  pivot_wider(names_from = YEAR,
              values_from = Proportion) %>% 
  column_to_rownames(., var = "length_bin") %>%
  mutate_all( ~replace(., is.na(.), 0)) %>%
  as.data.frame()

NMDS.L = metaMDS(length_NMDS,  k=2) 


# This was the graph Tommy wanted with the different colors as "communities" 
NMDS.L$species %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  cbind(., Year= as.numeric(rownames(.))) %>%
  as.data.frame() %>%
  arrange(Year) %>% 
  ggplot(aes(x = MDS1, y = MDS2, label = Year)) + 
  geom_text(aes(color = Year),size =4)+ 
  scale_color_gradientn(colours = rainbow(5)) +
  labs(color = "Year") + 
  ggtitle("SQRT: Size structure through time") 


NMDS.L$species %>% as.data.frame()  %>%  
  cbind(., Year= as.numeric(rownames(.))) %>%
  as.data.frame() %>%
  arrange(Year) %>%
  cbind(., year_bin[-3]) %>% 
  ggplot(aes(x = MDS1,
             y = MDS2,
             label = Year,
             color = as.character(year_bin[-3]))) + 
  geom_text(size =3) + 
  stat_ellipse() + 
  labs(color = " Year Bin") + 
  ggtitle("SQRT:Size structure - site averaged") +
  scale_color_manual(labels =  bins[2:6], values= unique(year_bin))

NMDS.L.site = metaMDS(length_frame_site, k = 2)


NMDS.L.site$species %>% as.data.frame() %>% 
  cbind(., "ID" = rownames(.)) %>%
  as.data.frame() %>%
  separate(ID, into = c("YEAR", "SITE")) %>%
  select(-SITE) %>%
  arrange(YEAR) %>%
  mutate(YEAR = .bincode(.$YEAR, bins)) %>%
  ggplot(aes(x = MDS1,
             y = MDS2,
             label = YEAR,
             color = as.character(YEAR))) + 
  #geom_point() +
  stat_ellipse(level = .9, size = 1) +
  labs(color = "Year") + 
  ggtitle("SQRT:Size structure (by site) through time") +
  scale_color_discrete(name = "Year", labels = as.character(bins[-1]))
  

## Graph length frequency across time ------------------

length_frame %>%  
  mutate(YEAR = .bincode(YEAR, c(1999, 2000, 2005, 2010, 2015, 2020))) %>% 
  group_by(YEAR, length_bin) %>%
  summarise(length_count_average = mean(length_count)) %>%
  ggplot(aes(x = length_bin, y = length_count_average, colour= as.character(YEAR))) +
  geom_point() + 
  geom_smooth(se=F,method = 'lm') + 
  ylim(0,800) + ylab("Average Frenqency of Size")+ xlab("Length Bin") +
  labs(colour = "Year") + ggtitle("Average size frequency across time") + 
  scale_color_discrete(name = "Year", labels = as.character(bins[-1])) + ylim(-1,200) + facet_wrap(~YEAR)


length_frame %>%
  mutate(YEAR_bin = .bincode(YEAR, bins)) %>%
  ggplot(aes(x = length_bin,
             y = length_count, 
             fill = as.character(YEAR_bin))) +
  geom_bar(stat = 'identity') + 
  xlim(0,18) + ylab("Length Frequency") +
  xlab("Length Bin") + 
  facet_wrap(~YEAR_bin) + 
  scale_fill_discrete(name = "Year", labels = as.character(bins[-1]))

## Species specific analyses ---------------------------------------




# Brook Trout
ST_data = tpn_data %>% filter(SPECIES == "ST") %>% 
  select(SPECIES, LENGTH, WEIGHT, SITE, YEAR) %>% 
  na.omit()

ST_data %>% group_by(YEAR) %>%
  summarise( AVG.Length = mean(LENGTH)) %>%
  ggplot(aes(x = YEAR, y = AVG.Length)) + geom_point()

ST_data %>% 
  ggplot(aes(x = YEAR, y =LENGTH)) + geom_point() + 
  geom_smooth() + 
  ggtitle("ST Length through time")


summary(lm(ST_data$LENGTH~ST_data$YEAR))



## CC 

CC_data = BEF_data %>% filter(SPECIES == "CC") %>% 
  select(SPECIES, LENGTH,  SITE, YEAR) %>% 
  na.omit()

CC_data %>% group_by(YEAR) %>%
  summarise( AVG.Length = mean(LENGTH)) %>%
  ggplot(aes(x = YEAR, y = AVG.Length)) + geom_point()

CC_data %>% 
  ggplot(aes(x = YEAR, y =LENGTH)) + geom_point() + 
  geom_smooth() + 
  ggtitle("CC Length through time")

CC_data %>% group_by(YEAR) %>%
  summarise( st.d = sd(LENGTH)) %>%
  ggplot(aes(x = YEAR, y = st.d)) + geom_point()


CC_data %>% group_by(YEAR) %>%
  summarise(AVG = mean(LENGTH)) %>%
  mutate(l = AVG - lag(AVG, default = first(AVG))) %>%
  ggplot(aes(y = l, x = YEAR)) + 
  geom_line()


## Common Shiners 

CS_data = BEF_data %>% filter(SPECIES == "CS") %>% 
  select(SPECIES, LENGTH,  SITE, YEAR) %>% 
  na.omit()

CS_data %>% group_by(YEAR) %>%
  summarise( AVG.Length = mean(LENGTH)) %>%
  ggplot(aes(x = YEAR, y = AVG.Length)) + geom_point()

CS_data %>% 
  ggplot(aes(x = YEAR, y =LENGTH)) + geom_point() + 
  geom_smooth()

CS_data %>% 
  ggplot(aes(x = as.character(YEAR), y =LENGTH)) + geom_boxplot()  
  

CS_data %>% group_by(YEAR) %>%
  summarise( st.d = sd(LENGTH)) %>%
  ggplot(aes(x = YEAR, y = st.d)) + geom_point()

summary(lm(CS_data$LENGTH~CS_data$YEAR))

CS_data %>% group_by(YEAR) %>%
  summarise(AVG = mean(LENGTH)) %>%
  mutate(l = AVG - lag(AVG, default = first(AVG))) %>%
  ggplot(aes(y = l, x = YEAR)) + 
  geom_line()


# . Lake Trout -------------------------------

## Combining BEF, GLN, and TPN data 
LT_combined = gln_data %>% filter(SPECIES == "LT") %>% select(LENGTH, YEAR) %>% mutate(gear = "GLN") %>% rbind(LT_data %>% select(LENGTH, YEAR) %>% mutate(gear = "TPN")) %>% rbind(BEF_data %>% filter(SPECIES =="LT") %>% select(LENGTH, YEAR) %>% mutate(gear = "zBEF")) ## combining all data types

years_tpn = data.frame(names = rownames(CPUE.w.sec.tpn)) %>%
  separate(names, into = c("YEAR", "SITE"))

years_gln = data.frame(names = rownames(CPUE.w.sec.gln)) %>%
  separate(names, into = c("YEAR", "SITE"))

years_bef = data.frame(names = rownames(CPUE.w.sec)) %>%
  separate(names, into = c("YEAR", "SITE"))

### hmm

CPUE.w.sec.tpn = CPUE_wide_seconds(tpn_data) %>%
  unite("Group", c(YEAR, SITE)) %>% 
  column_to_rownames(., var = "Group") %>% 
  mutate(sumrow = rowSums(.)) %>%
  filter(sumrow>0) %>%
  select(-c(sumrow, NF))

CPUE.w.sec.gln = CPUE_wide_seconds(gln_data) %>%
  unite("Group", c(YEAR, SITE)) %>% 
  column_to_rownames(., var = "Group") %>% 
  mutate(sumrow = rowSums(.)) %>%
  filter(sumrow>0) %>%
  select(-c(sumrow, NF))

# Graphing length of LT from 3 different gear types 
LT_combined %>% ggplot(aes(x = YEAR, y = LENGTH, col = gear)) +
  geom_point() + geom_smooth() + ggtitle("LT Combined Data")









#---------------------------------------
NMDS.tpn=metaMDS(CPUE.w.sec.tpn, # Our community-by-species matrix
               k=15) 

stressplot(NMDS.d)
plot(NMDS.d)

## The plot for years here is by site and it looks terrible. Not worth looking at. 


ordiplot(NMDS.d,type="n", main = 'Day Averaged - Species')

for(i in unique(year_bin)) {
  ordiellipse(NMDS.d$point[grep(i,year_bin),],draw="polygon",kind=c("sd"),
              groups=year_bin[year_bin==i],col=color[grep(i,year_bin)],label=F) } 

orditorp(NMDS.d,display="species",col="black",air=0.01)
legend("bottomright", legend = names, col = unique(color), lty= rep(1, 5), title = "Year Breaks")



ordiplot(NMDS.d,type="n")
#Plot convex hulls with colors baesd on year_binment
for(i in unique(year_bin)) {
  ordihull(example_NMDS$point[grep(i,year_bin),],draw="polygon",
           groups=year_bin[year_bin==i],col=colors[grep(i,year_bin)],label=F) } 
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",col=c(rep("green",5),
                                            rep("blue",5)),air=0.01,cex=1.25)

















## Gill Net (GLN) Data ---------------------------------------


gln_LT_length = gln_data %>% filter(SPECIES == "LT") %>% select(LENGTH, YEAR) %>% group_by(YEAR) %>% summarise(m = mean(LENGTH))







## Something else 

RS = cbind(value = as.numeric(CPUE.w.sec$RS), YEAR= as.numeric(years_bef$YEAR)) %>% as.data.frame() %>% group_by(YEAR) %>%
  summarise(RS= mean(value)) 

ggplot(aes(y = CPUE.w.sec$RS, x = years_bef$YEAR)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

CPUE.w.sec %>% select(RS, LT) %>% 
  mutate(YEAR = as.numeric(years_bef$YEAR)) %>% 
  pivot_longer(c(RS, LT), names_to = "species") %>%
  group_by(YEAR, species) %>% 
  summarise(mean_cpue = mean(value)) %>% ggplot(aes(x = YEAR, y = mean_cpue, color = species)) + geom_point()

LT_RS = left_join(as.data.frame(tpn_LT_length),
                  as.data.frame(RS)) %>%
  mutate(Year_bin = .bincode(YEAR, bins)) %>%
  filter(YEAR != 2010) 

LT_RS %>% ggplot(aes(x = m, y = RS, color = as.character(Year_bin))) + geom_point() + labs(color = "Year Bin")

summary(lm(LT_RS$m ~ LT_RS$m))
+ stat_ellipse()

%>% 
  ggplot(aes(x = RS, y = m)) + geom_point() + geom_smooth(method = "lm")






## Rate of Change -------------------------

## Taking the next year and subtracting it

## Using averaged sites
# Create data frame
roc = CPUE.w.sec.a %>% 
  mutate(across(BB:WS, ~ .x - lag(.x, default = first(.x)))) %>%
  mutate(year_bin = .bincode(rownames(.), bins)) %>%  mutate(year = row.names(.)) %>%
  pivot_longer(cols = c(-year, -year_bin), names_to = "species") 
# Plot ROC data, breaking up by species
roc %>%  
  ggplot(aes(x = as.numeric(year), y = value, color = species)) +
  geom_point() + ylab("Change in CPUE") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Year") +
  facet_wrap(~species) + 
  theme(legend.position="none")
# Running an anova across year bins for each species 
roc %>% mutate(value = abs(value), year_bin = as.character(year_bin)) %>% 
  filter(year_bin != "1") %>%
  group_by(species) %>% 
  summarise(ANOVA = summary(aov(value ~ year_bin))[[1]]$`Pr(>F)`[1]) 
  
 
## ROC for length -----------------------------------
# Create data table for length
ROC_length = l %>% group_by(SPECIES, YEAR) %>%
  summarise(avg_length = mean(LENGTH)) %>% 
  ungroup() %>% 
  group_by(SPECIES) %>%
  mutate(change = avg_length - lag(avg_length,
                                   default = first(avg_length))) %>%
  mutate(year_bin = .bincode(YEAR, bins))
# Plot results broken up by species - colored for aesthetics 
ROC_length %>% 
  ggplot(aes(x = YEAR,
             y = change,
             color = SPECIES)) +
  geom_point() + 
  facet_wrap(~SPECIES) + 
  theme(legend.position = "none") + 
  ylab("Change in Length") + 
  xlab("Year")
# Run anova across year bins for each species
ROC_length%>%
  filter(year_bin != 1) %>%
  ungroup() %>% 
  group_by(SPECIES) %>%
  mutate(year_bin = as.character(year_bin)) %>%
  summarise(ANOVA = summary(aov(avg_length ~ (year_bin)))[[1]]$`Pr(>F)`[1])


## Unfinished / Notes ------------------------------------------

## Trying to do site specific across time- I stopped doing this can pick back up if Tommy thinks is worth it 

# Shifting window bins. Bin 1 = yr 1-4, Bin 2 = yr 2-5, Bin 3 = yr 3 -6
## Note - if I can't get this to work, could maybe bin the rate of change within years and then do some anova across bins
CPUE.w.sec %>% 
  mutate(year = years_bef$YEAR, site = years_bef$SITE) %>%
  select(CC,  site) %>% 
  
  head()


## NMDS for size structure - bin by size class, across time (communities) - size class of each fish species would be the species - no actualy, do the whole community, so each species is a size bin (abundance within the bin) tells you something about ecotrophic efficiency. Do for both lake trout from the gill net and also the whole community from the BEF. 
## mess around with the CPUE through time analysis, is there some first derivative we could do? look that up 
## response time - think about it but what is the inflection point in the curve where the species start to show some response. 
## Then could we plot response timing and body size/age at maturation 
## Gillnet or trapnets 



