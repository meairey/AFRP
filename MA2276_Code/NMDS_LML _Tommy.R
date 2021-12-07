### Libraries --------------

library(dplyr)
library(tidyr)
library(tidyverse)
library(vegan)
library(RColorBrewer)

### Data -------------------
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
names = bins[-1] ## This goes into the legend 
year_bin =  (data.frame(treat = c(2000:2019)) %>%
               separate(treat, into = c("Year", "Site")) %>% 
               mutate(Year = .bincode(as.numeric(.$Year),
                                      ## Define desired breaks below
                                      breaks = bins)) %>% 
               select(Year))$Year
#c(1999,2000, 2005, 2012, 2019)))
#breaks = c(1999,2000, 2009, 2011, 2019)))

## Filter data -----------

## Filter data for Little Moose, Boat electrofishing, in the spring. I've also filtered it to be yr 2000+ because earlier years had strange site labels. 

TPN_gearcodes = unique((sample %>% filter(GEAR == "TPN", WATER == "LML"))$GEAR_CODE)

all_data = filter_data(water = "LML", gear = "BEF",
                       gear_code = "NAF", 
                       species = species) %>% 
  filter(MONTH %in% c(4,5,6,7,8), YEAR >= 2000)


#tpn_data = filter_data(water = "LML", gear = "TPN" ,
                       
                       #species = species) ### You need to figure out why this isnt workin in the filter data function!! 

tpn_data = left_join(fish, sample, by = "YSAMP_N") %>% 
  left_join(sites, by = "SITE_N") %>% 
  left_join(shoreline_length, by = "SITE_N") %>%
  separate(SITE_N,  into = c("GEAR", "WATER","SITE")) %>% filter(WATER == "LML", 
                                                                 GEAR == "TPN", 
                                                                 YEAR >= 2000)
gln_data = left_join(fish, sample, by = "YSAMP_N") %>% 
  left_join(sites, by = "SITE_N") %>% 
  left_join(shoreline_length, by = "SITE_N") %>%
  separate(SITE_N,  into = c("GEAR", "WATER","SITE")) %>% filter(WATER == "LML", 
                                                                 GEAR == "GLN", 
                                                                 YEAR >= 2000)

## Removing rare + stocked taxa ------------ 
## Taxa get removed if they are rare or if they are stocked given that stocking would be an artificial manipulation of populations not relating to bass 

`%nin%` = Negate(`%in%`)
rare_threashold = 50 ## change this based on preference
rare = all_data %>% group_by(SPECIES) %>% 
  summarise(frequency = n()) %>% 
  filter(frequency < rare_threashold)
stocked = c("LLS", "RT") ## Stocked fish in little moose
remove = c(stocked, rare$SPECIES) ## Remove smallmouth bass
all_data = all_data %>% filter(SPECIES %nin% remove)

# Site averaged NMDS across sites -----------------
## This averages CPUE across sites and days within a given year

CPUE.w.sec.a = CPUE_wide_seconds_avg(all_data) %>% 
  column_to_rownames(., var = "YEAR") # Data setup

NMDS.cpue_year=metaMDS(CPUE.w.sec.a, # Our community-by-species matrix
                     k=2, try = 100) 

stressplot(NMDS) 
plot(NMDS)

#NMDS$species = species
#NMDS$points = years

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
  ggtitle("Years - averaged across sites")  + 
  scale_color_manual(labels =  bins[2:6], values= unique(year_bin))

 
# Day averaged NMDS across sites --------------
CPUE.w.sec = CPUE_wide_seconds(all_data) %>%
  unite("Group", c(YEAR, SITE)) %>% 
  column_to_rownames(., var = "Group") %>% 
  mutate(sumrow = rowSums(.)) %>%
  filter(sumrow>0) %>%
  select(-sumrow)

years_bef = data.frame(names = rownames(CPUE.w.sec)) %>%
  separate(names, into = c("YEAR", "SITE"))

color = as.character(year_bin) %>% as.factor() %>% as.numeric()

NMDS.cpue_siteyear=metaMDS(CPUE.w.sec, # Our community-by-species matrix
             k=3) 

stressplot(NMDS.cpue_siteyear)
plot(NMDS.cpue_siteyear)


NMDS.cpue_siteyear$species %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  as.data.frame() %>%
  ggplot(aes(x = MDS1, y = MDS2, label = rownames(NMDS.cpue_year$species))) + 
  geom_text(size =4)+ 
  ggtitle("Species: by site + year")

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

l = all_data %>% select(SPECIES, LENGTH, SITE, YEAR) %>% 
  na.omit()

# Define weight/length matrix, remove NAs

# .  Binning data -----------

## Create bins
length_range <- range(l$LENGTH)[1]:range(l$LENGTH)[2]
i <- length_range[1] # starting interval for l.r
bin_size = 20 ## Define this based on desired bin size
l.r = na.omit(length_range[1:(i+bin_size)==(i+bin_size)]) # vector of weights to be binned
l.r[1] = length_range[1] - 1
l.r[length(l.r)+1] = length_range[length(length_range)]

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
  ggplot(aes(x = MDS1, y = MDS2, label = rownames(NMDS.L$species))) + 
  geom_text(aes(color = as.numeric(rownames(NMDS.L$species))),size =4)+ 
  scale_color_gradientn(colours = rainbow(5)) +
  labs(color = "Year") + 
  ggtitle("Size structure through time")



NMDS.L$species %>% as.data.frame()  %>%  
  cbind(., Year= as.numeric(rownames(.))) %>%
  as.data.frame() %>%
  arrange(Year) %>%
  cbind(., year_bin) %>% 
  ggplot(aes(x = MDS1,
             y = MDS2,
             label = Year,
             color = as.character(year_bin))) + 
  geom_text(size =3) + 
  stat_ellipse() + 
  labs(color = " Year Bin") + 
  ggtitle("Size structure - site averaged") +
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
 # geom_point() +
  stat_ellipse(level = .9, size = 1) +
  labs(color = "Year") + 
  ggtitle("Size structure (by site) through time") +
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
  scale_color_discrete(name = "Year", labels = as.character(bins[-1]))

l %>%  
  mutate(length_bin = .bincode(LENGTH, l.r)) %>%
  mutate(YEAR = .bincode(YEAR, c(1999, 2000, 2005, 2010, 2015, 2020))) %>% 
  ggplot(aes(x = length_bin )) + 
  geom_bar(aes(fill = as.character(YEAR))) +   
  ggtitle("Size structure through time") +
  labs(fill = "Year")  + 
  scale_fill_discrete(name = "Year", labels = as.character(bins[-1]))
  #+ 
  #scale_y_continuous(trans='log10')




## Species specific analyses ---------------------------------------

# Lake Trout 

LT_data = tpn_data %>% filter(SPECIES == "LT") %>% 
  select(YSAMP_N,DAY_N,SEASON,SPECIES, WATER, FISH_N,LENGTH, WEIGHT, SITE, YEAR,  EFFORT) %>% 
  na.omit() ## make data frame

# Average length across time
LT_data %>% group_by(YEAR) %>%
  summarise( AVG.Length = mean(LENGTH)) %>%
  ggplot(aes(x = YEAR, y = AVG.Length)) + geom_point()
# Length across time with trendline
LT_data %>% 
  ggplot(aes(x = YEAR, y =LENGTH)) + geom_point() + 
  geom_smooth() + 
  ggtitle("LT Length through time")
# Standard deviation of length across time
LT_data %>% group_by(YEAR) %>%
  summarise( st.d = sd(LENGTH)) %>%
  ggplot(aes(x = YEAR, y = st.d)) + geom_point()
# lm of data 
summary(lm(LT_data$LENGTH~LT_data$YEAR))
# Setting up lake trout cpue 

years_tpn = data.frame(names = rownames(CPUE.w.sec.tpn)) %>%
  separate(names, into = c("YEAR", "SITE"))

years_gln = data.frame(names = rownames(CPUE.w.sec.gln)) %>%
  separate(names, into = c("YEAR", "SITE"))

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


## I am working through getting the different nmds analyses for the TPN of GLN data and working towards length distributions of the LT, RWF, ST, and CC
ggplot(,aes(y = CPUE.w.sec$LT, x = years_bef$YEAR)) + geom_point() + ggtitle("BEF LT CPUE")
ggplot(,aes(y = CPUE.w.sec.tpn$LT, x = years_tpn$YEAR)) + geom_point() + ggtitle("TPN LT CPUE")
ggplot(,aes(y = CPUE.w.sec.gln$LT, x = years_gln$YEAR)) + geom_point() + ggtitle("GLN LT CPUE")


#---------------------------------------
NMDS.d=metaMDS(CPUE.w.sec.tpn, # Our community-by-species matrix
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






%>% 
  column_to_rownames(., var = "YEAR") # Data setup
NMDS=metaMDS(CPUE.w.sec.a, # Our community-by-species matrix
             k=2, try = 100) 











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

ST_data %>% group_by(YEAR) %>%
  summarise( st.d = sd(LENGTH)) %>%
  ggplot(aes(x = YEAR, y = st.d)) + geom_point()

summary(lm(ST_data$LENGTH~ST_data$YEAR))

# Round White Fish 

RWF_data = tpn_data %>% filter(SPECIES == "RWF") %>% 
  select(SPECIES, LENGTH, WEIGHT, SITE, YEAR) %>% 
  na.omit()

RWF_data %>% group_by(YEAR) %>%
  summarise( AVG.Length = mean(LENGTH)) %>%
  ggplot(aes(x = YEAR, y = AVG.Length)) + geom_point()

RWF_data %>% 
  ggplot(aes(x = YEAR, y =LENGTH)) + geom_point() + 
  geom_smooth() + 
  ggtitle("RWF Length through time")

RWF_data %>% group_by(YEAR) %>%
  summarise( st.d = sd(LENGTH)) %>%
  ggplot(aes(x = YEAR, y = st.d)) + geom_point()

summary(lm(RWF_data$LENGTH~RWF_data$YEAR))

## CC 

CC_data = all_data %>% filter(SPECIES == "CC") %>% 
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

ggplot(,aes(y = CPUE.w.sec$CC, x = (years$YEAR))) + geom_point()

## Common Shiners 

CS_data = all_data %>% filter(SPECIES == "CS") %>% 
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



## IF we havent measured the size of a fish - they measure every year, but they don't measure every fish - look at a way to resample that size distribution for the year. Use distribution and randomly sample it. 

## NMDS for size structure - bin by size class, across time (communities) - size class of each fish species would be the species - no actualy, do the whole community, so each species is a size bin (abundance within the bin) tells you something about ecotrophic efficiency. Do for both lake trout from the gill net and also the whole community from the BEF. 
## mess around with the CPUE through time analysis, is there some first derivative we could do? look that up 
## response time - think about it but what is the inflection point in the curve where the species start to show some response. 
## Then could we plot response timing and body size/age at maturation 
## Gillnet or trapnets 



