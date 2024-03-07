## This is the script for the 2nd LML paper on slope/intercept and food web structure shift through time

## !! Remove the pre-data? 
# Data
k = BEF_data %>%
  filter(YEAR > 1999) %>%
  select(YEAR, SPECIES, LENGTH, YEAR) %>%
  filter(LENGTH > 90) %>%
  mutate(LENGTH = log10(LENGTH)) %>%
  na.omit()

# Creating bins - based on logged total length

bin_size = .1
l.r = seq(from =range(k$LENGTH)[1]-.1, to =  range(k$LENGTH)[2], by = bin_size)
l.r[length(l.r)+1] = range(k$LENGTH)[2] + .1



# Data ------
## Set up the data by filtering BEF_data and selecting a set of columns to use
# Does not consider species identity

k = BEF_data %>% 
  filter(SPECIES %nin% c("SMB", "RS")) %>%
  select(YEAR, SPECIES, LENGTH, YEAR) %>%
  filter(LENGTH > 90) %>%
  mutate(LENGTH = log10(LENGTH)) %>%
  na.omit() %>% 
  mutate(LENGTH.bin = .bincode(LENGTH, l.r))  %>%
  group_by(LENGTH.bin, SPECIES,  YEAR) %>%
  summarise(abundance = n()) %>%
  mutate(abundance = log2(abundance)) %>%
  ungroup() %>% 
  filter(YEAR > 1999, YEAR != 2002) %>% #  Filter what years to remove
  filter(SPECIES %nin% remove, #filter out any species 
         #SPECIES != "RS") 
         )%>%
 filter(abundance> 0 )
species = data.frame()
for(h in 1:length(unique(k$YEAR))){
    
    j = k %>% 
      filter(YEAR == unique(k$YEAR)[h])
    
    f = (lm(j$abundance~j$LENGTH.bin))$coefficients
    species[h,1] = f[1]
    species[h,2] = f[2]
    
    
}    
## !! Go through and check to see if V1 - should be intercept and V2 - should be slope should be swapped!!
species = species %>% rename("slope" = V2, "intercept" = V1)
species %>% as.data.frame() %>% ggplot(aes(x = unique(k$YEAR), y = slope)) + 
  geom_point() + 
  geom_smooth(method ="lm", se = F, col = 1) +
  ylab("Slope") + xlab("Year") + theme_classic()


species %>% as.data.frame() %>% ggplot(aes(x = unique(k$YEAR), y = intercept)) + 
  geom_point() + 
  geom_smooth(method ="lm", se = F, col = 1) +
  ylab("intercept") + xlab("Year") + theme_classic()

species %>% as.data.frame() %>% mutate(year = unique(k$YEAR)) -> species

lm(species$slope ~ species$year) %>% summary()





# Site averaged NMDS across sites -----------------
## This averages CPUE across sites and days within a given year
### Notes
#### NMDS$species = species
#### NMDS$points = years
CPUE.w.sec.a = ((CPUE_wide_seconds_avg(BEF_data) %>% 
                   column_to_rownames(., var = "YEAR")))
CPUE.w.sec.a = CPUE_wide_seconds_avg(BEF_data) %>% 
  column_to_rownames(., var = "YEAR") # Data setup
#CPUE.w.sec.a.smb =   ((CPUE_wide_seconds_avg(SMB_data) %>% 
#                        column_to_rownames(., var = "YEAR"))) # With SMB

NMDS.cpue_year=metaMDS(CPUE.w.sec.a, # Our community-by-species matrix
                       k=2, try = 100) 

# Visualization of species 
NMDS.cpue_year$species %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  as.data.frame() %>%
  ggplot(aes(x = MDS1, 
             y = MDS2,
             label = rownames(NMDS.cpue_year$species))) + 
  geom_text(size =4) + 
  ggtitle("Species - averaged across sites")

NMDS.cpue_year$points %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  cbind(., Year= as.numeric(rownames(.))) %>%
  as.data.frame() %>%
  arrange(Year) %>% 
  ggplot(aes(x = MDS1,
             y = MDS2,
             label = Year)) + 
  theme_minimal() +
  geom_text(aes(color = as.numeric(Year)),
            size =4) + 
  scale_color_gradientn(colours = rainbow(5)) +
  labs(color = "Year") + 
  ggtitle("CPUE") + 
  theme(legend.position = "none")

# Day averaged NMDS across sites --------------

# Data setup
CPUE.w.sec = ((CPUE_wide_seconds(BEF_data) %>%
                 unite("Group", c(YEAR, SITE)) %>% 
                 column_to_rownames(., var = "Group") %>% 
                 mutate(sumrow = rowSums(.)) %>%
                 filter(sumrow>0) %>%
                 select(-sumrow)))
# Running NMDS ---- by site + by year
NMDS.cpue_siteyear=metaMDS(CPUE.w.sec, k=3) 
# Diagnostic plots 
stressplot(NMDS.cpue_siteyear)
plot(NMDS.cpue_siteyear)




NMDS.cpue_siteyear$species %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  as.data.frame() %>%
  ggplot(aes(x = MDS1, y = MDS2, label = rownames(NMDS.cpue_year$species))) + 
  geom_text(size =4)+ 
  ggtitle("Community = Site/Year")


CPUE.w.sec



## Creating row names 
years_bef = data.frame(names = rownames(CPUE.w.sec)) %>%
  separate(names, 
           into = c("YEAR", "SITE"))

## Points through time dist matrix ------------
nmds.dist = dist(NMDS.cpue_year$points, upper = T) %>% 
  as.matrix(.) %>% as.data.frame()

before_after = c(1990, 2000, 2023)

data.frame(dist = nmds.dist$`2000`,
           year = unique(years_bef$YEAR)) %>%
  ggplot(aes(x = as.numeric(year), y = dist)) + 
  geom_point() +
  ylab("Distance from 2000") + 
  xlab("Year")


datty = data.frame(dist = nmds.dist$`2000`, 
                   year = unique(years_bef$YEAR)) %>%
  filter(year > 2000)
summary(lm(datty$dist ~ as.numeric(datty$year)))

cpue.dist.frame = data.frame(dist = nmds.dist$`2000`, 
           year = unique(years_bef$YEAR)) 
cpue.dist.frame %>% 
  mutate(bin = .bincode(year, before_after)) %>%
  ggplot(aes(x = as.numeric(year), y = dist, color = as.character(bin))) + 
  geom_point() + 
  geom_smooth(method = lm, se=F) + 
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Euclidean distance from 2000")


## Size structure NMDS -------------------

l = BEF_data %>% select(SPECIES, LENGTH, SITE, YEAR) %>% 
  na.omit() %>% mutate(LENGTH = (LENGTH)) %>% filter(YEAR != 2002) ## Modified to remove 2002 and transform length by the sqrt 


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
  mutate(Percent = length_count / totals * 100) %>% 
  ungroup() %>%
  select(YEAR, length_bin, Percent) %>%
  pivot_wider(names_from = length_bin,
              values_from = Percent) %>% 
  column_to_rownames(., var = "YEAR") %>%
  mutate_all( ~replace(., is.na(.), 0)) %>%
  as.data.frame()

NMDS.L = metaMDS(length_NMDS,  k=2) 


# This was the graph Tommy wanted with the different colors as "communities" 
NMDS.L$points %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  cbind(., Year= as.numeric(rownames(.))) %>%
  as.data.frame() %>%
  arrange(Year) %>% 
  ggplot(aes(x = MDS1, y = MDS2, label = Year)) + 
  geom_text(aes(color = Year),size =4)+ 
  scale_color_gradientn(colours = rainbow(5)) +
  labs(color = "Year") + 
  ggtitle("SQRT: Size structure through time") 



nmds.dist_L = dist(NMDS.L$points, upper = T) %>% 
  as.matrix(.) %>% as.data.frame() %>% 
  rownames_to_column(var = "YEAR") %>%
  mutate(YEAR = as.numeric(YEAR)) %>% 
  arrange(YEAR) %>%
  mutate(YEAR = as.character(YEAR)) %>%
  column_to_rownames(var = "YEAR")

nmds.dist_frame = data.frame(dist = nmds.dist_L$`2000`, 
           year = unique(years_bef$YEAR)[-5]) 

nmds.dist_frame %>% 
  mutate(bin = .bincode(year, before_after)) %>%
  ggplot(aes(x = as.numeric(year), y = dist, color = as.character(bin))) + 
  geom_point() + 
  geom_smooth(method = lm, se=F) + 
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Euclidean distance from 2000")

## z transform the nmds dist points 
## !! You can also try doing euclid.dist between distance of years apart
## Are the two distances unrelated or related - what is driving the changes in the community, think maybe z transform, autoscaling
library(scales)
nmds.dist_frame %>% 
  rename(length.dist = dist) %>% 
  left_join(cpue.dist.frame) %>%
  mutate(length.dist = rescale(length.dist,c(0,1))) %>%
  mutate(dist = rescale(dist,c(0,1))) %>% 
  mutate(ratio = length.dist / dist) %>% 
  ggplot(aes(x = year, y = ratio)) +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = '2000' )+
  geom_point(aes(col = year)) +
  xlab("size") + ylab("cpue") + 
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90)) 

