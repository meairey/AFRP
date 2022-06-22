### Libraries --------------

library(dplyr)
library(tidyr)
library(tidyverse)
library(vegan)
library(RColorBrewer)
library(ggridges)
library(ecp)

### Data -------------------
# This is just setup at the project working directory. Use option in upper right corner of R to get into project directory. For example, on my computer ,its stored in my family one-drive
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP")



## Functions source -----------
source("AFRP_Master_Code/AFRP_Functions.R")
sample = read.csv("Data/FISH_SAMPLE_edited.csv")


# Boat Electrofishing Data
BEF_data_unfiltered = filter_data(water = "LML", gear = "BEF",
                                  gear_code = "NAF", 
                                  species = species) %>% 
  filter(MONTH %in% c(4,5,6,7,8), YEAR >= 1998)

# Removing species ---------------
`%nin%` = Negate(`%in%`) # sets up a way to exclude if in a string
rare_threashold = 50 ## change this based on preference
rare = BEF_data_unfiltered %>% group_by(SPECIES) %>% 
  summarise(frequency = n()) %>% 
  filter(frequency < rare_threashold)
stocked = c("LLS", "RT") ## Stocked fish in little moose
remove = c(stocked, rare$SPECIES,"RWF", "SMB") ## Remove SMB + RWF (targeted 2000s)
BEF_data = BEF_data_unfiltered %>% filter(SPECIES %nin% remove)
site_hab = BEF_data %>% select(SITE, HAB_1) %>%
  rename(Site = SITE) %>%
  unique()
# Data setup ----------
CPUE.w.sec = ((CPUE_wide_seconds(BEF_data) %>%
                 unite("Group", c(YEAR, SITE)) %>% 
                 column_to_rownames(., var = "Group") %>% 
                 mutate(sumrow = rowSums(.)) %>%
                 filter(sumrow>0) %>%
                 select(-sumrow)))
years_bef = data.frame(names = rownames(CPUE.w.sec)) %>%
  separate(names, 
           into = c("YEAR", "SITE")) %>%
  mutate(YEAR = .bincode(YEAR, bins))
cpue.habs = CPUE.w.sec %>% 
  mutate(names = rownames(.)) %>% 
  separate(names, into = c("year", "Site")) %>%
  left_join(site_hab) %>%
  select(Site, HAB_1)
# Plotting CPUE --------
CPUE.w.sec %>% 
  mutate(site_year = rownames(.)) %>%
  separate(site_year, into = c("year", "Site")) %>%
  pivot_longer(BB:WS, names_to = "species", 
               values_to = "cpue") %>%
  left_join(site_hab) %>% 
  filter(species %in% c("MM","CS","CC","WS","RS", "PS")) %>%
  ggplot(aes(x = as.numeric(year), y = cpue, color = species)) + 
  geom_smooth(se=F) + 
  #scale_y_continuous(trans = "log2") + 
  facet_wrap(~HAB_1) 
## Anova test ----------
anov_dat = CPUE.w.sec %>% 
  mutate(site_year = rownames(.)) %>%
  separate(site_year, into = c("year", "Site")) %>%
  pivot_longer(BB:WS, names_to = "species", 
               values_to = "cpue") %>%
  left_join(site_hab) %>%
  mutate(year = .bincode(year, bins))

# Plotting CPUE by site 
 
CPUE.w.sec %>% 
  mutate(site_year = rownames(.)) %>%
  separate(site_year, into = c("year", "Site")) %>%
  pivot_longer(BB:WS, names_to = "species", 
               values_to = "cpue") %>%
  left_join(site_hab) %>% 
  filter(species %in% c("MM","CS","CC")) %>%
  ggplot(aes(x = as.numeric(Site), y = cpue, color = HAB_1))+
  geom_point() + 
  facet_wrap(~year)

# Running NMDS ---- by site + by year
NMDS.cpue_siteyear=metaMDS(CPUE.w.sec, k=3) 

NMDS.cpue_siteyear$species %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  as.data.frame() %>%
  ggplot(aes(x = MDS1, y = MDS2, label= rownames(.))) + 
  geom_text()+ 
  ggtitle("Site - NMDS")

## Anova test ----------

for(i in unique(cpue.habs$HAB_1)){
  
  x = CPUE.w.sec[which(cpue.habs$HAB_1 == i),]
  y = years_bef$YEAR[which(cpue.habs$HAB_1 == i)]
  print(i)
  print(summary(anosim(x,y)))
}
## Mantel ---------------
for(i in unique(cpue.habs$HAB_1)){
  x = CPUE.w.sec[which(cpue.habs$HAB_1 == i),]
  y = x %>%
    mutate(years = rownames(.)) %>%
    pivot_longer(BB:WS,names_to = "species",
                     values_to = "CPUE") %>%
    separate(years, into = c("year","site")) %>% 
    group_by(species, year) %>%
    summarise(mean = mean(CPUE)) %>%
    pivot_wider(names_from = species,
                values_from = mean) %>%
    column_to_rownames(var = "year")
    
  
  cpue.dist= vegdist(y, method = "bray")
  time.dist = dist(as.numeric(row.names(y)),
                   method = "euclidean")
  print(i)
  print(mantel(xdis = cpue.dist, 
         ydis = time.dist, 
         method = "spearman",
         permutations = 9999, na.rm = TRUE))
  
}
    

## Stability 
dlist = list()
for(i in unique(cpue.habs$HAB_1)){
  x = CPUE.w.sec[which(cpue.habs$HAB_1 == i),]
  y = x %>%
    mutate(years = rownames(.)) %>%
    pivot_longer(BB:WS,names_to = "species",
                 values_to = "CPUE") %>%
    separate(years, into = c("year","site")) %>% 
    group_by(species, year) %>%
    summarise(mean = median(CPUE)) %>%
    pivot_wider(names_from = species,
                values_from = mean) %>%
    column_to_rownames(var = "year")
  
  cpue_dist <- vegdist(y)
  
  d = cpue_dist %>%
    as.matrix() %>% 
    as_tibble(rownames= "sample") %>%
    pivot_longer(-sample) %>%
    filter(sample < name) %>%
    mutate(diff = abs(as.numeric(name) - 
                        as.numeric(sample)),
           HAB_1 = i) 
  c = d %>%
    ggplot(aes(x = diff,y = value)) + 
    geom_point() + 
    ylab("Dissimilarity") + 
    xlab("Years between time points") + 
    geom_smooth() + 
    ggtitle(paste(i))
  
  print(c)
  
  dlist[[i]] = d
  
}

rbind(dlist[[1]],
      dlist[[2]], 
      dlist[[3]], 
      dlist[[4]]) %>% 
  as.data.frame() %>%
  ggplot(aes(x = diff,y = value)) + 
  geom_point() + 
  ylab("Dissimilarity") + 
  xlab("Years between time points") + 
  geom_smooth() + 
  ggtitle(paste(i)) + 
  facet_wrap(~HAB_1) + 
  ggtitle("")
  



mice_dist %>%
  as.matrix() %>%
  as_tibble(rownames= "sample") %>%
  pivot_longer(-sample) %>%
  filter(sample < name) %>%
  



##
cat = NMDS.cpue_siteyear$points %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  cbind(., Year= rownames(.)) %>%
  separate(Year, into = c("Year","Site"), remove = F) %>%
  as.data.frame() %>%
  left_join(site_hab) %>%
  mutate(Year = .bincode(Year, bins)) %>%
  arrange(Year)

cat %>% 
  ggplot(aes(x = MDS1, 
             y = MDS2, 
             color = as.character(Year))) + 
  #geom_point(size =.2) + 
  stat_ellipse() +
  labs(color = "Year") + 
  ggtitle("Years: by site") + 
  facet_wrap(~HAB_1)


cat %>% 
  ggplot(aes(x = MDS1, 
             y = MDS2, 
             color = as.character(HAB_1))) + 
  #geom_point(size =.2) + 
  stat_ellipse() +
  labs(color = "Year") + 
  ggtitle("By Habitat") 

# Anosim ------------------
cpue.dist = vegdist(CPUE.w.sec)
cpue.ano <- anosim(cpue.dist, cpue.habs$HAB_1)
summary(cpue.ano)
plot(cpue.ano)



## Habitat changepoints ------------------
vec = vector()
p.val = vector()
species = colnames(CPUE.w.sec)
change_points_list = list()
v = CPUE.w.sec %>% 
  mutate(y_s = rownames(CPUE.w.sec)) %>%
  mutate(HAB_1 = cpue.habs$HAB_1) %>%
  pivot_longer(1:length(species),
               names_to = "Species") %>%
  separate(y_s, 
           into = c("Year", "site")) %>%
  unite("ID", 
        c(site:Species), 
        sep = "_", 
        remove = F) %>%
  select(-site)

## Fixed change point using multiple sites a year as multivariate --------- 
for(i in species){
  listy = list()
  
  for(h in c("R","RW","S","SW")){
  
  # Set up data frame
  x = v %>% filter(Species == i, HAB_1 == h) %>%
    mutate(value = as.numeric(value)) %>% 
    select(-Species, -HAB_1) %>%
    mutate(value = log10(value+1)) %>%
    pivot_wider(values_from = value,
                names_from = ID) %>%
    replace(is.na(.), 0) %>%
    select(-Year) %>%
    as.matrix()
  rownames(x) = rownames(CPUE.w.sec.a)
  
  # Run changepoint analysis 
  output = e.divisive(x, 
                      R = 499, 
                      alpha = 1, 
                      min.size = 2,
                      sig.lvl = .05)
  
  # Format data
  dat = data.frame(Year = rownames(CPUE.w.sec.a), 
                   color = output$cluster)
  v_mod = left_join(v,dat)
  
  # Linear model to overlap on plots
  lin.reg = summary(lm(v_mod$value~as.numeric(v_mod$Year)))
  p_value = round(lin.reg$coefficients[2,4],4)
  r_2 = round(lin.reg$adj.r.squared,4)
  y_point = max(x) 
  # CB pallete
  cbbPalette <- c("#000000",  "#56B4E9", "#D55E00","#009E73", "#F0E442", "#0072B2",  "#CC79A7","#E69F00")
  # Plots
  
  f = v_mod %>%
    filter(Species == i, HAB_1 == h)
  
  listy[[h]] = f
  
  pl = v_mod %>%
    filter(Species == i, HAB_1 == h)%>% 
    ggplot(aes(x = as.numeric(Year), 
               y = value)) +
    geom_point(aes(col = as.character(color)), alpha = .2, size = 2) + 
    #geom_text(x= 2010, y= y_point, label="Scatter plot") + 
    geom_smooth(method = "lm") + 
    #xlab("Year") + 
    ylab(paste(i,h,"CPUE")) +
    xlim(1998, 2021) + 
    theme(text = element_text(size = 16)) + 
    theme(legend.position="none") + 
    theme(axis.title.x = element_blank()) + 
    theme(axis.text.x = element_text(angle = 90)) + 
    scale_colour_manual(values=cbbPalette)
  
  

  }
  
  dog = rbind(listy[[1]],listy[[2]], listy[[3]],listy[[4]])
  
  thug = dog %>% ggplot(aes(x = as.numeric(Year), 
                            y = value)) + 
    geom_point(aes(color = as.character(color))) +
    facet_wrap(~HAB_1) + 
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "none") + 
    geom_smooth(method = "lm") +
    xlim(1998, 2021) +
    ylab(paste(i,"CPUE")) +
    xlab("Year")
  print(thug)
}

