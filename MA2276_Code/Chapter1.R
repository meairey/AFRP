##-------------------------Data Loading ----------------------------------------
load("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/isotopedata.RData")
##------------------------ Library loading -------------------------------------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP")
library(ggplot2)
library(dplyr)
library(tidyr)
library(SIBER)
library(readr)
library(tidyverse)
library(tidyselect)
library(stringr)
library(vegan)
library(ggpattern)
library(gridExtra)
`%nin%` = Negate(`%in%`)

##  ------------------------- Source ----------------------------------------
source("MA2276_Code/Isotopes/isotope_functions.R")

## ----------------------------- Data setup ---------------------------------

level_order = c("BND", "BNM","CC","CLM","CS","FHM","FSD","GS","NPD","NRD","WS","ST","LLS","MM","RBS","SMB","PS","BB","CR","FI","FO","RS", "BK")

family = c("L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L","C","S","S","E","C","C","C","I","C","C","C","C", "F")

native_invasive = c("N","I","N","N","N","I","N","N","I","N","N","N","N","N","I","N","I","N","N","N","N","N","N","N")


##---------------------------- Data prep for SIBER---------------------------------------
d = read.csv("MA2276_Code/Data/Dissertation_Isotopes/SIA_updated_FINAL.csv")  # Individual information
s = read.csv("MA2276_Code/Data/Dissertation_Isotopes/SIA_UPDATED_SAMPLE_zoop.csv") # Sample information
i = read.csv("MA2276_Code/Data/Dissertation_Isotopes/SIA_RAW_2023.csv") # Isotope data

df_iso= left_join(d, s, by ="YSAMP_N" ) %>% left_join(i) # Joining together all the sample data
df_iso = df_iso %>% filter(WATER != "LSP", WATER != "USP", WATER != "ETL") %>% ## Remove any lakes without many samples. I think I accidentally ran a couple of crayfish from each of these lakes 
  filter(TAXA != "INVERT") %>%
  mutate(WATER = str_replace(WATER, "COM ", "COM"))



combo_2020 = read.csv("Data/SIA_Data.csv", header=T) %>%
  mutate(Sample.ID = parse_number(as.character(Sample.ID))) %>% 
  left_join(read.csv("MA2276_Code/Data/ADKwebs_Data.csv")) %>%
  rename(WATER = Water) %>% select(d13C, d15N, Species, WATER) %>% 
  mutate(YEAR = 2020) %>% 
  mutate(MONTH = 8) %>%
  rename(CODE = Species) %>% 
  droplevels() %>%
  mutate(WATER = str_replace(WATER, "COM", "COM "))


df_iso %>% select(WATER, CODE) %>% unique() %>% group_by(CODE) %>% 
  summarize(sampled_lakes = n()) %>% 
  filter(sampled_lakes > 1) %>% arrange(sampled_lakes)

#combo_2020 = combo_2020 %>% select(d13C, d15N, Species, WATER) %>% 
#  mutate(YEAR = 2020) %>% 
# mutate(MONTH = 8) %>%
# rename(CODE = Species) %>% 
# droplevels()

lw_data = read.csv("Data/SIA_Data.csv", header=T) %>%
  mutate(Sample.ID = as.numeric(Sample.ID)) %>% 
  full_join(read.csv("MA2276_Code/Data/ADKwebs_Data.csv"))

combo_2023 = df_iso %>% select(WATER, MONTH, YEAR, CODE, d13C, d15N) %>% droplevels()


## Join 2020 and 2023 data runs together


combo = full_join(combo_2020, combo_2023) %>% na.omit() # Combining 2020/2021 data

community = combo %>% select(WATER, MONTH, YEAR) %>% 
  unique() %>% na.omit() %>%
  arrange(YEAR, WATER,MONTH) %>%
  mutate(community = c(1:length(MONTH))) %>% 
  mutate(community = replace(community, 21, 20)) # This fixes the issue with PRL being sampled for spring in June/July

data_codes = left_join(combo, community) %>% ## Changing codes that weren't the same through time
  mutate(CODE = str_replace(CODE, "PD", "NPD")) %>%
  mutate(CODE = str_replace(CODE, "NNPD", "NPD")) %>%
  mutate(CODE = str_replace(CODE, "BM", "BNM")) %>%
  mutate(CODE = str_replace(CODE, "HYB", "FSD-NRD")) %>%
  mutate(group = as.numeric(as.factor(CODE))) 


## Filter out any species across communities with too few observations for SIBER
samplesize_filter = data_codes %>%
  group_by(community, group) %>% 
  summarize(count = n()) %>%
  ungroup()


data = data_codes %>% 
  rename(iso1 = d13C,
         iso2 = d15N) %>%
  select(iso1, iso2, group, community) %>% full_join(samplesize_filter) %>%
  filter(count > 3) %>% 
  select(-count)

## Filtering out non-complete webs 

dd_communities = c(6,18,21)

#write.csv(data, file = "combined_iso_data.csv")
#dev.off()
## Baselines
baselines = left_join(d, s, by ="YSAMP_N" ) %>% left_join(i) %>% filter(WATER != "LSP", WATER != "USP", WATER != "ETL") %>% filter(TAXA %in% c("INVERT","ZOOP")) 
baselines %>% 
  separate(YSAMP_N, c("ID", "WATER", "YEAR","DSAMP")) %>%
  ggplot(aes(x = FAMILY, y = d15N, col = YEAR)) + facet_wrap(~WATER) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

#write.csv(baselines,file = "baselines_isodata.csv")
#dev.off() 
## -------------------------------- Overlap -----------------------------                

data = data %>% filter(community %nin% dd_communities )
### Overlap function -------------
dumb_fix = c(1:26)[-dd_communities]
overlap_list = list() 

for(h in 1:length(dumb_fix)){
  
  overlap_list[[h]] = overlap(data,dumb_fix[h],20) %>% as.data.frame() 
  
  
}


# Code to reduce list to matrix 
overlap_data = Reduce(full_join, overlap_list) %>%
  select('Spp Pair', everything())




## ------------------------------- Relative Area -------------------------------
relative_list = list()
median_areas = rep(0, 8)
vec_list = list() 
c_list = list()
n_list = list()
area_list = list() 
dumb_fix_relative = dumb_fix[c(-4, -9,-11,-12,-18, -20)]
dumb_fix_relative[8:19]
for(h in dumb_fix_relative){
  
  dat = data_setup(data,h)
  
  ellipse.area = siberEllipses(dat[[2]])
  TA = rowSums(ellipse.area)
  ellipse.area = ellipse.area/TA
  
  names = unique(legend$CODE[dat[[3]]$group])
  name_common = unique(legend$common[dat[[3]]$group])
  
  colnames(ellipse.area) = names
  vec = vector()
  for(i in 1:length(ellipse.area[1,])){
    cat= ellipse.area[ellipse.area[,i] >  quantile(x = ellipse.area[,i], 
                                                   probs = .05) &
                        ellipse.area[,i] < quantile(x = ellipse.area[,i], 
                                                    probs = .95),i]
    vec = cbind(vec, cat)
  }
  colnames(vec) = names
  
  median_area = vec %>% as.data.frame() %>%
    summarise_all(list(median))
  
  p = vec %>%
    as.data.frame() %>%
    pivot_longer(1:length(names(dat[[2]])),
                 names_to = "SP",
                 values_to = "Dat") 
  g = ggplot() + 
    geom_point(p, mapping = aes(y = SP , x = Dat, col = SP)) + 
    xlim(0,1) + 
    ggtitle(lake_2023$Water[h]) +
    ylab("Species") + 
    xlab("Relative Niche Area") + 
    theme(text = element_text(size = 18)) +
    geom_point(mapping =aes(x = as.numeric(median_area[1,]), 
                            y = unique(p$SP)),
               col = unique(legend$color[sort(unique(dat[[3]]$group))]), pch = "|", size = 5)+ 
    scale_color_manual(values = legend$color[sort(unique(dat[[3]]$group))], 
                       
                       name = "Species")
  colnames(median_area) = paste(colnames(median_area),lake_2023$Water[h], sep=" ")
  relative_list[[h]] = (median_area[1,])
  median_areas[h] = mean(as.numeric(median_area[1,]))
  vec_list[[h]] = as.data.frame(vec) %>% mutate(Water = lake_2023$Name[h]) %>%
    select(Water, everything()) %>%
    pivot_longer(1:length(vec[1,])+1, names_to = "Species", values_to = "Overlap")
  
  print(g)
  
  
  area_list[[h]] = siberEllipses(dat[[2]]) %>%
    as.data.frame() %>% 
    mutate(Water = lake_2023$Name[h]) %>% 
    select(Water, everything()) %>% 
    as.data.frame() %>%  
    pivot_longer(1:length(vec[1,])+1,
                 names_to = "Species", 
                 values_to = "Overlap") %>%
    mutate(posterior_draw = rep(c(1:4000), each = length(unique(p$SP)))) %>%
    sample_n(300)
  
  area_list[[h]]  = p %>% 
    mutate(post = rep(c(1:3600),  each = length(unique(p$SP)))) %>%
    mutate(WATER = lake_2023[h,3] )
}


###  Data setup for relative area comparisons -----


names(vec_list) = lake_2023$Name
names(area_list) = lake_2023$Name

relative = unlist(relative_list)


med_relative.data = data.frame(med_area = relative, ID = names(relative)) %>%
  separate(ID, into = c("Species", "Water"),sep = " ")

full_relative = Reduce(full_join, area_list[c(1,2,3,5,7,8,9,11,14,15,16,17,19,22, 24,25,26)])


## -----------------------------  ALSC data   ----------------------------------

setwd("C:/Users/monta/OneDrive - Airey Family/Cornell/ALSC Analysis")

dat_spec = read.csv("species_alsc.csv")
### Datasetup -----------
min_bin = c(0,200000, 150000000000000000)
## This is presence absence 
#### Pres/abs -------
alsc_p.a = read.csv("ALSCDATA_fish_altered.csv", header=T)[,1:54] %>% 
  replace(!is.na(.),1)  %>%
  replace(is.na(.),0) %>%
  mutate(Richness = rowSums(.)) %>%
  cbind(read.csv("ALSCDATA_fish_altered.csv", header=T)[,56:60]) %>%
  cbind(read.csv("ALSCDATA_chem_altered.csv",header=T)) %>%
  #rename(Long = LONG..NAD83.DD.)%>%
  mutate(LONG = LONG*-1) %>%
  filter(Richness > 0)


alsc_p.a %>% filter(WS != 0) %>% dim()
alsc_p.a %>% filter(CS != 0) %>% dim()
alsc_p.a %>% filter(WS != 0) %>% filter(CS != 0) %>% dim()

WS_lakes = alsc_p.a %>% filter(WS != 0)
write_csv(WS_lakes, file = "WS_lakes.csv")
dev.off()


end_spp = which(colnames(alsc_p.a)=="Volume.m3.")-1 
sp_waters = alsc_p.a[,1:end_spp] %>% colSums(.)
common_spp =which(sp_waters > 50) 

### This has abundance data
#### Abundance -----------
alsc_abundance = read.csv("ALSCDATA_fish_altered.csv", header=T)%>%
  replace(is.na(.),0 ) %>%
  mutate(Richness = rowSums(.[,1:54])) %>%
  cbind(.,read.csv("ALSCDATA_chem_altered.csv",header=T)) %>%
  select(-NF) %>%
  #rename(Long = LONG..NAD83.DD.)%>%
  mutate(LONG = LONG*-1)%>%
  filter(Richness != 0 )

rare = alsc_abundance[,common_spp] %>%
  mutate(Richness = rowSums(.)) %>%
  filter(Richness != 0 ) %>%
  select(-Richness) %>% 
  #select(-Volume.m3.) %>% 
  #select(-BC, -BS) %>%
  mutate(Volume.m3.bin= .bincode(Volume.m3., min_bin)) %>%
  mutate(row.sum = rowSums(.[1:(length(.)-2)])) %>% 
  filter(row.sum > 0) %>% 
  select(-row.sum)


rare_dat = rare %>% select(-Volume.m3., -Volume.m3.bin)
data=sample(c(1:length(rare_dat[,1])), size=100, replace=F)


### Total count of lakes species occur in the ADK -----





prop_comm = alsc_p.a %>% rownames_to_column(var = "lake_id") %>%
  pivot_longer(ST:AW, names_to ="Species", values_to =  "Abundance") %>%
  group_by(Species) %>% 
  filter(Abundance>0) %>%
  summarize(count = n()) %>%
  mutate(proportion = count / 1149)



# This needs it to be an abundance matrix
average_abundance = alsc_abundance %>% rownames_to_column(var = "lake_id") %>%
  pivot_longer(ST:AW, names_to ="Species", values_to =  "Abundance") %>%
  group_by(Species) %>% 
  filter(Abundance>0) %>% 
  group_by(Species) %>% 
  summarize(average_abundance = mean(Abundance))

prop_abund = prop_comm %>% 
  left_join(average_abundance) %>%
  mutate(prop_abund_metric  = proportion*average_abundance)

prop_abund %>% ggplot(aes(x = average_abundance, y = proportion, label = Species)) + geom_text()

## Most abundant family
alsc_p.a %>% 
  select((ST:(all_of(end_spp)-1))) %>%
  rownames_to_column() %>%
  pivot_longer(2:end_spp,names_to = "Species") 

### Cooccurance of fish + pairwise species overlap -----

coccur_matrix = alsc_p.a %>% 
  select((ST:(all_of(end_spp)-1))) %>%
  rownames_to_column() %>%
  pivot_longer(2:end_spp,names_to = "Species" ) %>%
  filter(value>0) %>% 
  mutate(species = Species) %>% 
  group_by(rowname) %>%
  complete( species,Species,value) %>% 
  ungroup() %>%  
  group_by(Species, species) %>%
  unique() %>% 
  summarize(cooccur = n())  %>%
  rename(s2 = species)
#dat_spec = dat_spec %>% select(-s2, -family_s2)
dat_spec2 = dat_spec %>%
  mutate(s2 = Species, familys2 = family) %>% select(s2, familys2)

overlap_coocur = overlap_data %>% 
  pivot_longer(2:length(.[1,]), names_to = "Community", values_to = "Values") %>% na.omit() %>%
  rename("Species_Pair" = `Spp Pair`) %>% 
  group_by(Species_Pair) %>% 
  #summarize(mean = mean(as.numeric(Values), na.rm = T)) %>%
  separate(Species_Pair, into = c("s1", "v", "s2"), sep = " ") %>% 
  select(-v) %>%
  filter(s1 != s2) %>% rename(Species = "s1") %>% 
  mutate(Species = str_replace(Species, "NPD", "PD")) %>% 
  mutate(Species = str_replace(Species, "BNM", "BM")) %>%
  mutate(s2 = str_replace(s2, "NPD", "PD")) %>% 
  mutate(s2 = str_replace(s2, "BNM", "BM")) %>%
  left_join(prop_comm) %>% 
  full_join(coccur_matrix) %>% left_join(dat_spec) %>% left_join(dat_spec) %>% left_join(dat_spec2) %>% unite("species_pair",family, familys2, remove = F) %>% 
  filter(Values >0) %>% 
  filter(species_pair %in% c("Centrarchid_Centrarchid", 
                             "leuciscidae_leuciscidae", 
                             "salmonidae_salmonidae",
                             "Centrarchid_leuciscidae",
                             "leuciscidae_Centrarchid",
                             "Catostomid_leuciscidae",
                             "Catostomid_Centrarchid"
                             
  )) ## Filter to just include the commonly sampled species (minnows and centrarchids)

#### Overlap Visualize ----------- 
labs_overlap = c("BK" = "banded killifish", "RS" = "rainbow smelt","FO" = "Allegheny crayfish", "FI"= "calico crayfish", "CR" = "robust crayfish", "BB" = "brown bullhead", "PS" = "pumpkinseed", "SMB" = "smallmouth bass", "RBS"= "redbreasted sunfish", "MM" = "Central Mudminnow", "LLS" = "Atlantic salmon", "ST" = "brook trout", "WS" = "white sucker", "NRD" =  "N. redbelly dace", "NPD"= "N. pearl dace", "GS" = "golden shiner", "FSD" = "finescale dace", "FHM" = "fathead minnow", "CS" = "common shiner", "CLM" = "cutlips minnow", "CC" = "creek chub", "BNM" = "bluntnose minnow", "BND" = "blacknose dace")

library(wesanderson)

pal <- wes_palette("Zissou1", 100, type = "continuous")
overlap_data %>% 
  #distinct(`Com 1`, `Com 2`, `Com 3`, `Com 4`, `Com 5`, `Com 7`, `Com 8`,`Com 9`, `Com 10`, `Com 11`,`Com 12`,`Com 13`,`Com 14`, `Com 15`, `Com 16`, `Com 17`,`Com 19`,`Com 20`,`Com 22`,`Com 23`,`Com 24`,`Com 25`,`Com 26` , .keep_all = T) %>%
  pivot_longer(2:length(.[1,]), names_to = "Community", values_to = "Values") %>% na.omit() %>%
  rename("Species_Pair" = `Spp Pair`) %>% 
  group_by(Species_Pair) %>% 
  summarize(mean = median(as.numeric(Values), na.rm = T)) %>%
  separate(Species_Pair, into = c("s1", "v", "s2"), sep = " ") %>% 
  select(-v) %>%
  filter(s1 != s2) %>% 
  filter(s1 != "FSD-NRD", s2 != "FSD-NRD") %>% 
  #distinct(mean, .keep_all = T) %>%
  ggplot(aes(x = s1, y = s2, col = as.numeric(mean))) +
  geom_point(size = 5) + 
  #scale_color_viridis() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5 )) +
  ylab("") + xlab("") +
  labs(color = "Overlap") + 
  scale_x_discrete(limits = level_order,labels =labs_overlap) +
  scale_y_discrete(limits = level_order, labels =labs_overlap) +
  scale_color_gradientn(colours = pal) 

overlap_coocur %>%
  filter(family %in% c("Centrarchid","leuciscidae", "Catostomid"), familys2 %in% c("Centrarchid","leuciscidae","Catostomid")) %>%
  ## Try to get mean points 
  group_by(Species, s2, cooccur, species_pair) %>% 
  summarise(Values = median(as.numeric(Values))) %>%
  mutate(cooccur = cooccur / 1469) %>%
  ggplot(aes(x = cooccur, y =as.numeric(Values))) +
  geom_point(col = "#7E9181", size = 3) + 
  geom_smooth(method = lm, col = "#7E9181") +
  #xlim(0,300) + 
  theme_minimal() +
  ylab("median pairwise overlap") + 
  xlab("co-occurrence (proportion of ALSC lakes)")

#### Stat Test -------------
over_tests = overlap_coocur %>%
  filter(family %in% c("Centrarchid","leuciscidae","Catostomid"), familys2 %in% c("Centrarchid","leuciscidae","Catostomid")) %>% 
  group_by(Species, s2, cooccur, species_pair) %>% 
  summarise(Values = median(as.numeric(Values)))

cor.test(as.numeric(over_tests$Values), over_tests$cooccur) ## -.27 cor value stat.sig result
summary(lm(over_tests$Values~ over_tests$cooccur)) # R2 .07 value, stat sig result


## Overlap native vs. non-native ------------------
nat = dat_spec %>% filter(upland_invasive == "nat") %>% select(Species)
int = dat_spec %>% filter(upland_invasive == "int") %>% select(Species)

overlap.filtered = overlap_data %>% 
  #distinct(`Com 1`, `Com 2`, `Com 3`, `Com 4`, `Com 5`, `Com 7`, `Com 8`,`Com 9`, `Com 10`, `Com 11`,`Com 12`,`Com 13`,`Com 14`, `Com 15`, `Com 16`, `Com 17`,`Com 19`,`Com 20`,`Com 22`,`Com 23`,`Com 24`,`Com 25`,`Com 26` , .keep_all = T) %>%
  pivot_longer(2:length(.[1,]), names_to = "Community", values_to = "Values") %>% na.omit() %>%
  rename("Species_Pair" = `Spp Pair`) %>% 
  group_by(Species_Pair) %>% 
  summarize(mean = median(as.numeric(Values), na.rm = T)) %>%
  separate(Species_Pair, into = c("s1", "v", "s2"), sep = " ") %>% 
  select(-v) %>%
  filter(s1 != s2) %>% 
  filter(s1 != "FSD-NRD", s2 != "FSD-NRD") %>%
  mutate(Species1 = case_when(s1 %in% nat$Species ~"0", s1 %in% int$Species ~ "1") ) %>%
  mutate(Species2 = case_when(s2 %in% nat$Species ~"0", s2 %in% int$Species ~ "1") ) %>%
  na.omit() %>%
  mutate(Species1 = as.numeric(Species1)) %>%
  mutate(Species2 = as.numeric(Species2)) %>%
  mutate(pair_status = Species1 + Species2) %>%
  mutate(pair_status = as.character(pair_status)) %>%
  mutate(pair_status = case_when(pair_status == "0" ~ "native", pair_status == "1" ~ "mixed", pair_status == "2" ~ "introduced"))

overlap.filtered %>%
  distinct(mean, .keep_all=TRUE) %>%
  mutate(pair_status = case_when(pair_status == "native" ~ "native", pair_status == "mixed" ~ "non-native", pair_status == "introduced" ~"non-native")) %>%
  group_by(pair_status)  %>%
  summarize(gr = mean(mean), sd = sd(mean))


overlap.filtered %>%
  distinct(mean, .keep_all=TRUE) %>% 
  ggplot(aes(x = pair_status, y = mean)) + geom_boxplot()

## Overlap NMDS  - I dont think this will really work because the values need to be replaced with zeros... so it makes things (that never overlap in physical distribution) look very similar in isotopic space if their overlap is very low 

## Overlap of minnows only -----------------

min_over = overlap_coocur %>% filter(family == "leuciscidae" & familys2 == "leuciscidae") %>% 
  unite("pair", c(Species, s2), remove = F) %>% 
  arrange(pair) %>% 
  group_by(Community) 

unique_pairs = combn(unique(min_over$Species),2,FUN=paste,collapse='_') %>% as.data.frame() %>% rename("pair" =  ".")  %>% mutate(slim_pair = pair) %>% full_join(min_over) %>% select(pair, Community, Values, Species, slim_pair) %>%  na.omit() %>% arrange(Species)


unique_pairs %>%
  ggplot(aes(x = Community, y = as.numeric(Values), label= pair)) + geom_text()

csv_overlap_minnows = unique_pairs %>% select(pair, Community, Values) %>% arrange(Community)

#write.csv(csv_overlap_minnows, file = "overlap_minnows.csv")
dev.off()
### Relative area against proportion  --------


## 
full_relative %>% group_by(WATER, SP) %>% 
  filter(SP %nin% c("FSD-NRD", "FO", "FI", "CR")) %>%
  summarise(mean_a = mean(Dat), SD_a = sd(Dat)) %>%
  ungroup() %>%
  group_by(SP) %>%
  summarize(mean_area = mean(mean_a), SD_all = sd(mean_a)) %>%
  rename(Species = SP) %>% 
  mutate(Species = str_replace(Species, "NPD", "PD")) %>% 
  mutate(Species = str_replace(Species, "BNM", "BM")) %>%
  left_join(dat_spec) %>%
  left_join(prop_abund) %>% 
  ggplot(.) +
  theme_minimal() + 
  geom_pointrange(aes(x = mean_area, xmin = mean_area + SD_all, xmax = mean_area -SD_all, y = reorder(Species, proportion))) + 
  geom_point(aes(x = mean_area, y = reorder(Species, proportion), col = proportion), size =4) +
  viridis::scale_color_viridis() + 
  ylab("") + 
  xlab("Mean Relative Area") + 
  labs(color="Proportion")



## Med area 
prop_bin = c(-.1,.15,.9)
med_area_count = med_relative.data %>%
  mutate(Species = str_replace(Species, "NPD", "PD")) %>% 
  mutate(Species = str_replace(Species, "BNM", "BM")) %>%
  left_join(prop_comm) %>%
  left_join(dat_spec) %>% 
  left_join(prop_abund) %>%
  group_by(Species, family, proportion, prop_abund_metric) %>%
  mutate(proportion_bin = .bincode(proportion, prop_bin))


#### Visualize ----
med_area_count %>%
  na.omit() %>% 
  group_by(Species,proportion, proportion_bin) %>% 
  summarize(med_area = mean(med_area)) %>%
  ggplot(aes(x = as.numeric(proportion), y = as.numeric(med_area), col = as.factor(proportion_bin))) + 
  geom_boxplot() +
  geom_point() +
  theme_minimal() + 
  ylab("Relative Area") + xlab("Proportion of ALSC Lakes") +
  labs(col = "Frequency of Occurence") + 
  scale_color_discrete(labels = c("Rare","Commmon")) 


mean_med = med_area_count %>%
  na.omit() %>% 
  group_by(Species,proportion, proportion_bin, family) %>% 
  summarize(med_area = mean(med_area))

## med relative area vs. proportion ------------
mean_med %>% filter(Species != "BB") %>% ggplot(aes(x = proportion, y = med_area, label = Species)) + geom_text(col = "#7E9181", size = 3) + geom_smooth(method = "lm",  col = "#7E9181") + theme_minimal() + 
  xlab("proportion of ALSC lakes") + ylab("relative niche area")

## med relative area vs. iso proportions

mean_med %>% left_join(iso_proportion) %>%


  ggplot(aes(x = proportion_iso, y = med_area)) +
  geom_point(col = "#7E9181", size = 3) + 
  geom_smooth(method = "lm",  col = "#7E9181") + theme_minimal() + 
  xlab("proportion of sampled lakes") + ylab("relative niche area")


## LM for above graph 

lm_iso_area.dat = mean_med %>% left_join(iso_proportion)
%>% filter(Species != "BB")

summary(lm(data = lm_iso_area.dat, med_area ~ proportion_iso))

med_area_count %>% 
  #filter(Species != "BB") %>%
  na.omit() %>% 
  group_by(Species,proportion, proportion_bin) %>% 
  summarize(med_area = mean(med_area)) %>% 
  ungroup() %>%
  group_by(proportion_bin) %>%
  summarize(sd_area = sd(med_area))

#### Relative area comparison --------------------------------------------------

relative_cred = full_relative %>% group_by(SP, post) %>% 
  summarize(average_dat = median(Dat)) 
quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


full_relative %>% group_by(SP, post) %>% 
  summarize(average_dat = mean(Dat)) %>%
  filter(SP %nin% c("FSD-NRD", "FO", "FI", "CR")) %>%
  ungroup() %>%
  rename(Species = SP) %>% 
  mutate(Species = str_replace(Species, "NPD", "PD")) %>% 
  mutate(Species = str_replace(Species, "BNM", "BM")) %>%
  left_join(dat_spec) %>%
  left_join(prop_abund) %>% 
  ggplot(., aes(y = reorder(Common, proportion), x = average_dat, fill = proportion)) +
  theme_minimal() + 
  stat_summary(fun.data = quantiles_95, geom="boxplot",color = "gray") +
  #viridis::scale_fill_viridis() + 
  ylab("") + 
  xlab("Mean Relative Area") + 
  labs(color="Proportion") +
  scale_fill_gradientn(colours = pal)


## Cred tests between species pairs
## Start here!
S1 = relative_cred %>% filter(SP == "CLM")
S2 = relative_cred %>% filter(SP == "BK")


vector = (S1$average_dat - S2$average_dat)


data.frame(mean_dif = mean(vector), quant_05 = quantile(vector, c(.05)), quant_95 = quantile(vector, .95))


dumb
#### Stat test -----------
#T.test the two groups - if doing all points id stick with t test, if doing mean med_area i would do wilcoxon or something non-parametric
t.test(med_area_count$med_area[which(med_area_count$proportion_bin == 1)],med_area_count$med_area[which(med_area_count$proportion_bin == 2)] )
summary(lm(data = med_area_count %>% filter(Species != "BB"), log(med_area) ~ log(proportion)))

t.test(mean_med$med_area[which(mean_med$proportion_bin == 1)],mean_med$med_area[which(mean_med$proportion_bin == 2)] )

summary(lm(data = mean_med %>% filter(Species != "BB"), med_area ~ (proportion))) ## Sig
summary(lm(data = mean_med , med_area ~ (proportion))) ## Only significant if you remove bullhead
## Not significant
summary(lm(data = mean_med %>% filter(family == "leuciscidae"), med_area~proportion))


## Did I already try one value per? 


med_area_count %>% group_by(Species, proportion) %>%
  summarize(mean_med_area = median(med_area, na.rm = T)) %>% 
  ggplot(aes(x = proportion, y = mean_med_area)) + geom_point() + 
  geom_smooth(method = "lm")

med_area_count %>% group_by(Species, proportion) %>%
  summarize(mean_med_area = median(med_area, na.rm = T)) %>%
  filter(Species != "BB") %>%
  lm(data = ., mean_med_area~(proportion)) %>% summary()

cat = med_area_count %>% group_by(Species, proportion) %>%
  summarize(mean_med_area = median(med_area, na.rm = T)) 


cor.test(cat$mean_med_area,cat$proportion, method = "spearman")
## NMDS ----------------
dat_spec.frame = dat_spec %>% as.data.frame() %>%
  mutate(nat_therm = paste(upland_invasive, thermal)) %>%
  mutate(group_therm = paste(upland_invasive,group)) 
data=sample(c(1:length(rare_dat[,1])), size=200, replace=F)
nmds=metaMDS(rare_dat[data,], k=4, trymin =  50)
nmds$species %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  mutate(Species = rownames(.)) %>% 
  left_join(dat_spec.frame) %>% 
  as.data.frame() %>%
  ggplot(aes(x = MDS1, y = MDS2,
             label =(Species), col = nat_therm)) + 
  theme_minimal() + 
  geom_text(size =4) + 
  stat_ellipse(level = .9) + 
  ggtitle("ALSE - abundance, rare = 50, k = 4, size = 200")


##ggtitle("ALSC - abundance, rare = 10, k = 2, size = 900") 


nmds_minnow = metaMDS(minnows[,1:length(NatMin)], k = 2, try = 100)

## what average sized body of water does each species inhabit
newest_bin = c(0,200000, 500000, 1000000, 5000000, 10000000, 50000000, 1500000000)
min_bin = c(0,200000, 150000000000000000)
lake_size = alsc_p.a %>% rownames_to_column(var = "lake_id") %>%
  pivot_longer(ST:AW, names_to ="Species", values_to =  "Abundance") %>%
  group_by(Species) %>% 
  filter(Abundance>0) %>%
  summarize(mean_vol = median(Volume.m3.), sd_vol = sd(Volume.m3.)) %>%
  mutate(lake_size_bin = .bincode(mean_vol, min_bin )) 

nmds$points


nmds$species %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  mutate(Species = rownames(.)) %>% 
  left_join(dat_spec.frame) %>% 
  as.data.frame() %>%
  left_join(lake_size) %>%
  ggplot(aes(x = MDS1, y = MDS2,
             label =(Species), col = as.factor(lake_size_bin))) + 
  
  geom_text(size =4)+ 
  stat_ellipse(level = .9) + 
  ggtitle("ALSC - abundance, rare = 10, k = 2, size = 900")+
  labs(color = "Lake size: small (1) - big (5)")

nmds$points %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  mutate(Species = rownames(.)) %>% 
  left_join(dat_spec.frame) %>% 
  #as.data.frame() %>%
  #left_join(lake_size) %>%
  ggplot(aes(x = MDS1, y = MDS2,
             label =(Species), col = as.character(nmds_comm$Volume.m3.bin))) + 
  
  #geom_point(size =4)+ 
  stat_ellipse(level = .9) + 
  ggtitle("ALSC - abundance, rare = 10, k = 2, size = 900")+
  xlim(-.1,.02) + 
  labs(color = "Lake size: small (1) - big (5)")


nmds_comm = t(rare[data,]) %>% as.data.frame() 
included_species = rownames(nmds_comm)[c(-35,-36)]
anosim_lakesize = lake_size %>% filter(Species %in% included_species)
ano = anosim(nmds_comm[c(-35,-36),] , anosim_lakesize$lake_size_bin, distance = "bray", permutations = 9999)
ano


## -------------------- Trophic Position ----------------------------

baselines.1 = baselines %>% filter(FAMILY %in% c("HEPTAGENIIDAE", "BULK_ZOOP","LEPTOPHLEBIIDAE","PLANORBIDAE","VIVIPARIDAE","SPHAERIDAE","CAENIDAE")) %>% 
  filter(WATER !="COM") %>%
  group_by(WATER) %>% 
  summarize(base_d15N = mean(d15N, na.rm = T))%>% 
  mutate(consumer = 1)

baselines.2 = baselines %>% filter(WATER == "COM") %>% 
  group_by(WATER) %>%
  summarize(base_d15N = mean(d15N, na.rm = T)) %>%
  mutate(consumer = 2)

baselines = full_join(baselines.1, baselines.2)  

trophic_pos  = df_iso %>% filter(TAXA != "INVERT") %>% select(WATER, CODE, d15N) %>% 
  left_join(baselines) %>% 
  mutate(trophic_position = 2 + ((d15N-base_d15N) / (3.14))) %>%
  group_by(WATER, CODE) %>%
  summarize(mean_troph = mean(trophic_position, na.rm=T), sd_troph = sd(trophic_position, na.rm = T)) %>%
  na.omit() %>% 
  mutate(CODE = str_replace(CODE, "BNM", "BM")) %>% 
  mutate(CODE = str_replace(CODE, "NPD", "PD"))


trophic_pos.simp = df_iso %>% filter(TAXA != "INVERT") %>% select(WATER, CODE, d15N) %>% 
  left_join(baselines) %>% 
  mutate(trophic_position = 2 + ((d15N-base_d15N) / (3.14))) %>%
  group_by(CODE) %>%
  summarize(mean_troph = mean(trophic_position, na.rm=T), sd_troph = sd(trophic_position, na.rm = T))

#write.csv(trophic_pos.simp, "trophic_position_frame.csv")
dev.off()

## fishbase aspect
library(rfishbase)
## For fish species we have isotope information on only
adk_fishspecies = c("Margariscus nachtriebi", "Margariscus margarita","Luxilus cornutus","Chrosomus eos","Notemigonus crysoleucas","Pimephales promelas","Rhinichthys atratulus","Semotilus atromaculatus" ,"Couesius plumbeus","Rhinichthys cataractae","Hybognathus hankinsoni","Pimephales notatus", "Exoglossum maxillingua","Chrosomus neogaeus","Semotilus corporalis", "Ameiurus nebulosus", "Fundulus diaphanus", "Salmo salar", "Umbra limi", "Lepomis gibbosus", "Lepomis auritus","Osmerus mordax", "Micropterus dolomieu", "Cottus cognatus","Salvelinus fontinalis", "Catostomus commersonii")

adk_fish_codes = c("PD", "PD","CS", "NRD","GS","FHM","BND","CC","LC","LND","ESM","BM","CLM","FSD","FF", "BB","BK","LLS","MM","PS","RBS","RS","SMB","SS","ST","WS","Chrosomus neogaeus") %>%
  cbind(adk_fishspecies) %>% as.data.frame() %>% 
  rename(Species = adk_fishspecies) %>% 
  rename(code = '.')

## for all fish species in ALSC database


alsc_fishspecies = c("Salvelinus fontinalis", "Salmo trutta", "Oncorhynchus mykiss","Exoglossum maxillingua", "Esox lucius", "Esox niger", "Cyprinus carpio", "Notemigonus crysoleucas","Luxilus cornutus","Semotilus atromaculatus","Catostomus commersonii","Chrosomus eos","Pimephales notatus","Ameiurus nebulosus","Lepomis gibbosus","Ambloplites rupestris", "Perca flavescens","Micropterus salmoides","Micropterus dolomieu","Lepomis auritus", "Pomoxis nigromaculatus","Culaea inconstans", "Umbra limi", "Rhinichthys atratulus", "Fundulus diaphanus", "Salvelinus namaycush", "Salvelinus fontinalis × Salvelinus", "Oncorhynchus nerka", "Catostomus catostomus", "Couesius plumbeus" , "Semotilus corporalis","Margariscus margarita", "Lepomis macrochirus",  "Sander vitreus",  "Esox masquinongy X Esox lucius", "Ameiurus natalis", "Prosopium cylindraceum", "Pimephales promelas", "Cottus cognatus","Rhinichthys cataractae","Salmo salar", "Hybognathus regius", "Osmerus mordax" , "Hybognathus hankinsoni", "Coregonus clupeaformis", "Coregonus artedi", "Salmo trutta × Salvelinus fontinalis", "Anguilla rostrata" ,"Chrosomus neogaeus", "Erimyzon oblongus", "Salmo salar", "Notropis volucellus", "Notropis bifrenatus","Alosa pseudoharengus"  )

alsc_fish_codes = (alsc_abundance %>% colnames())[1:end_spp-1] %>% 
  cbind(alsc_fishspecies) %>% 
  as.data.frame() %>% 
  rename(Species = alsc_fishspecies) %>% 
  rename(code = '.')

#write.csv(alsc_fish_codes,"alsc_fish_codes.csv")

## Native fish codes ALSC 

alsc_fish_codes_native = alsc_fish_codes %>% filter(code %nin% c("BT","GS","RT","CP","CRP","RB","YP","LMB","SMB","BC","MM","BK","SPL","KOK","BG","WE","TM","LLS","ESM","RS","BRM","TGT","SAL","MCS","BDS","AW", "BM"))
species_fields

ecology_adk = ecology(alsc_fishspecies) %>% select(SpecCode, Species, Herbivory2, FeedingType, DietTroph, FoodTroph)

length_matrix =  species(alsc_fishspecies, fields = c("Species","CommonLength"))

ecology = left_join(ecology_adk, length_matrix)

fish_base_propcom = prop_comm %>% rename(CODE ="Species")

ecology_adk %>% left_join(alsc_fish_codes) %>% 
  rename(CODE = code) %>%
  full_join(trophic_pos) %>%
  rename(Fishbase = FoodTroph, ADK = mean_troph) %>% 
  pivot_longer(., c(Fishbase, ADK),
               names_to = "Source", 
               values_to = "Trophic_Position") %>% left_join(fish_base_propcom) %>% 
  filter(proportion > 0) %>%
  
  ggplot(aes(x = Trophic_Position, y = reorder(CODE, proportion), col = proportion, shape = Source)) + 
  geom_point(size = 3)+
  theme_minimal()


ecology_adk %>% left_join(adk_fish_codes) %>% 
  rename(CODE = code) %>%
  full_join(trophic_pos.simp) %>%
  rename(Fishbase = DietTroph, ADK = mean_troph) %>% 
  left_join(fish_base_propcom)%>% 
  filter(proportion > 0) %>%
  filter(CODE %nin% c("LND", "ESM")) %>%
  ggplot() + 
  geom_point(aes(x = Fishbase, 
                 y = reorder(CODE, proportion), 
                 size = proportion,shape ="1", col = Herbivory2))+
  geom_point(aes(x = ADK,
                 y = CODE, 
                 size = proportion, 
                 shape = "2"), 
             size = 4) + 
  geom_pointrange(aes(x = ADK, y = CODE, xmin = ADK - sd_troph, xmax = ADK + sd_troph))+
  theme_minimal() + 
  ylab("") + 
  xlab("Trophic Position") +
  scale_shape_manual("Data Source", 
                     values =c(15,16), 
                     labels = c("fishbase", "ADK Isotopes"))



ecology_adk %>% left_join(adk_fish_codes) %>% 
  rename(CODE = code) %>%
  full_join(trophic_pos.simp) %>%
  rename(Fishbase = DietTroph, ADK = mean_troph) %>% 
  left_join(fish_base_propcom)%>% 
  filter(proportion > 0) %>%
  filter(CODE %nin% c("LND", "ESM")) %>%
  select(CODE, Fishbase, FoodTroph, ADK, sd_troph, proportion) %>%
  pivot_longer(Fishbase:ADK, names_to = "Type", values_to = "TP") %>%
  na.omit() %>%
  mutate(Functional = case_when(TP >= 2.8 ~ "mainly animals", TP < 2.8 & TP >= 2.2 ~ "plants, detritus, animals", TP < 2.2 ~ "plants + detritus")) %>%
  rename(Species = CODE) %>%
  left_join(dat_spec) %>%
  ggplot(.) + 
  
  geom_pointrange(data = . %>% filter(Type == "ADK"), aes(x = TP, y = Common, xmin = TP - sd_troph, xmax = TP + sd_troph)) +
  geom_point(aes(x = TP, 
                 y = reorder(Common, proportion),  shape = Type, col = Functional), size = 5)+
  #geom_pointrange(aes(x = TP, y = CODE, xmin = ADK - sd_troph, xmax = ADK + sd_troph))+
  theme_minimal() + 
  ylab("") + 
  xlab("Trophic Position") + 
  scale_color_manual(values = c(wes_palette("Cavalcanti1",5)[1], wes_palette("Cavalcanti1",5)[4])) +
  scale_shape_discrete( labels = c("ADK Isotopes", "Fishbase Diet Only", "Fishbase Trophic"))






ecology_adk_long = ecology_adk %>% left_join(alsc_fish_codes) %>%
  select(-Species) %>%
  rename(Species = code) %>%
  left_join(dat_spec) %>%
  select(Species, everything()) %>%
  rename(CODE = Species) %>%
  full_join(trophic_pos) %>%
  rename(Fishbase = FoodTroph, ADK = mean_troph) %>% 
  pivot_longer(., c(Fishbase, ADK),
               names_to = "Source", 
               values_to = "Trophic_Position") %>% left_join(fish_base_propcom) %>%
  select(CODE, Source, Trophic_Position, proportion, upland_invasive) %>%
  filter(Source == "Fishbase") %>%
  filter(CODE != "SAL")



wilcox.test((ecology_adk_long %>% filter(upland_invasive == "int"))$Trophic_Position, (ecology_adk_long %>% filter(upland_invasive == "nat"))$Trophic_Position)


ecology_adk_long %>%
  select(Trophic_Position, upland_invasive) %>%
  na.omit() %>%
  ggplot(aes(y = Trophic_Position, x =upland_invasive, col = upland_invasive)) + geom_boxplot()

sd((ecology_adk_long %>% filter(upland_invasive == "int"))$Trophic_Position)
sd((ecology_adk_long %>% filter(upland_invasive == "nat"))$Trophic_Position)



ecology_adk_long %>%
  select(Trophic_Position, upland_invasive, proportion, CODE) %>%
  na.omit() %>%
  group_by(CODE, proportion, upland_invasive) %>% 
  summarize(Trophic_Position = mean(Trophic_Position)) %>%
  ggplot(aes(x = Trophic_Position, y = reorder(CODE, proportion), col = proportion, shape = upland_invasive)) + 
  theme_minimal() + 
  geom_point(size = 3) + 
  scale_color_viridis_b() +
  ylab("") + 
  xlab("Trophic Position") + 
  labs(col = "Proportion") +
  scale_shape_discrete(labels = c("Non-native","Lowland Only","Native")) +
  labs(shape = "Status")
## Trying to get either mine or fishbase, not both
ecology_adk %>% left_join(alsc_fish_codes) %>%
  select(-Species) %>%
  rename(Species = code) %>%
  left_join(dat_spec) %>%
  select(Species, everything()) %>%
  rename(CODE = Species) %>%
  full_join(trophic_pos) %>%
  rename(Fishbase = FoodTroph, ADK = mean_troph) %>%
  select(CODE, Fishbase, ADK) %>%
  mutate(new = case_when(ADK > 0 ~ ADK, ADK < 0 ~ "ADK"))


pivot_longer(., c(Fishbase, ADK),
             names_to = "Source", 
             values_to = "Trophic_Position") %>% left_join(fish_base_propcom) %>%
  select(CODE, Source, Trophic_Position, proportion, upland_invasive) %>%
  group_by(CODE)# %>%
#mutate(new = case_when(T)


## Trying to combine fishbase trophic position with the ALSC abundance



ecology_adk_long %>% ggplot(aes(x = proportion, y = Trophic_Position)) + geom_point()



alsc_abundance %>% mutate(ID = c(1:length(alsc_abundance[,1]))) %>%
  select(1:end_spp-1, ID) %>%
  pivot_longer(1:end_spp-1, names_to = "CODE",values_to = "Abundance") %>%
  left_join(ecology_adk_long) %>% 
  group_by(CODE) %>% 
  mutate(Abundance = as.numeric(Abundance)) %>%
  mutate(Abundance = replace(Abundance,Abundance == 0, NA)) %>%
  na.omit() %>%
  summarize(mean_abundance = mean(Abundance), mean_trophic = mean(Trophic_Position)) %>%
  
  ggplot(aes(x = mean_trophic, y = mean_abundance)) + geom_point() + geom_smooth()



alsc_abundance %>% mutate(ID = c(1:length(alsc_abundance[,1]))) %>%
  select(1:end_spp-1, ID) %>%
  pivot_longer(1:end_spp-1, names_to = "CODE",values_to = "Abundance") %>%
  left_join(ecology_adk_long) %>% 
  group_by(CODE) %>% 
  mutate(Abundance = as.numeric(Abundance)) %>%
  mutate(Abundance = replace(Abundance,Abundance == 0, NA)) %>%
  na.omit() %>%
  summarize(mean_abundance = mean(Abundance), mean_trophic = mean(Trophic_Position)) %>%
  
  ggplot(aes(x = mean_trophic)) + geom_histogram()


alsc_abundance %>% mutate(ID = c(1:length(alsc_abundance[,1]))) %>%
  select(1:end_spp-1, ID) %>%
  pivot_longer(1:end_spp-1, names_to = "CODE",values_to = "Abundance") %>%
  left_join(ecology_adk_long) %>% 
  group_by(CODE) %>% 
  mutate(Abundance = as.numeric(Abundance)) %>%
  mutate(Abundance = replace(Abundance,Abundance == 0, NA)) %>%
  na.omit() %>%
  ungroup() %>%
  group_by(ID) %>%
  summarize(highest_abundance = first(Abundance), highest_code = first(CODE), highest_trophic = first(Trophic_Position)) %>%
  ggplot(aes(x = highest_trophic)) + geom_histogram(bins = 10) + 
  geom_vline(xintercept = 2.8)+ 
  geom_vline(xintercept = 2.1)


summarize(mean_abundance = mean(Abundance), mean_trophic = mean(Trophic_Position)) %>%
  
  ggplot(aes(x = mean_trophic)) + geom_histogram()



ecology_adk %>% select(Herbivory2) %>%
  na.omit() %>%
  ggplot(aes(x = Herbivory2)) + geom_histogram(stat = "count")

ecology %>% ggplot(aes(x = CommonLength, y = FoodTroph, col = Herbivory2)) + geom_point()

ecology %>% select(CommonLength, Herbivory2, FoodTroph) %>%
  na.omit() %>%
  ggplot(aes(x = as.numeric(CommonLength), fill = Herbivory2)) + geom_histogram(bins = 9) + 
  facet_wrap(~Herbivory2) +
  ylab("Total Species") + 
  xlab("Common Length (cm)") + 
  theme_minimal() + 
  theme(legend.position = "none")


ecology %>% filter(Herbivory2 == "mainly animals (troph. 2.8 and up)") %>% 
  ggplot(aes(x = CommonLength, y = FoodTroph)) + geom_point() + 
  geom_smooth(method = "lm")


dat_spec.mod = dat_spec %>% rename(CODE = Species)

ecology %>% left_join(alsc_fish_codes) %>% 
  rename(CODE = code) %>%
  left_join(dat_spec.mod) %>%
  filter(Herbivory2 == "mainly animals (troph. 2.8 and up)") %>%
  filter(upland_invasive != "low_only") %>%
  mutate(upland_invasive = case_when(upland_invasive == "int" ~ "introduced", upland_invasive == "nat" ~ "native")) %>%
  ggplot(aes(x = upland_invasive, y = FoodTroph)) +
  theme_minimal() + 
  geom_boxplot(aes(fill = upland_invasive)) +
  geom_point() +
  ylab("Trophic Position") + 
  xlab("") +
  scale_fill_manual(values = c(wes_palette("Zissou1",5)[1], wes_palette("Cavalcanti1",5)[3])) + 
  theme(legend.position = "none")


nat.pred = ecology %>% left_join(alsc_fish_codes) %>% 
  rename(CODE = code) %>%
  left_join(dat_spec.mod) %>%
  filter(Herbivory2 == "mainly animals (troph. 2.8 and up)", upland_invasive == "nat")

inv.pred = ecology %>% left_join(alsc_fish_codes) %>% 
  rename(CODE = code) %>%
  left_join(dat_spec.mod) %>%
  filter(Herbivory2 == "mainly animals (troph. 2.8 and up)", upland_invasive == "int")

t.test(nat.pred$FoodTroph, inv.pred$FoodTroph)

t.test(nat.pred$CommonLength, inv.pred$CommonLength)


stop


## Creek chub ---------------
CC_troph = df_iso %>% filter(TAXA != "INVERT") %>% select(WATER, CODE, d15N) %>% 
  left_join(baselines) %>% 
  mutate(trophic_position = 2 + ((d15N-base_d15N) / (3.14)))# %>% 
  #filter(CODE == "CC")

write_csv(df_iso, "df_iso.csv")


pred_join = trophic_pos %>% 
  filter(CODE == "CC") %>%
  ungroup() %>% 
  mutate(pred = c(0,0,1, 1, 1, 0, 0)) %>% 
  select(WATER, pred) 


dat_troph = CC_troph %>%
  left_join(pred_join) %>%
  na.omit()

t.test((dat_troph %>% filter(pred == 0))$trophic_position, (dat_troph %>% filter(pred == 1))$trophic_position)

CC_troph %>%
  left_join(pred_join) %>%
  na.omit() %>%
  ggplot(aes(x = as.factor(pred), y = trophic_position, fill = as.factor(pred))) + 
  geom_boxplot() +
  theme_minimal() +
  ylab("Trophic Position") + 
  xlab("Predator Present") +
  scale_fill_manual(values = c(wes_palette("Zissou1",5)[1], wes_palette("Cavalcanti1",5)[3])) + 
  theme(legend.position = "none")




## Overlap data 

overlap_coocur %>%
  filter(Species == "CC" | s2 == "CC") %>%
  distinct(Values,.keep_all = T) %>%
  mutate(Community = parse_number(Community)) %>% left_join(lake_2023) %>%
  rename(WATER = Water) %>%
  left_join(pred_join) %>%
  mutate(pred_new = case_when(is.na(pred)==T ~ 0,
                              is.na(pred)==F ~ pred)) %>%
  ggplot(aes(x = as.factor(pred_new),
             y = as.numeric(Values), fill = as.factor(pred_new))) +
  geom_boxplot() +
  theme_minimal()+
  scale_fill_manual(values = c(wes_palette("Zissou1",5)[1], wes_palette("Cavalcanti1",5)[3])) + 
  theme(legend.position = "none") +
  ylab("median pairwise overlap") +
  xlab("predator presence")
  
  
dat_cc_overlap = overlap_coocur %>% 
  #filter(Species == "CC" | s2 == "CC") %>%
  distinct(Values,.keep_all = T) %>%
  mutate(Community = parse_number(Community)) %>% left_join(lake_2023) %>%
  rename(WATER = Water) %>%
  left_join(pred_join) %>%
  mutate(pred_new = case_when(is.na(pred)==T ~ 0, is.na(pred)==F ~ pred)) %>%
  filter(pred >= 0) %>%
  mutate(Values = as.numeric(Values))

wilcox.test(
  (dat_cc_overlap %>% filter(pred == 0))$Values, 
  (dat_cc_overlap %>% filter(pred == 1))$Values)


## Relative vs. pred

dat_cc_relative = full_relative %>% group_by(WATER, SP) %>% 
  filter(SP %nin% c("FSD-NRD", "FO", "FI", "CR")) %>%
  summarise(mean_a = median(Dat), SD_a = sd(Dat)) %>% 
  rename(Name = WATER) %>%
  left_join(lake_2023) %>%
  rename(WATER = Water) %>%
  left_join(pred_join) %>%
  mutate(pred_new = case_when(is.na(pred)==T ~ 0, is.na(pred)==F ~ pred))

t.test(
  (dat_cc_relative %>% filter(pred_new == 0))$mean_a, 
  (dat_cc_relative %>% filter(pred_new == 1))$mean_a)

dat_cc_relative%>%
  ggplot(aes(x = as.factor(pred_new), y = mean_a, fill=as.factor(pred_new))) + 
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c(wes_palette("Zissou1",5)[1], wes_palette("Cavalcanti1",5)[3])) + 
  theme_minimal() + 
  theme(legend.position = "none") +
  ylab("median relative area") +
  xlab("predator presence") + 
  ylim(0,.5)


## Nesdedness -----------------------------------------------------------
dat_spec = read.csv("species_alsc.csv")
## Overall community nestedness
watershed_pres = alsc %>%
  pivot_longer(ST:AW, names_to ="Species", values_to =  "Presence") %>% 
  left_join(dat_spec) %>% 
  filter(upland_invasive == "nat") %>% ## filters to only include native species 
  select(Common, Presence, Watershed) %>% 
  group_by(Watershed, Common) %>%
  summarize(watershed_pres = sum(Presence)) %>% 
  mutate(watershed_pres = case_when(
    watershed_pres > 0 ~ 1, 
    TRUE   ~ watershed_pres)) %>%
  mutate(watershed_pres = as.numeric(watershed_pres)) %>%
  pivot_wider(names_from = Common, values_from = watershed_pres)%>% 
  column_to_rownames(var = "Watershed") %>% t()

k = nestednodf(watershed_pres)

k$statistic



k$comm %>% as.data.frame %>%
  rownames_to_column(var = "rowname") %>%
  pivot_longer(2:9, names_to = "Watershed", values_to = "Nest") %>%
  rename(Species = rowname) %>%
  left_join(dat_spec) %>%
  ggplot(aes(y = factor(Species, 
                        level = rownames(k$comm)),
             x = factor(Watershed,
                        level = colnames(k$comm)), 
             fill = as.factor(Nest))) +
  theme_minimal() + 
  geom_tile()  +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size = 12)) + 
  theme(axis.text.y = element_text(size = 12)) + 
  scale_fill_manual(values = c(
    "0" = "white",
    "1" = "#7E9181"
  )) + 
  
  ylim(rev(rownames(k$comm))) +  
  ylab("") +
  xlab("") +
  theme(legend.position="none")

## NMDS -------------------------------------
# More code is in the oversaved_alsctidyworkflow.R

dat_spec.frame = dat_spec %>% as.data.frame() %>% mutate(nat_therm = paste(upland_invasive, thermal)) %>%
  mutate(group_therm = paste(upland_invasive,group))
nmds_codes = alsc_fish_codes %>% rename( "Scientific" = Species ) %>% 
  rename("Species" = code)

nmds=metaMDS(rare[data,], k=2)
nmds$species %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  mutate(Species = rownames(.)) %>% 
  left_join(dat_spec) %>% 
  left_join(nmds_codes) %>%
  as.data.frame() %>%
  ggplot(aes(x = MDS1, y = MDS2,
             label =(Common), color = upland_invasive)) + 
  
  theme_minimal() +
  geom_text(size =3.5,key_glyph = "rect")+ 
  stat_ellipse(level = .9, size = 1) + 
  #ggtitle("ALSC - pres/abs, rare = 10, k = 2, size = 900")+
  xlim(-.1,.05) +
  labs(col = "Status") +
  #scale_color_discrete( labels = c(int = "Invasive", nat = "Native", low_only = "Lowland Native"), ) +
  scale_color_manual(labels = c("Invasive","Lowland Native","Native"), values= c("#588157","#988F2A","#725752")) 



## Dummy graph

dummy = data.frame(dummy_datax = c(rnorm(12, mean = 5, sd = 2), rnorm(12, mean = 7, sd = 2)),
                   dummy_datay = c(rnorm(12, mean = 3, sd = .5), rnorm(12, mean = 5, sd = .5)),
                   species = c(rep("fish spp.A", 12), rep("fish spp.B", 12)))


ggplot(dummy, aes(x = dummy_datax, y = dummy_datay, col = species, shape = species), color ="#7E9181" ) +
  theme_minimal() + 
  geom_point( size = 3) + stat_ellipse(level = .9) + 
  ylab("Trophic Position Gradient (d15N)") + 
  xlab("Carbon Resource Gradient (d13C)") +
  #ylim(0,5) + 
  #xlim(0,10) + 
  scale_color_manual(values = c("#7E9181","#E1AF00")) + 
  theme(text = element_text(size = 13))
wes_palette("Zissou1",5)[4]

## iso overlap with just my lakes need to fix the dat matrix to reflect ST and LML fish
overlap_data %>% 
  pivot_longer(2:length(.[1,]), names_to = "Community", values_to = "Values") %>% na.omit() %>%
  rename("Species_Pair" = `Spp Pair`) %>% 
  group_by(Species_Pair) %>% 
  summarize(mean = mean(as.numeric(Values), na.rm = T)) %>%
  separate(Species_Pair, into = c("s1", "v", "s2"), sep = " ") %>% 
  select(-v) %>%
  filter(s1 != s2) %>% rename(Species = "s1") %>% 
  mutate(Species = str_replace(Species, "NPD", "PD")) %>% 
  mutate(Species = str_replace(Species, "BNM", "BM")) %>%
  mutate(s2 = str_replace(s2, "NPD", "PD")) %>% 
  mutate(s2 = str_replace(s2, "BNM", "BM")) %>%
  full_join(cooccurr_iso) %>%
  ggplot(aes(x = cooccurr, y = mean)) + 
  geom_point() + 
  geom_smooth(method = lm)


 %>% 
  full_join(coccur_matrix) %>% 
  left_join(dat_spec) %>% 
  left_join(dat_spec) %>%
  left_join(dat_spec2) %>% 
  unite("species_pair",family, familys2, remove = F) %>% 
  filter(Values >0) %>% 
  filter(species_pair %in% c("Centrarchid_Centrarchid", 
                             "leuciscidae_leuciscidae", 
                             "salmonidae_salmonidae",
                             "Centrarchid_leuciscidae",
                             "leuciscidae_Centrarchid",
                             "Catostomid_leuciscidae",
                             "Catostomid_Centrarchid"

