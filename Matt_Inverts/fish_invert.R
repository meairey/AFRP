invert = read.csv("Matt_Inverts/DNET_dat.csv") %>%
  unite("SAMP_ID", ID_1, ID_2, sep = "")

sample = read.csv("Matt_Inverts/SAMPLE_LML_MW.csv")

habitats = read.csv("Data/BEFsites_LengthAndHabitat.csv") %>%
  filter(Water == "LML") %>% select(Water, SITE, Habitat)


data = left_join(invert, sample)


dat_n = data %>% 
  group_by(FAMILY, GENUS, BEF_ID) %>%
  summarize(total_N = sum(N_IND))

data_family = data %>% 
  group_by(FAMILY, BEF_ID) %>%
  summarize(total_N = sum(N_IND)) %>%
  ungroup() %>%
  pivot_wider(names_from = FAMILY, values_from = total_N) %>%
  column_to_rownames(var = "BEF_ID") 

family_NMDS = metaMDS(data_family, k = 2,try = 50)

family_NMDS$points %>% as.data.frame() %>%
  mutate(SITE = rownames(.)) %>%
  mutate(SITE = as.numeric(SITE)) %>%
  left_join(habitats) %>%
  mutate(CLASS = substr(Habitat, 1, 1)) %>%
  #filter(SITE != 17) %>%
  ggplot(aes(x = MDS1, MDS2,  col = Habitat)) +
  geom_text(aes(label = as.factor(SITE)),) + 
  stat_ellipse(level = .9)



data_genus = data %>% 
  group_by(FINESCALE_ID, BEF_ID) %>%
  summarize(total_N = sum(N_IND)) %>%
  ungroup() %>%
  pivot_wider(names_from = FINESCALE_ID, values_from = total_N) %>%
  column_to_rownames(var = "BEF_ID")

finescale_NMDS = metaMDS(data_genus, k = 2,try = 50) 

finescale_sites = finescale_NMDS$points %>% as.data.frame() %>%
  mutate(SITE = rownames(.)) %>%
  mutate(SITE = as.numeric(SITE)) %>%
  left_join(habitats) %>%
  mutate(CLASS = substr(Habitat, 1, 1))# %>%
  #filter(SITE != 17) %>%
finescale_species = finescale_NMDS$species %>% as.data.frame() %>% select(MDS1, MDS2) %>% 
  mutate(SPECIES = rownames(.))

ggplot() +
  geom_text(data = finescale_sites, aes(x = MDS1, MDS2, label = as.factor(SITE), col = CLASS)) +
  stat_ellipse(data = finescale_sites, aes(x = MDS1, MDS2, col = CLASS)) + 
  geom_text(data = finescale_species, aes(x = MDS1, y = MDS2, label = as.factor(SPECIES)))


data %>%
  rename(SITE = BEF_ID) %>%
  group_by(SITE) %>% 
  summarize(total_N = sum(N_IND)) %>%
  ungroup() %>% left_join(habitats) %>%
  filter(total_N > 0) %>%
  ggplot(aes(x = Habitat, y = total_N)) + 
  geom_boxplot(outlier.shape =NA) + 
  geom_point()


data_genus %>% diversity("shannon") %>% 
  as.data.frame() %>%
  rename(diversity = '.') %>%
  mutate(SITE = as.numeric(rownames(.))) %>%
  left_join(habitats) %>%
  ggplot(aes(x = Habitat, y = diversity)) + 
  geom_boxplot() + geom_point()

## Loading in the fish data

## These are large files that I don't want to upload to github please source from your own computer
## LML 
source("AFRP_Master_Code/AFRP_Functions.R")
fish = read.csv("../AFRP/Data/FISH_MEASUREMENT_LML.csv") 

#fish = read.csv("../AFRP/MA2276_Code/Data/FISH_MEASUREMENT_2022.csv")
sample = read.csv("../AFRP/MA2276_Code/Data/FISH_SAMPLE_2022.csv")
sites = read.csv("../AFRP/MA2276_Code/Data/SITES.csv")
shoreline_length = read.csv("../AFRP/MA2276_Code/Data/BEFsites_LengthAndHabitat.csv")

sample = read.csv("../AFRP/Data/FISH_SAMPLE_2024.csv")

habitats

BEF_data_unfiltered =left_join(fish, sample, by = "YSAMP_N") %>% 
  #left_join(sites, by = "SITE_N") %>% 
  #left_join(shoreline_length, by = "SITE_N") %>%
  separate(SITE_N,  into = c("GEAR", "WATER","SITE")) %>%
  filter(WATER == "LML" & GEAR == "BEF"& GEAR_CODE == "NAF" &  MONTH %in% c(9,10)) %>%
  mutate(SPECIES = str_replace(SPECIES, "cs", "CS")) %>%
  mutate(SPECIES = str_replace(SPECIES, "MAM", "MM")) %>%
  mutate(EFFORT = round(as.integer(parse_number(EFFORT)))) 

BEF_data = BEF_data_unfiltered %>%  filter(SPECIES %nin% c("GSF", "MAM")) %>%
  mutate(SITE = as.integer(SITE)) %>% 
  left_join(habitats) %>%
  rename(HAB_1 = Habitat)




species_names = c("brown bullhead","blacknose dace", "creek chub", "common shiner","landlocked salmon","lake trout","central mudminnow","northern redbelly dace", "pumpkinseed", "rainbow smelt","rainbow trout", "round whitefish", "smallmouth bass", "slimy sculpin","brook trout", "white sucker")

codes = data.frame((species_codes = unique(BEF_data$SPECIES))) %>% 
  arrange(species_codes)


codes = data.frame(species_names = species_names, 
                   species = codes$X.species_codes)
species = codes$species
# Colorblind pallete
cbbPalette <- c("#000000",  "#56B4E9", "#D55E00","#009E73","#CEC6C6", "#0072B2","#E69F00","#F0E442",  "#CC79A7")


CPUE.w.sec = ((CPUE_wide_seconds(BEF_data) %>%
                 unite("Group", c(YEAR, SITE)) %>% 
                 column_to_rownames(., var = "Group") %>% 
                 mutate(sumrow = rowSums(.)) %>%
                 filter(sumrow>0) %>%
                 dplyr::select(-sumrow)))

#write.csv(CPUE.w.sec, file="CPUE.w.sec.csv")
#dev.off()

CPUE.w.sec.a = ((CPUE_wide_seconds_avg(BEF_data) %>% 
                   column_to_rownames(., var = "YEAR")))

v = CPUE.w.sec %>% 
  mutate(y_s = rownames(CPUE.w.sec)) %>%
  pivot_longer(1:length(codes$species),
               names_to = "Species") %>%
  separate(y_s, 
           into = c("Year", "site"), sep = "_") %>%
  unite("ID", 
        c(site:Species), 
        sep = "_", 
        remove = F) %>%
  
  dplyr::select(-site) %>%
  mutate(value = value * 60 * 60 )


v %>% filter(Species == "CS") %>%
  ggplot(aes(x = Year, y = value)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

cor(CPUE.w.sec)

data_family = data_family %>% as.data.frame() %>% mutate(SITE = rownames(.))

cor_data = CPUE.w.sec %>% mutate(ID = rownames(.)) %>%
  separate(ID, into = c("YEAR", "SITE")) %>%
  pivot_longer(BB:WS,names_to = "SPECIES", values_to = "CPUE") %>%
  group_by(SITE, SPECIES) %>%
  summarize(average_CPUE = mean(CPUE)) %>%
  pivot_wider(names_from = SPECIES, values_from = average_CPUE) %>%
  left_join(data_family) %>%
  ungroup() %>%
  select(-SITE) %>%
  na.omit()


cd = cor(cor_data)

library(corrplot)
corrplot(cd, insig = "pch")

