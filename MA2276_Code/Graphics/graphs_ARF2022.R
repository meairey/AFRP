species_richness = all_data %>% select(WATER, SPECIES) %>%
  unique() %>%
  group_by(WATER) %>%
  count() %>%
  filter(WATER != "COM", 
         WATER != "ETL", 
         WATER != "WFL")

species_richness = combo %>% select(Water, Species) %>%
  unique() %>%
  group_by(Water) %>%
  count() %>%
  filter(Water != "COM", 
         Water != "ETL", 
         Water != "WFL")
species_richness = species_richness[-5,]


bind = data.frame(median_area = median_areas[-5], 
                  overlap = as.numeric(t(overlap_averages[1,]))[-5],
                  richness = species_richness$n) %>%
  mutate(Habitat = c(rep("Marsh",4), rep("Lake",3)))

bind[-c(1:4),] %>% ggplot(aes(y = overlap, x = richness, size = (total_CPUE_lakes$LAKE_CPUE*10))) + geom_point() + 
  geom_smooth(method = "lm", color = "black", show.legend = F, se=F) + 
  labs(size = "CPUE (n/hour)") + 
  ylim(.1,.25) + 
  ylab("Average Pairwise Overlap") + 
  xlab("Species Richness")


bind[c(1:4),] %>% ggplot(aes(y = overlap, x = richness, size = total_CPUE_HRM$LAKE_CPUE)) + geom_point() + 
  geom_smooth(method = "lm", show.legend = F, se=F)+ 
  labs(size = "CPUE (n/hour)") + 
  ylim(.1,.25)

bind %>% ggplot(aes(y = overlap, x = richness, color = Habitat ,size = CPUE_graph$LAKE_CPUE)) +
  geom_point() + 
  geom_smooth(method = "lm", show.legend = F, se=F)+ 
  labs(size = "CPUE (n/hour)") + 
  ylim(.1,.25) + 
  ylab("Average Pairwise Overlap") + 
  xlab("Species Richness")


bind %>% ggplot(aes(y = overlap, x = median_area,size = CPUE_graph$LAKE_CPUE)) +
  geom_point() + 
  geom_smooth(method = "lm", show.legend = F, se=F)+ 
  labs(size = "CPUE (n/hour)") + 
  ylim(.1,.25) + 
  ylab("Average Pairwise Overlap") + 
  xlab("Average Niche Area")


CC_lengths = length_graph %>% 
  filter(SPECIES == "CC") %>%
  mutate(Water = WATER)
CC_lengths = (left_join(lake, CC_lengths))[-8,]


NRD_lengths = length_graph %>% 
  filter(SPECIES == "NRD") %>%
  mutate(Water = WATER)
NRD_lengths = (left_join(lake, NRD_lengths))[-8,]


CC_mat = matrix(unlist(CC_data), nrow = 1000, ncol = 6)
colnames(CC_mat) = lake$Water[-c(5,8)]
CC_mat %>%
  as.data.frame() %>%
  pivot_longer(1:6,names_to = "Waters") %>%
  ggplot(aes(x = value, y = Waters, color =(rep(na.omit(CC_lengths$mean_length[-8]), 1000)))) + geom_point() +
  labs(color = "Length")


NRD_mat = matrix(unlist(NRD_data), nrow = 1000, ncol = 4)
colnames(NRD_mat) = lake$Water[-c(1,4,5,7)]   
NRD_mat %>%
  as.data.frame() %>%
  pivot_longer(1:4,names_to = "Waters") %>%
  ggplot(aes(x = value, y = Waters,color =(rep(na.omit(NRD_lengths$mean_length[-8]), 1000)))) + geom_point()
