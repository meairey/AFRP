fish = read.csv("Data/FISH_MEASUREMENT_LSP.csv")
sample = read.csv("Data/FISH_SAMPLE_LSP.csv")


LSP = left_join(fish, sample)


LSP %>% filter(GEAR=="TPN") %>%
  filter(SPECIES != "NF") %>%
  group_by(YEAR, MONTH, DAY_N, SPECIES, EFFORT) %>%
  summarize(CPUE = n()) %>%
  mutate(EFFORT = as.numeric(EFFORT)) %>%
  mutate(CPUE = CPUE / EFFORT) %>%
  ggplot(aes(x = YEAR, y = CPUE, col = SPECIES)) + 
  geom_point() + 
  geom_smooth() + 
  ylab("Fish / Night") + 
  facet_wrap(~MONTH, scales = "free_y")


LSP %>% filter(YEAR == 1980)

LSP %>% filter(GEAR=="TPN") %>%
  filter(SPECIES != "NF") %>%
  group_by(YEAR, MONTH, DAY_N, SPECIES, EFFORT, ORIGIN) %>%
  summarize(CPUE = n()) %>%
  mutate(EFFORT = as.numeric(EFFORT)) %>%
  mutate(CPUE_night = CPUE / EFFORT) %>%
  filter(YEAR > 1975)  


LSP %>% filter(GEAR=="TPN") %>%
  filter(MONTH ==10) %>%
  filter(SPECIES != "NF") %>%
  group_by(YEAR, MONTH, DAY_N, SPECIES, EFFORT, ORIGIN) %>%
  summarize(CPUE = n()) %>%
  mutate(EFFORT = as.numeric(EFFORT)) %>%
  mutate(CPUE = CPUE / EFFORT) %>%
  ggplot(aes(x = YEAR, y = CPUE, col = SPECIES)) + 
  geom_point() + 
  geom_smooth() + 
  ylab("Fish / Night") + 
  facet_wrap(~SPECIES+ORIGIN)


         