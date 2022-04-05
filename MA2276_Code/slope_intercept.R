# Data
k = BEF_data %>% 
  select(YEAR, SPECIES, LENGTH, YEAR) %>%
  filter(LENGTH > 90) %>%
  mutate(LENGTH = log10(LENGTH)) %>%
  na.omit()

# Creating bins

bin_size = .1
l.r = seq(from =range(k$LENGTH)[1]-.1, to =  range(k$LENGTH)[2], by = bin_size)
l.r[length(l.r)+1] = range(k$LENGTH)[2] + .1

k = k %>% 
  mutate(LENGTH.bin = .bincode(LENGTH, l.r))  %>%
  group_by(LENGTH.bin, SPECIES, YEAR) %>%
  summarise(abundance = n()) %>%
  mutate(abundance = log2(abundance)) %>%
  ungroup()

k %>% ggplot(aes(x = LENGTH.bin, y = abundance, col = as.character(YEAR))) +
  geom_point() + 
  geom_smooth(method = "lm", se = F) +
  labs(color = "Year") + 
  facet_wrap(~SPECIES)

j = k %>% filter(SPECIES == "SS")
summary(lm(j$abundance~j$LENGTH.bin))

species = matrix(nrow = length(unique(k$YEAR)), ncol = 2)
colnames(species) = c("slope", "intercept")
slope_array = list()
for(l in unique(k$SPECIES)){
  for(h in 1:22){
  
  j = k %>% filter(SPECIES == l) %>%
      filter(YEAR == unique(k$YEAR)[h])
  
  if (dim(j)[1] == 0){
    species[h,1] = NA
    species[h,2] = NA
  }else{
  f = (lm(j$abundance~j$LENGTH.bin))$coefficients
  species[h,1] = f[1]
  species[h,2] = f[2]
  }
  
  
  
  }
  slope_array[[l]] = species[,1]
  m = species %>% as.data.frame() %>% 
    mutate(year = unique(k$YEAR)) %>%
    ggplot(aes(x = year, y = as.numeric(intercept) )) + geom_point() + 
    ggtitle(paste(l))
  n = species %>% as.data.frame() %>% 
    mutate(year = unique(k$YEAR)) %>%
    ggplot(aes(x = year, y = as.numeric(slope))) + geom_point() + 
    ggtitle(paste(l)) + ylab("slope")
  print(n)
}
  
  
slope_array %>% unlist() %>% matrix(., ncol = 1, nrow = 220) %>% as.data.frame() %>%
  mutate(SPECIES = rep(unique(k$SPECIES), each = 22)) %>%
  mutate(YEAR = rep(c(1998:2019), 10)) %>%
  rename(slope = V1) %>%
  ggplot(aes(x = YEAR, y = slope, col = SPECIES)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  xlim(1996, 2021) + 
  facet_wrap(~SPECIES)
  
