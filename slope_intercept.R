# Data
k = BEF_data %>% 
  select(YEAR, SPECIES, LENGTH, YEAR) %>%
  mutate(LENGTH = log10(LENGTH)) %>%
  na.omit()

# Creating bins

bin_size = .05
l.r = seq(from =range(k$LENGTH)[1]-.1, to =  range(k$LENGTH)[2], by = bin_size)
l.r[length(l.r)+1] = range(k$LENGTH)[2] + .1

k = k %>% 
  mutate(LENGTH.bin = .bincode(LENGTH, l.r))  %>%
  group_by(LENGTH.bin, SPECIES, YEAR) %>%
  summarise(abundance = n()) %>%
  mutate(abundance = log10(abundance))

k %>% ggplot(aes(x = LENGTH.bin, y = abundance, col = as.character(YEAR))) +
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~SPECIES)

j = k %>% filter(SPECIES == "SS")
summary(lm(j$abundance~j$LENGTH.bin))

species = matrix(nrow = length(unique(k$YEAR)), ncol = 2)
colnames(species) = c("slope", "intercept")

for(l in unique(k$SPECIES)[2]){
  for(h in 1:22){
  
  j = k %>% filter(SPECIES == l) %>%
      filter(YEAR == unique(k$YEAR)[h])
  f = (lm(j$abundance~j$LENGTH.bin))$coefficients
  
  species[h,1] = f[1]
  species[h,2] = f[2]
  
  }
  species %>% as.data.frame() %>% 
    mutate(year = unique(k$YEAR)) %>%
    ggplot(aes(x = year, y = intercept )) + geom_point() + 
    ggtitle(paste(l))
  species %>% as.data.frame() %>% 
    mutate(year = unique(k$YEAR)) %>%
    ggplot(aes(x = year, y = slope )) + geom_point() + 
    ggtitle(paste(l))
  
}
  
  

  
