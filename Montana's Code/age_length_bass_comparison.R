age = read.csv("Montana's Code/FISH_AGE_GROWTH.csv", header = TRUE)
fish = read.csv("Data/FISH_MEASUREMENT.csv", header=TRUE)
samp = read.csv("Data/FISH_SAMPLE.csv", header = TRUE)
colnames(fish)


library(dplyr)
library(ggplot2)
joined = left_join(age,fish, by = "FISH_N") %>% 
  left_join(samp) %>%
  select( YSAMP_N, FISH_N, SPECIES, LENGTH, WEIGHT, FINAL_AGE, WATER, YEAR) %>%
  mutate(ratio = FINAL_AGE/LENGTH)





st %>% group_by(WATER) %>%
  summarise(n())
  filter(SPECIES == "ST", WATER == "HAL")


st %>% 
  filter(FINAL_AGE == 1) %>%
  ggplot(aes(x = YEAR, LENGTH)) + geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("ST Age 1")




smb = joined %>%
  filter(SPECIES == "SMB", WATER == "LML" )

hist(smb$FINAL_AGE)
unique(smb$FINAL_AGE)

smb %>% 
  filter(FINAL_AGE == 4) %>%
  ggplot(aes(x = YEAR, LENGTH)) + geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("SMB Age 4")

smb %>% 
  filter(FINAL_AGE == 2) %>%
  ggplot(aes(x = YEAR, LENGTH)) + geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("SMB Age 2")

smb %>% 
  filter(FINAL_AGE == 1) %>%
  ggplot(aes(x = YEAR, LENGTH)) + geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("SMB Age 1")  

smb %>% 
  filter(FINAL_AGE == 5) %>%
  ggplot(aes(x = YEAR, LENGTH)) + geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("SMB Age 5")
