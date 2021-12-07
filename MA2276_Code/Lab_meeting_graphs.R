CPUE %>%
  filter(SPECIES %in% c("CC", "CS", "LLS", "LT", "MM","PS", "RS", "SMB","SS","ST","WS"), MONTH  <7) %>%
  ggplot(aes(x = YEAR, y = CPUE_seconds, col = HAB_1)) +
  geom_smooth(se=F) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(trans='log2') +
  facet_wrap(~SPECIES)

Spring = CPUE %>%
  filter(MONTH < 7)
Fall = CPUE %>%
  filter(MONTH>7)

cat = Spring %>%
  filter(SPECIES == "MM", 
         n >0, YEAR == 2018) %>%
  select(SITE, n, HAB_1, YEAR) %>%
  group_by(SITE, HAB_1, YEAR) %>%
  summarise(total = sum(n))

dog = Fall %>%
  filter(SPECIES == "MM", 
         n >0,YEAR == 2018) %>%
  select(SITE, n, HAB_1, YEAR) %>%
  group_by(SITE, HAB_1, YEAR) %>%
  summarise(total = sum(n))

multi = Fall %>% 
  filter(SPECIES %in% c("MM", "CC", "CS", "WS"), n>0, YEAR == 2018) %>%
  select(SITE, n, HAB_1, YEAR, SPECIES)%>%
  group_by(SITE, HAB_1, YEAR, SPECIES) %>%
  summarise(total = sum(n))


Spring_CC = CPUE %>%
  filter(YEAR == 2017, SPECIES == "CC", n >0) %>%
  select(SITE, n, HAB_1) %>% 
  group_by(SITE, HAB_1) %>%
  summarise(total = sum(n))



all_data %>%
  filter(MONTH < 7, SPECIES %in% c("CC", "CS", "LLS", "LT", "MM","PS", "RS", "SMB","SS","ST","WS")) %>%
  group_by(YEAR, SPECIES, HAB_1, SEASON) %>%
  mutate(avg.length = mean(LENGTH, na.rm=T)) %>%
  ggplot(aes(YEAR, y = avg.length,color = HAB_1)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  scale_y_continuous(trans='log2') +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1)) + 
  facet_wrap(~SPECIES)

all_data %>%
  filter(MONTH > 7, SPECIES %in% c("CC", "CS", "LLS", "LT", "MM","PS", "RS", "SMB","SS","ST","WS")) %>%
  group_by(YEAR, SPECIES, HAB_1, SEASON) %>%
  ggplot(aes(YEAR, y = LENGTH,color = HAB_1)) + 
  geom_smooth(method = "lm", se = F) + 
  scale_y_continuous(trans='log2') +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1),
        text = element_text(size=20)) + 
  ggtitle("Length vs. time") +
  facet_wrap(~SPECIES)
  



all_data %>%
  filter(SPECIES %in% c("BND")) %>%
  select(SITE, SPECIES, SEASON, MONTH) %>%
  unique()
