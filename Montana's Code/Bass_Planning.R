CPUE %>%
  ggplot(aes(x = YEAR, y = (CPUE_seconds))) +
  geom_point() +
  geom_smooth(method = "lm") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  
  facet_wrap(~SITE)

## Sites cherry picked
CPUE.lit %>%
  filter(SPECIES %in% c("SMB","CC","CS"), 
         CPUE_std > 0,
         SITE %in% c("004", "005","006","007","008","018","019","020","021","023","024","025","026","028","032", "010", "014", "015")) %>%
  ggplot(aes(x = SPECIES, y = CPUE_std, col = HAB_1)) + geom_point() +
  facet_wrap(~SITE)

## All sites 
CPUE.lit %>%
  filter(SPECIES %in% c("SMB","CC","CS"), 
         CPUE_std > 0) %>%
  ggplot(aes(x = SPECIES, y = CPUE_std, col = HAB_1)) + geom_point() +
  facet_wrap(~SITE)


CPUE %>%
  filter(SPECIES %in% c("SMB","CC","CS"), 
         CPUE_seconds > 0,
         SITE %in% c( "001", "009", "03", "006", "026", "017", "016", "013","011", "029", "007","008","022","030")) %>%
  ggplot(aes(x = SPECIES, y = CPUE_seconds)) + geom_point() +
  facet_wrap(~SITE)



CPUE %>%
  filter( SPECIES %in% c("SMB","CC","CS", "MM"), 
         CPUE_seconds > 0,
         SITE == ("021")) %>%
  ggplot(aes(x = SPECIES, y = CPUE_seconds)) + geom_point() +
  facet_wrap(~YEAR)
