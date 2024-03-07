si = read.csv("MA2276_Code/Data/SI_MEASUREMENT_AFRP.csv")


si %>% separate(ISO_SAMP_N, into = c("SAMPLE", "WATER","YEAR", "ISO_N")) %>%
  filter(CATEGORY != "FISH", TAXA != "CR") %>%
  unite(ID, CATEGORY, TAXA) %>%
    filter(WATER == "LML", YEAR == 2002) %>% 
  ggplot(aes(x = D13C, y= D15N, col= ID)) + geom_point() + stat_ellipse()



si %>% separate(ISO_SAMP_N, into = c("SAMPLE", "WATER","YEAR", "ISO_N")) %>%
  #filter(CATEGORY != "FISH", TAXA != "CR") %>%
  unite(ID, CATEGORY, TAXA) %>% 
  filter(WATER == "LML", YEAR %nin% c(2005,2006,2009, 1999)) %>%
  filter(YEAR == 2002) %>%
  ggplot(aes(y = ID, x = D13C)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(~YEAR)


si %>% separate(ISO_SAMP_N, into = c("SAMPLE", "WATER","SITE","YEAR", "ISO_N")) %>%
  #filter(CATEGORY != "FISH", TAXA != "CR") %>%
  #unite(ID, CATEGORY, TAXA) %>%
  
  filter(WATER == "HAL", YEAR == 2014) %>% 
  ggplot(aes(y = TAXA, x = D13C)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(~YEAR)
