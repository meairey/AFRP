source("isotope_functions.R")
combined %>% 
  filter(Species %in% c("CC","NRD", "PS", "ST","GS","BND")) %>% 
  filter(Water != "LML" & Water != "HRM") %>%
  ggplot(aes(x = as.numeric(TL), y = d15N, col = Species)) +
  geom_point() + scale_x_continuous(trans='log2') +
  ylab("d15N") + xlab("TL") +
  geom_smooth(method= "lm", se = F) +
  ylim(5,10) + xlim(16,256) +
  facet_wrap(~Water)

combined %>% 
  filter(Species %in% c("CC","NRD", "PS", "ST","GS","BND")) %>% 
  filter(Water != "LML" & Water != "HRM") %>%
  ggplot(aes(y = as.numeric(TL), x = d13C, col = Species)) +
  geom_point() + scale_y_continuous(trans='log2') +
  ylab("TL") + xlab("d13C") +
  ylim(32, 256) +
  geom_smooth(method= "lm", se = F) +
  facet_wrap(~Water)
