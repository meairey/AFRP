set.seed(250)
#install.packages("ecp")

library(ecp)


vec = vector()
p.val = vector()
species = colnames(CPUE.w.sec)

for(i in 1:length(species)){
  x = CPUE.w.sec %>% select(species[i]) %>% as.matrix()
  output = e.divisive(x, R= 499, alpha = 1)
  
  dat = data.frame(Year = years_bef$YEAR, CPUE = x[,1], color = output$cluster)
  vec[i] = years_bef$YEAR[which(output$cluster > 1)][1]
  p.val[i] = output$p.values[1]
  pl = ggplot(dat,aes(x = Year,y = CPUE, col = as.character(color))) + 
    geom_point() + 
    ggtitle(paste("Changepoint Analysis - ", species[i])) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    labs(col = "Cluster") + 
    geom_vline(xintercept = vec[i])
  print(pl)

}

checkpoint.data =data.frame(species = species, changepoint = vec, p.val = p.val)

x = CPUE.w.sec %>% as.matrix()
x.long = x %>% as.data.frame %>% mutate(year = years_bef$YEAR) %>%
  pivot_longer(-year, names_to = "size class", values_to = "CPUE") %>%
  mutate(year = as.numeric(year))
output = e.divisive(x, R = 499, alpha = 1)
dat = data.frame(Year = years_bef$YEAR, CPUE = x.long$CPUE, color = output$cluster)
ggplot(dat, aes(x = Year, y = CPUE, col = as.character(color))) + geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) + 
  labs(color = "Cluster") + 
  geom_vline(xintercept = years_bef$YEAR[which(output$cluster > 1)][1])


x = t(length_frame_site) %>% as.matrix
output = e.divisive(x, R = 499, alpha = 1)
x.names = data.frame(year_site = rownames(x)) %>% separate(year_site, sep = "_", into = c("year","site")) %>% select(year)
x.long = x %>% as.data.frame %>% mutate(year = x.names$year) %>%
  pivot_longer(-year, names_to = "size class", values_to = "CPUE") %>%
  mutate(year = as.numeric(year))
dat = data.frame(Year = x.long$year,CPUE = x.long$CPUE, color = output$cluster)

ggplot(dat, aes(x = as.numeric(Year),
                y = color,
                col = as.character(color))) +
  geom_point() + 
  #theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) + 
  labs(color = "Cluster")  
  geom_vline(xintercept = x.names$year[which(output$cluster > 1)][1])
