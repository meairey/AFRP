set.seed(250)
#install.packages("ecp")

library(ecp)


vec = vector()
p.val = vector()
species = colnames(CPUE.w.sec)
change_points_list = list()

for(i in 1:length(species)){
  x = CPUE.w.sec %>% select(species[i]) %>% as.matrix()
  output = e.divisive(x, R= 499, alpha = 1)
  
  dat = data.frame(Year = years_bef$YEAR, CPUE = x[,1], color = output$cluster)
  vec[i] = years_bef$YEAR[which(output$cluster > 1)][1]
  p.val[i] = output$p.values[1]
  pl = ggplot(dat,aes(x = Year,y = CPUE, col = as.character(color))) + 
    geom_point() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
          axis.text = element_text(size = 9)) + 
    labs(col = paste(species[i], "Cluster")) + 
    geom_vline(xintercept = vec[i]) 
    #ylim(0,.4) 
  print(pl)
  change_points_list[[i]] = data.frame(cluster = output$cluster,
                                       ID = rownames(CPUE.w.sec),
                                       species = paste(species[i]))
}




cp_dat = change_points_list %>% unlist %>% matrix(.,ncol = 3) %>%
  as.data.frame() %>%
  separate(V2, into = c("year","site")) %>%

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
  
  
# Length based change point analysis 
  
1 
for(i in 1:length(species)){
  x = BEF_data %>% filter(SPECIES == species[i]) %>% 
    select(LENGTH, YEAR, SITE) %>% na.omit() %>% 
    group_by(YEAR, SITE) %>%
    summarize(mean_L = mean(LENGTH))
  y = x$YEAR
  
  x = x %>% select(mean_L)
  
  output = e.divisive(x, R= 499, alpha = 1)
  
  h = x %>% ggplot(aes(x = YEAR, y = mean_L, 
                       col = as.character(output$cluster))) +
    geom_point() +
    labs(col = paste(species[i])) + 
    ylab("Mean Length (mm) per site") + 
    geom_vline(xintercept = 2010)
  
  print(h)
  
}

  
  
x %>% ggplot(aes(x = YEAR, y = mean_L, 
                 col = as.character(output$cluster))) +
  geom_point() +
  labs(col = paste(species[i])) + 
  ylab("Mean Length (mm) per site")
