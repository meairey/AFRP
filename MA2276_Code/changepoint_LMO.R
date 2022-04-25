set.seed(250)
#install.packages("ecp")

library(ecp)


vec = vector()
p.val = vector()
species = colnames(CPUE.w.sec)
change_points_list = list()
## Reformatting changepoint -----------------

v = CPUE.w.sec %>% 
  mutate(y_s = rownames(CPUE.w.sec)) %>%
  pivot_longer(1:length(species),
               names_to = "Species") %>%
  separate(y_s, 
           into = c("Year", "site")) %>%
  unite("ID", 
        c(site:Species), 
        sep = "_", 
        remove = F) %>%
  select(-site)

## Fixed change point using multiple sites a year as multivariate --------- 
for(i in species){
  
  # Set up data frame
  x = v %>% filter(Species == i) %>%
    mutate(value = as.numeric(value)) %>% 
    select(-Species) %>%
    mutate(value = log10(value+1)) %>%
    pivot_wider(values_from = value,
                names_from = ID) %>%
    replace(is.na(.), 0) %>%
    select(-Year) %>%
    as.matrix()
  rownames(x) = rownames(CPUE.w.sec.a)
  
  # Run changepoint analysis 
  output = e.divisive(x, 
                      R = 499, 
                      alpha = 1, 
                      min.size = 2,
                      sig.lvl = .05)
  
  # Format data
  dat = data.frame(Year = rownames(CPUE.w.sec.a), 
                   color = output$cluster)
  v_mod = left_join(v,dat)
  
  # Linear model to overlap on plots
  lin.reg = summary(lm(v_mod$value~as.numeric(v_mod$Year)))
  p_value = round(lin.reg$coefficients[2,4],4)
  r_2 = round(lin.reg$adj.r.squared,4)
  y_point = max(x) 
  # CB pallete
  cbbPalette <- c("#000000",  "#56B4E9", "#D55E00","#009E73", "#F0E442", "#0072B2",  "#CC79A7","#E69F00")
  # Plots
  pl = v_mod %>%
    filter(Species == i)%>% 
    ggplot(aes(x = as.numeric(Year), 
               y = value)) +
    geom_point(aes(col = as.character(color)), alpha = .2, size = 2) + 
    #geom_text(x= 2010, y= y_point, label="Scatter plot") + 
    geom_smooth(method = "lm") + 
    #xlab("Year") + 
    ylab(paste(i,"CPUE")) +
    xlim(1998, 2021) + 
    theme(text = element_text(size = 16)) + 
    theme(legend.position="none") + 
    theme(axis.title.x = element_blank()) + 
    theme(axis.text.x = element_text(angle = 90)) + 
    scale_colour_manual(values=cbbPalette)
    
  print(pl)
}


# Length based change point analysis ---------- 


for(i in 1:length(species)){
  
  x = BEF_data %>% filter(SPECIES == species[i]) %>% 
    filter(YEAR >= 2000) %>%
    select(LENGTH, YEAR, SITE) %>% na.omit() %>% 
    group_by(YEAR, SITE) %>%
    summarize(median_L = mean(LENGTH,na.rm = T)) %>%
    ungroup() %>%
    pivot_wider(names_from = SITE, values_from = median_L)
  
  output = e.divisive(x, R= 499,
                      alpha = 1,
                      min.size = 2, 
                      sig.lvl  = .05)
  
  output_dat = data_frame(YEAR = unique(x$YEAR), 
                          cluster = output$cluster)
  
  dat = BEF_data %>%
    filter(YEAR >= 2000) %>%
    filter(SPECIES == species[i]) %>% 
    left_join(output_dat)
  
  lm_obj = summary(lm(dat$LENGTH ~ dat$YEAR, 
                      na.rm = T))
  
  h = dat %>%
    filter(SPECIES == species[i]) %>%
    ggplot(aes(x = YEAR, y = LENGTH)) +
    geom_point(aes(col = as.character(cluster),
                   alpha = .06)) +
    labs(col = paste(species[i])) + 
    ylab("Length") +
    geom_smooth(method = "lm") + 
    ylab(paste(species[i], "Length")) + 
    theme(text = element_text(size = 16)) + 
    theme(legend.position="none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90)) + 
    xlim(1999, 2021) + 
    geom_text(x= 2019, 
              y= 50,
              label= round(lm_obj$coefficients[2,4],3)) +
    scale_colour_manual(values=cbbPalette)
  
  print(h)
  
  
  
}


before_after = c(1990, 2000, 2023)
BEF_data %>% 
  mutate(b_a = .bincode(YEAR, before_after)) %>%
  ggplot(aes(x = as.character(b_a), y = LENGTH)) + 
  geom_boxplot() + 
  facet_wrap(~SPECIES) + 
  xlab("Before vs. After")
scale_y_continuous(trans = "log10") 



BEF_data %>% 
  filter(SPECIES == "CC") %>%
  ggplot(aes(x = as.character(YEAR), y = LENGTH)) + 
  geom_boxplot()





## Average CPUE per year --------------------- 
for(i in species){
  i = species[2]
  x = v %>%  group_by(Species, Year)%>% 
    summarise(mean_CPUE = mean(value)) %>%
    filter(Species == i) %>%
    select(-Species) %>%
    replace(is.na(.), 0) %>%
    ungroup() %>%
    select(-c(Year, Species)) %>%
    #mutate(mean_CPUE = scale(mean_CPUE)) %>%
    as.matrix()
  #x = x*10000
  rownames(x) = rownames(CPUE.w.sec.a)
  output = e.divisive(x, 
                      R = 499,
                      alpha = 1,
                      sig.lvl = .05,
                      min.size = 2)
  
  dat = data.frame(Year = rownames(CPUE.w.sec.a), 
                   color = output$cluster)
  
  v_mod = left_join(v,dat)
  pl = v_mod %>%
    filter(Species == i)%>% 
    ggplot(aes(x = as.numeric(Year), 
               y = value)) +
    geom_point(aes(col = as.character(color)), alpha = .2) + 
    labs(col = paste(i, "Cluster")) + 
    geom_smooth(method = "lm") + 
    ylab("Year") + 
    xlab("CPUE")
  
  pl = ggplot(,aes(y = x[,1], x = as.numeric(rownames(x)), col = as.character(output$cluster))) + 
    geom_point()+ 
    ylab(paste(i,"CPUE")) + 
    xlab("Year") + 
    theme(legend.position = "none")
    
  

  print(pl)
}
summary(lm(x[,1] ~ as.numeric(rownames(x))))



## Testing if CC have increased in CPUE 

CC_2000 = CPUE.w.sec$CC[which(years_bef == 2000)]
CC_2019 = CPUE.w.sec$CC[which(years_bef == 2019)]
t.test(CC_2000, CC_2019)           
CC = BEF_data %>% filter(SPECIES == "LT") 
summary(lm(CC$LENGTH~CC$YEAR))
summary(lm(CPUE.w.sec$LT ~ as.numeric(years_bef$YEAR)))


## Loading  in cp data --------------
cp_data = read.csv("MA2276_Code/Data/Changepoint_data_analysis.csv") %>%
  as.data.frame()

rownames(cp_data) = cp_data$Species
res = cor(t(cp_data %>% select(-Species)))
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


BEF_data %>% select(SPECIES, LENGTH) %>%
  group_by(SPECIES) %>%
  summarize(max = max(LENGTH, na.rm = T), 
            avg = mean(LENGTH, na.rm = T))
##

B
  

for(i in 1:length(species)){
  i = 1
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
  


 

