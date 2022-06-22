### LML Analysis -- Paper 1

### Libraries --------------
library(lattice)
library(MASS)
library(dplyr)
#install.packages("pscl")
require(pscl) # alternatively can use package ZIM for zero-inflated 
library(lmtest)
library(dplyr)
library(tidyr)
library(tidyverse)
library(vegan)
library(RColorBrewer)
library(ggridges)
library(ecp)

### Data -------------------
# This is just setup at the project working directory. Use option in upper right corner of R to get into project directory. For example, on my computer ,its stored in my family one-drive
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP")



## Functions source -----------
source("AFRP_Master_Code/AFRP_Functions.R")
sample = read.csv("Data/FISH_SAMPLE_edited.csv")




# Boat Electrofishing Data
BEF_data_unfiltered = filter_data(water = "LML", gear = "BEF",
                                  gear_code = "NAF", 
                                  species = species) %>% 
  filter(MONTH %in% c(4,5,6,7,8), YEAR >= 1998)

# Removing rare + stocked taxa ------------ 
## Taxa get removed if they are rare or if they are stocked given 
### that stocking would be an artificial manipulation of populations 
### not relating to bass. SMB are also removed to reduce how they swamp
### community analysis 

`%nin%` = Negate(`%in%`) # sets up a way to exclude if in a string
rare_threashold = 50 ## change this based on preference
rare = BEF_data_unfiltered %>% group_by(SPECIES) %>% 
  summarise(frequency = n()) %>% 
  filter(frequency < rare_threashold)
stocked = c("LLS", "RT") ## Stocked fish in little moose
remove = c(stocked, rare$SPECIES) ## Remove SMB + RWF (targeted 2000s)
BEF_data = BEF_data_unfiltered %>% filter(SPECIES %nin% remove)

# Data setup
CPUE.w.sec = ((CPUE_wide_seconds(BEF_data) %>%
                 unite("Group", c(YEAR, SITE)) %>% 
                 column_to_rownames(., var = "Group") %>% 
                 mutate(sumrow = rowSums(.)) %>%
                 filter(sumrow>0) %>%
                 select(-sumrow)))

CPUE.w.sec.a = ((CPUE_wide_seconds_avg(BEF_data) %>% 
                   column_to_rownames(., var = "YEAR")))
## Working on piecewise regressions -----------------------------------
# CPUE in minutes
species = colnames(CPUE.w.sec)
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
  dplyr::select(-site) %>%
  mutate(value = value * 60 * 60 )

# Colorblind pallete
cbbPalette <- c("#000000",  "#56B4E9", "#D55E00","#009E73", "#F0E442", "#0072B2",  "#CC79A7","#E69F00")
## Tommy said to get rid of the regressions for each breakpoint so I'm not going to go through and just add in the means. 
# Also try jitter

for(i in species){
  
  pdf(file = paste("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/MA2276_Code/Graphics/LMLP1/",i, ".pdf", sep = ""),   # The directory you want to save the file in
      width = 4, # The width of the plot in inches
      height = 4) # The height of the plot in inches
  
  # Set up data frame
  x = v %>% filter(Species == i) %>%
    mutate(value = as.numeric(value)) %>% 
    dplyr::select(-Species) %>%
    #mutate(value = log10(value+1)) %>%
    pivot_wider(values_from = value,
                names_from = ID) %>%
    replace(is.na(.), 0) %>%
    dplyr::select(-Year) %>%
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
  
  # If there are multiple breakpoints plot the means of the chunks
  
  listy = list()
  pred_list = list()
  itter_list = list()
  itter_list[[2]] = c(NA, NA, NA)
  itter_list[[3]] = c(NA, NA, NA)
  itter_list[[4]] = c(NA, NA, NA)
  
  if((2 %in% v_mod$color) == TRUE){
    dat_graph = v_mod %>% filter(Species == i)%>%
      mutate(value = round(value))
    
    mean_cpue = dat_graph %>%
      group_by(color) %>% 
      summarize(mean = mean(value))
    
    fred = left_join(dat_graph, mean_cpue)
    
    h = ggplot() + 
      geom_jitter(aes(x = as.numeric(try$Year), 
                     y = try$value,
                     col = as.character(try$color)),alpha = .2, width = .2)
    
    for(z in 1:length(unique(v_mod$color))){
      
      h = ggplot() + 
        geom_jitter(aes(x = as.numeric(dat_graph$Year), 
                        y = dat_graph$value,
                        col = as.character(dat_graph$color)), alpha = .2, width = .2)
      
      
      try.yr = dat_graph %>% filter(Year %in% 
                                unique((v_mod %>% 
                                          filter(color == z))$Year)) 
      
      
      
      
      
    }
    
    g = h + geom_line(aes(x = as.numeric(fred$Year), y = fred$mean)) + 
      labs(color = paste(i, "Cluster")) + 
      ylab(paste(i, "CPUE / Hour") )+ xlab("Year") + 
      theme(legend.position="none") + 
      #scale_y_continuous(trans = 'log10') + 
      geom_vline(xintercept = 2000, linetype = "dashed") +
      xlim(1997, 2021)
    g
    print(g)
    
  } else {
      dat_graph = v_mod %>% filter(Species == i)%>%
        mutate(value = round(value)) 
      
      dat_graph.yr = dat_graph %>%
        filter(Year > 2000)
      
      
      dat_graph.pred = dat_graph %>%
        filter(Year > 2000)   %>% 
        mutate(Year = as.numeric(Year)) %>%
        mutate(Year = scale(Year)[,1]) 
      
      
      mean_cpue = dat_graph %>%
        group_by(color) %>% 
        summarize(mean = mean(value))
      
      fred = left_join(dat_graph, mean_cpue)
      
      h = ggplot() + 
        geom_jitter(aes(x = as.numeric(dat_graph$Year), 
                        y = dat_graph$value), color = "red", alpha = .2, width = .2)
      
      
      try(M4 <- zeroinfl(value ~ (Year) | (Year),
                         dist = 'negbin',
                         data = dat_graph.pred))
      
      
      Pred<- predict(M4,newdata = dat_graph.pred, type = "response")
      
      g = h + geom_line(aes(x = as.numeric(dat_graph.yr$Year), y = Pred), col = 1) + 
        labs(color = paste(i, "Cluster")) + 
        geom_vline(xintercept = 2000, linetype = "dashed") +
        ylab(paste(i, "CPUE / Hour") )+ xlab("Year") + 
        theme(legend.position="none") + 
        #scale_y_continuous(trans = 'log10') + 
        xlim(1997, 2021)
      print(g)
      g
      
    }
  
    
    
    dev.off()
}
     

cp_data = read.csv("MA2276_Code/Data/LML_CP_data.csv")
species_data = read.csv("MA2276_Code/Data/LML_SPECIES_DATA.csv")  %>%
  arrange(MEAN_LML_LENGTH) %>% 
  rename(species = SPECIES) 

cp_data = left_join(cp_data, species_data) %>% arrange(MEAN_LML_LENGTH)

cp_data %>% ggplot(aes(x = year, y = species, label = direction_shape, color = direction_shape)) + geom_text(size = 6,key_glyph = "rect") +
  xlim(2000, 2020) + 
  #theme(legend.position = 'none') + 
  geom_hline(yintercept = "LT", col = "#00BFC4", lwd = 1.5) + 
  geom_hline(yintercept = "CS", col = "#F8766D", lwd = 1.5) +
  geom_hline(yintercept = "CC", col = "#F8766D", lwd = 1.5) + 
  labs(color = "Direction of Change") + 
  scale_color_manual(labels = c("Negative", "Positive"), values = c("#F8766D", "#00BFC4")) + 
  ylab("Species") + 
  xlab("Year") +
  scale_y_discrete(limits = unique(cp_data$species)) + 
  ggtitle("Arranged by mean size")
  
  
    
   
### old delete if tommy likes above ---------------------
  
  
 






## Old code 

for(i in species){
  
  
  # Set up data frame
  x = v %>% filter(Species == i) %>%
    mutate(value = as.numeric(value)) %>% 
    dplyr::select(-Species) %>%
    mutate(value = log10(value+1)) %>%
    pivot_wider(values_from = value,
                names_from = ID) %>%
    replace(is.na(.), 0) %>%
    dplyr::select(-Year) %>%
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
  
  ## Regressions 
  try = v_mod %>% filter(Species == i)%>%
    mutate(value = round(value))
  h = ggplot() + 
    geom_jitter(aes(x = as.numeric(try$Year), 
                   y = try$value,
                   col = as.character(try$color)),alpha = .2, width = .2)
  ## Adding in loop for regressions
  
  listy = list()
  pred_list = list()
  itter_list = list()
  itter_list[[2]] = c(NA, NA, NA)
  itter_list[[3]] = c(NA, NA, NA)
  itter_list[[4]] = c(NA, NA, NA)
  
  for(z in 1:length(unique(v_mod$color))){
    
    h = ggplot() + 
      geom_jitter(aes(x = as.numeric(try$Year), 
                     y = try$value,
                     col = as.character(try$color)), alpha = .2)
    
    
    try.yr = try %>% filter(Year %in% 
                              unique((v_mod %>% filter(color == z))$Year))
    
    
    
    try.r = try.yr    %>% 
      mutate(Year = as.numeric(Year)) %>%
      mutate(Year = scale(Year)[,1])
    
    
    
    
    
    try(M4 <- zeroinfl(value ~ (Year) | (Year),
                       dist = 'negbin',
                       data = try.r))
    
    
    Pred<- predict(M4,newdata = try.r, type = "response")
    #lines(try.r$Year, Pred, col = 3, lwd =3)
    
    
    itter_list[[z]] = data.frame(Year = try.yr$Year, pred = Pred, cluster = z )
    
    
    
    
    
  }
  
  
  data = rbind(itter_list[[1]], itter_list[[2]], itter_list[[3]], itter_list[[4]])  
  
  p = h + geom_line(aes(x = as.numeric(data$Year), y = data$pred)) + 
    labs(color = paste(i, "Cluster")) + 
    ylab(paste(i, "CPUE / Hour") )+ xlab("Year") + 
    theme(legend.position="none") + 
    scale_y_continuous(trans = 'log10') + 
    xlim(1997, 2021)
  print(p)
  
  
}

