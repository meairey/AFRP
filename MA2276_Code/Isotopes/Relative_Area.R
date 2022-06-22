## Library Load -------------------------
library(easypackages)
#install.packages("tibble")
libraries("snow","plotrix", "SIBER","ggplot2", "tidyr","ellipse","mixtools",
          "mvtnorm","plot3D","scatterplot3d","scales","viridis","ggplot2",
          "gridExtra", "dplyr","RColorBrewer", "tibble")
## Always set the working directory to be the project directory in R
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP")
source("MA2276_Code/Isotopes/isotope_functions.R")

## Run Siber ------------------- 


## Total Area different----------------------------
## dat[[1]] = siber example
## dat[[2]] = posterior
## dat[[3]] = data frame that gets subsetted
## dat[[4]] = combo that is subsetted
relative_list = list()
median_areas = rep(0, 8)
c_list = list()
n_list = list()
for(h in 1:8){
  
  dat = data_setup(data,h)
  
  ellipse.area = siberEllipses(dat[[2]])
  TA = rowSums(ellipse.area)
  ellipse.area = ellipse.area/TA

  names = unique(legend$Species[dat[[3]]$group])
  name_common = unique(legend$common[dat[[3]]$group])
  
  colnames(ellipse.area) = names
  vec = vector()
  for(i in 1:length(ellipse.area[1,])){
    cat= ellipse.area[ellipse.area[,i] >  quantile(x = ellipse.area[,i], 
                                                   probs = .05) &
                        ellipse.area[,i] < quantile(x = ellipse.area[,i], 
                                                    probs = .95),i]
    vec = cbind(vec, cat)
  }
  colnames(vec) = names
  
  median_area = vec %>% as.data.frame() %>%
    summarise_all(list(median))
  
  p = vec %>%
    as.data.frame() %>%
    pivot_longer(1:length(names(dat[[2]])),
                 names_to = "SP",
                 values_to = "Dat") 
  g = ggplot() + 
    geom_point(p, mapping = aes(y = SP , x = Dat, col = SP)) + 
    xlim(0,1) + 
    ggtitle(lake$Water[h]) +
    ylab("Species") + 
    xlab("Relative Niche Area") + 
    theme(text = element_text(size = 18)) +
    geom_point(mapping =aes(x = as.numeric(median_area[1,]), 
                            y = unique(p$SP)),
               col = unique(legend$color[sort(unique(dat[[3]]$group))]), pch = "|", size = 5)+ 
    scale_color_manual(values = legend$color[sort(unique(dat[[3]]$group))], 
                       
                       name = "Species")
  colnames(median_area) = paste(colnames(median_area),lake$Water[h], sep=" ")
  relative_list[[h]] = (median_area[1,])
  median_areas[h] = mean(as.numeric(median_area[1,]))

  print(g)
}

## Relative_area versus size ------------------

relative = unlist(relative_list)
dog = data.frame(med_area = relative) %>%
  rownames_to_column(., var = "ID") %>%
  separate(ID, into = c("Species", "Water"),sep = " ")

cat = lw_data %>% select(Water, Species, TL) %>%
  na.omit(.) %>%
  group_by(Water, Species) %>%
  summarise(m = mean(as.numeric(TL)))
            
full_join(cat, dog) %>%
  filter(Species == "NRD") %>%
  ggplot(aes(x = med_area, y = m)) + geom_point()

for(i in 1:8){
  k = lw_data %>% 
    filter(Water == lake$Water[[i]]) %>%
    select(d13C, Species, TL) %>%
    na.omit() %>%
    group_by(Species) %>%
    summarize(count = n()) %>% 
    filter(count > 3)
  
  j = lw_data %>% 
    filter(Water == lake$Water[[i]]) %>%
    filter(Species %in% k$Species) %>%
    filter(Species == "NRD")
    
    
    
  z = j %>% ggplot(aes(x = d13C, y = as.numeric(TL), col = Species)) + 
    geom_point() + 
    geom_smooth(method = "lm", se=F) + 
    ylab("Total Length (mm)") + 
    ggtitle(lake$Water[i]) + 
    scale_color_manual(values = legend$color[which(legend$Species %in% j$Species)],      
                       name = "Species")
  print(z)
}

lw_data %>% 
  filter(Water != "TPP", 
         Water != "LML") %>%
  group_by(Water) %>%
  mutate(d13C = scale(d13C)) %>%
  mutate(d15N = scale(d15N)) %>%
  filter(Species == "CC") %>%
  ggplot(aes(x = d13C, y = as.numeric(TL), col = Water)) + 
  geom_point() + 
  geom_smooth(method = "lm", se=F) + 
  ylab("Total Length (mm)")  + 
  xlab("Scaled d13C") + 
  ggtitle("Creek Chub")

lw_data %>% 
  filter(Water != "HRM_UB", 
         Water != "HRM_BAR", 
         Water != "HRM_SHI") %>%
  group_by(Water) %>%
  mutate(d13C = scale(d13C)) %>%
  mutate(d15N = scale(d15N)) %>%
  filter(Species == "NRD") %>%
  ggplot(aes(x = d15N, y = as.numeric(TL), col = Water)) + 
  geom_point() + 
  geom_smooth(method = "lm", se=F) + 
  ylab("Total Length (mm)")  + 
  xlab("Scaled d15N") +
  ggtitle("N. Redbelly Dace") +  
  xlim(-1.7,1.7)

lw_data %>% 
  filter(Water != "HRM_FOR", 
         Water != "", 
         Water != "HRM_SHI") %>%
  group_by(Water) %>%
  mutate(d15N = scale(d15N)) %>%
  mutate(d13C = scale(d13C)) %>%
  filter(Species == "GS") %>%
  ggplot(aes(x = d15N, y = as.numeric(TL), col = Water)) + 
  geom_point() + 
  geom_smooth(method = "lm", se=F) + 
  ylab("Total Length (mm)")  + 
  xlab("Scaled d15N") + 
  ggtitle("Golden Shiner") + 
  xlim(-2,1.5)


## Non parametric lmms 
#install.packages("mblm")
library(mblm)

cat = lw_data %>% 
  filter(Water == "LOP", Species =="PS") %>% 
  select(d13C, TL) %>%
  mutate(TL = as.numeric(TL)) %>%
  na.omit()
#summary(lm(cat$TL~as.numeric(cat$d15N)))


f = mblm(TL~d13C, data = cat)
summary(f)






adfadf f fasdf
lw_data %>% 
  filter(Water == "PRL", Species != "LLS") %>% 
  ggplot(aes(x = d13C, y = as.numeric(TL), col = Species)) +
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  ylab("Total Length (mm)") + 
  scale_color_manual(values = legend$color[which(legend$Species %in% c("BND","CC","RS","ST"))],      
                     name = "Species", 
                     labels = c("Blacknose Dace",
                                "Creek Chub", 
                                "Rainbow Smelt", 
                                "Brook Trout")) + 
  ggtitle("PRL")


## Proportion of axes - different -------------------

CC_data = list()
NRD_data = list()
for(h in 1:8){
 
  # Define community - can delete 
  dat = data_setup(data,h) ## Set up the data 
  ellipse.area = siberEllipses(dat[[2]]) # Run SIBER to get ellipse area
  TA = rowSums(ellipse.area) ## Get Total Area
  ellipse.area = ellipse.area/TA # Get total area of each ellipse
  names = unique(legend$Species[dat[[3]]$group])
  names_common = unique(legend$common[dat[[3]]$group])
  colnames(ellipse.area) = names
  vec = vector()
  ellip = array(0, dim=c(n.posts, 4, length(names)))
  ellip = ellip_data(length(names), n.posts, dat)
 
  # Define blank vectors to fill
  blank_vec_C = rep(0, n.posts); blank_vec_N = rep(0, n.posts)
  vector = rep(names, each = n.posts)
  spp.c = vector(); spp.n = vector()
  spp.post.n = vector(); spp.post.c = vector()
  
  # Fills in the ellips for each species - dont run multiple times
  for(j in 1:n.posts){
    C_range = max(ellip[j,3,])- min(ellip[j,1,])
    N_range = max(ellip[j,4,]) - min(ellip[j,2,])
    for(i in 1:(length(names))){
      spp.post.c[i] = (max(ellip[j,3,i])- min(ellip[j,1,i]))/C_range
      spp.post.n[i] = (max(ellip[j,4,i])- min(ellip[j,2,i]))/N_range
    }
    spp.c = rbind(spp.c, spp.post.c)
    spp.n = rbind(spp.n, spp.post.n)
  }
  
  # filters ellips for 90% quantile
  for(i in 1:length(spp.c[1,])){
    SPP.C= spp.c[spp.c[,i] >  quantile(x = spp.c[,i] ,probs = .05) &
                   spp.c[,i] < quantile(x = spp.c[,i] ,probs = .95),i]
    SPP.C = c(SPP.C, rep("NA", (1000 - length(SPP.C)))) %>% as.numeric()
    blank_vec_C = cbind(blank_vec_C, SPP.C)
    SPP.N= spp.n[spp.n[,i] >  quantile(x = spp.n[,i] ,probs = .05) &
                   spp.n[,i] < quantile(x = spp.n[,i] ,probs = .95),i]
    SPP.N = c(SPP.N, rep("NA", (1000 - length(SPP.N)))) %>% as.numeric()
    blank_vec_N = cbind(blank_vec_N, SPP.N)
  }
  
  # Combines ellip vectors with the means of each ellipse
  for(i in 1:length(spp.c[1,])){
    blank_vec_C = cbind(blank_vec_C, dat[[2]][[i]][1:n.posts,5])
    blank_vec_N = cbind(blank_vec_N, dat[[2]][[i]][1:n.posts,6])
  }
  
  # Remove first empty column - I'm sure this could be more eloquent
  C_data = blank_vec_C[,-1] 
  N_data = blank_vec_N[,-1]
  
  # Creating ID's for the columns 
  col_ID = data.frame(sp = rep(names, 2)) %>% 
    mutate(type = rep(c("proportion", "mean"), 
                      each = length(names))) %>% 
    unite("ID",c(sp, type))
  
  colnames(N_data)=col_ID$ID
  colnames(C_data)= col_ID$ID
  
  
  ## 13C Filtering out the data for the proportion and mean graphs 
  C_proportion = C_data[,1:length(names)] %>%
    as.data.frame() %>%
    pivot_longer(1:length(names),
                 names_to = "SP_pro",
                 values_to = "Dat_C_pro")
  
  C_mean = C_data[,(length(names)+1):length(C_data[1,])] %>%
    as.data.frame() %>%
    mutate(fish_n = c(1: length(N_data[,1])))%>%
    pivot_longer(1:length(names),
                 names_to = "SP_mean",
                 values_to = "Dat_C_mean") %>%
    mutate(Dat_C_mean = rescale(Dat_C_mean))
  
  
  ## CC graphs for ARF
  
  CC_data[[h]] = as.numeric((C_mean %>% filter(SP_mean == "CC_mean") %>%
    select(Dat_C_mean))$Dat_C_mean)
  NRD_data[[h]] = as.numeric((C_mean %>% filter(SP_mean == "NRD_mean") %>%
                               select(Dat_C_mean))$Dat_C_mean)
  
  frame_C = cbind(C_mean, C_proportion) %>%
    as.data.frame() %>%
    separate(SP_mean, into = c("Species","Type")) %>%
    select(-Type, - SP_pro, Species)
  
  ## 15N Filtering out the data for the proportion and mean graphs 
  N_proportion = N_data[,1:length(names)] %>%
    as.data.frame() %>%
    pivot_longer(1:length(names),
                 names_to = "SP_pro",
                 values_to = "Dat_N_pro")
  
  N_mean = N_data[,(length(names)+1):length(N_data[1,])] %>%
    as.data.frame() %>%
    mutate(fish_n = c(1: length(N_data[,1]))) %>%
    pivot_longer(1:length(names),
                 names_to = "SP_mean",
                 values_to = "Dat_N_mean") %>%
    mutate(Dat_N_mean = rescale(Dat_N_mean))
  
  frame_N = cbind(N_mean, N_proportion) %>%
    as.data.frame() %>%
    separate(SP_mean, into = c("Species","Type")) %>%
    select(-Type, - SP_pro, Species)
  
  
  
  # median of proportions for 13C
  median_C = C_data[, 1:length(names)] %>% as.data.frame()%>%
    summarise_all(list(median), na.rm = T)
  # median of positions for 13C
  median_C_axis = C_mean %>%
    pivot_wider(names_from = SP_mean, values_from = Dat_C_mean) %>%
    select(-fish_n) %>%
    summarise_all(list(median), na.rm = T)
  # median of proportions for 15N
  median_N = N_data[, 1:length(names)] %>% as.data.frame() %>%
    summarise_all(list(median), na.rm = T)
  # median of positions for 15N
  median_N_axis = N_mean %>%
    pivot_wider(names_from = SP_mean, values_from = Dat_N_mean) %>%
    select(-fish_n) %>%
    summarise_all(list(median), na.rm = T)

  
  
  ## Graphs! 
  # Proportion of axis per species 
  p = ggplot() + 
    geom_point(frame_C,mapping=aes(y = Species, x = Dat_C_pro, col = Species))  +
    xlim(0,1) + ggtitle(paste(lake$Water[h])) +
    ylab("Species") + 
    xlab("Proportion of C axis")+
    theme(text = element_text(size = 18)) +
    geom_point(mapping =aes(x = as.numeric(median_C[1,]), 
                            y = names),
               col = unique(legend$color[sort(unique(dat[[3]]$group))]),
               pch = "|", size = 4) + 
    scale_color_manual(values = legend$color[sort(unique(dat[[3]]$group))], 
                       
                       name = "Species")
  #print(p)
  
  
  #colnames(N_data)=names

  g = ggplot() + 
    geom_point(frame_N,mapping=aes(y = Species, x = Dat_N_pro, col = Species))  +
    xlim(0,1) + ggtitle(paste(lake$Water[h])) +
    ylab("Species") + 
    xlab("Proportion of N axis")+
    theme(text = element_text(size = 18)) +
    geom_point(mapping =aes(x = as.numeric(median_N[1,]), 
                            y = names),
               col = unique(legend$color[sort(unique(dat[[3]]$group))]),
               pch = "|", size = 4) + 
    scale_color_manual(values = legend$color[sort(unique(dat[[3]]$group))], 
                       
                       name = "Species")
    #print(g)
   # Proportion vs. mean of d13C
   l = frame_C %>% 
     ggplot(aes(x = Dat_C_mean, y = Dat_C_pro, col = Species)) + 
     geom_point() + 
     ylab("Proportion of 13C axis") + 
     xlab("Mean 13C") 
   #print(l)
   # Proportion vs. mean of d15N
   k = frame_N %>% 
     ggplot(aes(x = Dat_N_mean, y = Dat_N_pro, col = Species)) + 
     geom_point() + 
     ylab("Proportion of 15N axis") + 
     xlab("Mean 15N") 
   #print(k)
   
  
   
   ## rescaled values for relative position  
   ## d15N
   b = ggplot() + 
     geom_point(frame_N,mapping=aes(y = Species, x = Dat_N_mean, col = Species))  +
     xlim(0,1) + ggtitle(paste(lake$Water[h])) +
     ylab("Species") + 
     xlab("Relative d15N position")+
     theme(text = element_text(size = 18)) +
     geom_point(mapping =aes(x = as.numeric(median_N_axis[1,]), 
                             y = names),
                col = unique(legend$color[sort(unique(dat[[3]]$group))]),
                pch = "|", size = 4) + 
     scale_color_manual(values = legend$color[sort(unique(dat[[3]]$group))], 
                        
                        name = "Species")
   print(b)
   # d13C
   frame_C = frame_C %>% left_join(legend)
   bC = ggplot() + 
     geom_point(frame_C,
                mapping=aes(y = common,
                            x = Dat_C_mean,
                            col = Species), 
                alpha = .05, size = 6)+
     xlim(0,1) + ggtitle(paste(lake$Water[h])) +
     ylab("Species") + 
     xlab("Scaled d13C")+
     theme(text = element_text(size = 18)) +
     geom_point(mapping =aes(x = as.numeric(median_C_axis[1,]), 
                             y = unique(frame_C$common)),
                #col = unique(legend$color[sort(unique(dat[[3]]$group))]),
                col = 1,
                pch = "|", size = 8) + 
     scale_color_manual(values = legend$color[sort(unique(dat[[3]]$group))],
                        labels = legend$common[sort(unique(dat[[3]]$group))],
                        
                        name = "Species")+ guides(colour = guide_legend(override.aes = list(alpha = 1))) + theme(legend.position = 'none') + ylab("")
  print(bC)
  
  cat = cbind(frame_C$Dat_C_mean, frame_N$Dat_N_mean) %>% as.data.frame()
  cat = cbind(c(median_C_axis), c(median_N_axis)) %>% as.data.frame()
  colnames(cat) = c("C","N")
  fox =  cat %>% mutate(C = as.numeric(C), N = as.numeric(N)) %>% 
    
    rownames_to_column(var = "ID") %>%
    pivot_longer(C:N, names_to = "i",
                 values_to = "d") %>%
    separate(ID, into = c("Species", "type")) %>%
    mutate(type = lake$Water[h]) %>%
    unite('ID',c(Species, type,i), sep = " ") %>% 
    as.matrix()
  
  frank = fox %>% as.data.frame() %>%
    pivot_wider(names_from = ID, values_from = "d")
  
  
  
  
  c_list[[h]] = frank
  
}

## Testig oout something thats using posterior draws 

ellipse.area %>% as.data.frame() %>% 
  rownames_to_column(var = "fish_n") %>%
  mutate(fish_n = as.numeric(fish_n)) %>%
  pivot_longer(BND:ST, names_to = "Species", values_to = "Area") %>%
  full_join(frame_N) -> dog 
  ggplot(aes(x = Dat_N_mean, y = Area)) +
  geom_point() + 
  geom_smooth(method = "lm")

summary(lm(dog$Area~dog$Dat_N_mean))

r_a = dog
r_i = data.frame(dat= unlist(c_list)) %>% rownames_to_column(var = "ID") %>% 
  separate(ID, into = c("Species", "Water", "Isotope"), sep = " ") %>% 
  pivot_wider(names_from="Isotope", values_from = "dat")

full = full_join(r_a, r_i)




sp_filter = full %>% group_by(Species) %>%
  summarize(count = n()) %>%
  filter(count > 3)

sp_filter = full %>% 
  filter(Water %in% c("HRM_BAR", "HRM_BW2","HRM_FOR", "HRM_UB"))

full %>%
  filter(Water %in% c("LOP", "TPP","PRL")) %>%
  ggplot(aes(x = as.numeric(med_area), y =as.numeric(N))) + 
  geom_point(aes(col = Species), size = 2) + 
  geom_smooth(method = "lm", se = F, col = 1) +
  ylab("d15N") + 
  xlab("Niche Area") + 
  facet_wrap(~Water)+ 
  scale_color_manual(values = legend$color[which(legend$Species %in% sp_filter$Species)], 
                     labels = legend$common[which(legend$Species %in% sp_filter$Species)],
                     name = "Species")


legend$common = c("Brown Bullhead", "Bluntnose Minnow", "Blacknose Dace", "Creek Chub", "Common Shiner", "Fathead Minnow", "Finescale Dace", "Golden Shiner", "Chrosomus Hybrid", "Landlocked Salmon", "N. Redbelly Dace", "N. Pearl Dace", "Pumpkinseed Sunfish", "Rainbow Smelt", "Brook Trout", "White Sucker")

full %>%
  filter(Water %in% c("HRM_BAR", "HRM_BW2","HRM_FOR", "HRM_UB")) %>%
  ggplot(aes(x = as.numeric(med_area), y =as.numeric(N) )) + 
  geom_point(aes(col = Species), size = 2) + 
  geom_smooth(method = "lm", se=F, col = 1) +
  facet_wrap(~Water) + 
  scale_color_manual(values = legend$color[which(legend$Species %in% sp_filter$Species)], 
                     labels = legend$common[which(legend$Species %in% sp_filter$Species)],
                     name = "Species") + 
  ylab("d15N") + xlab("Niche Area")


full %>%
  filter(Water== "TPP") -> test 
summary(lm(as.numeric(test$med_area)~as.numeric(test$N)))
full %>%
  filter(Water %in% c("TPP", "PRL")) -> test 
summary(lm(as.numeric(test$med_area)~as.numeric(test$C)))  

full %>%
  filter(med_area < .7) %>% 
  ggplot(aes(x = as.numeric(med_area), y = as.numeric(C))) + geom_point() + 
  geom_smooth(method = "lm")
  
  
full %>% filter(Species %in% sp_filter$Species)%>%
  ggplot(aes(x = as.numeric(C), 
                    y = as.numeric(med_area), col = Species)) +
  geom_point() + 
  geom_smooth(method = "lm", se=F) + 
  ylab("Relative Area") + 
  xlab("Relative d13C Position")+ 
  scale_color_manual(values = legend$color[which(legend$Species %in% sp_filter$Species)],      
                     name = "Species")


full %>% filter(Species %in% sp_filter$Species)%>%
  ggplot(aes(x = as.numeric(med_area), 
             y = as.numeric(N), col = Species)) +
  geom_point() + 
  geom_smooth(method = "lm", se=F)+ 
  xlab("Relative Area") + 
  ylab("Relative d15N Position")  + 
  scale_color_manual(values = legend$color[which(legend$Species %in% sp_filter$Species)],      
                     name = "Species")


filtered_full = full %>% filter(Species %in% sp_filter$Species, Species == "GS")
summary(lm(as.numeric(filtered_full$C)~as.numeric(filtered_full$med_area)))

plot(filtered_full$C, filtered_full$med_area)
plot(lm(as.numeric(filtered_full$C)~as.numeric(filtered_full$med_area)))

