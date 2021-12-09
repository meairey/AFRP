## Library Load -------------------------
library(easypackages)  
libraries("snow","plotrix", "SIBER","ggplot2", "tidyr","ellipse","mixtools",
          "mvtnorm","plot3D","scatterplot3d","scales","viridis","ggplot2",
          "gridExtra", "dplyr","RColorBrewer")
## Always set the working directory to be the project directory in R
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP")
source("MA2276_Code/Isotopes/isotope_functions.R")

## Run Siber ------------------- 
# Do I need this?
#siber.example <- createSiberObject(x) 
#posterior <- siberMVN(siber.example, parms, priors)
#mu.post <- extractPosteriorMeans(siber.example, posterior)
#layman.B <- bayesianLayman(mu.post)




### Proportion of axes HRM ---------------
for(h in 2:4){
  
  combo = combined %>% 
    mutate(group = as.factor(as.numeric(as.factor(Species)))) %>%
    filter(Water == "HRM", Site == h) %>% select(Species,
                                                 group,
                                                 Site, 
                                                 d15N, 
                                                 d13C)
  data = combo  %>%
    data.frame(iso1 = .$d13C,
               iso2 = .$d15N,
               group = (.$Species),
               community = as.numeric(as.factor(.$Site)))   %>% 
    
    select(iso1, iso2, group, community) %>%
    group_by(group, community) %>%
    filter(n()>=exclude) %>%
    na.omit() 
  
  data = data[order(data$group),] %>% as.data.frame()
  
  
  d = data_setup(x,h)
  spp=length(names(d[[2]]))
  
  
  
  spec = names(d[[2]]) %>%
    as.data.frame() %>%
    separate(1, into = c("com","sp"))
  names = legend$Species[as.numeric(spec$sp)]
  
  
  ellip = array(0, dim=c(n.posts, 4, spp))
  blank_vec_C = vector()
  blank_vec_N = vector()
  
  
  
  ellip = ellip_data(spp, n.posts, d)
  
  vector = rep(names, each = n.posts)
  
  
  spp.c = vector()
  spp.n = vector()
  spp.post.n = vector()
  spp.post.c = vector()
  
  for(j in 1:n.posts){
    
    
    C_range = max(ellip[j,3,])- min(ellip[j,1,])
    N_range = max(ellip[j,4,]) - min(ellip[j,2,])
    
    
    
    for(i in 1:spp){
      
      spp.post.c[i] = (max(ellip[j,3,i])- min(ellip[j,1,i]))/C_range
      
      spp.post.n[i] = (max(ellip[j,4,i])- min(ellip[j,2,i]))/N_range
      
    }
    spp.c = rbind(spp.c, spp.post.c)
    spp.n = rbind(spp.n, spp.post.n)
  }
  
  
  for(i in 1:length(spp.c[1,])){
    SPP.C= spp.c[spp.c[,i] >  quantile(x = spp.c[,i] ,probs = .05) &
                   spp.c[,i] < quantile(x = spp.c[,i] ,probs = .95),i]
    blank_vec_C = cbind(blank_vec_C, SPP.C)
    
    SPP.N= spp.n[spp.n[,i] >  quantile(x = spp.n[,i] ,probs = .05) &
                   spp.n[,i] < quantile(x = spp.n[,i] ,probs = .95),i]
    blank_vec_N = cbind(blank_vec_N, SPP.N)
  }
  
  colnames(blank_vec_C)=legend$Species[as.numeric(spec$sp)]
  median_C = blank_vec_C %>% as.data.frame() %>%
    summarise_all(list(median))
  colnames(blank_vec_N)=legend$Species[as.numeric(spec$sp)]
  median_N = blank_vec_N %>% as.data.frame() %>%
    summarise_all(list(median))
  
  frame = blank_vec_C %>% as.data.frame() %>%
    pivot_longer(1:length(blank_vec_C[1,]),
                 names_to = "SP",
                 values_to = "Dat") 
  p = ggplot() + 
    geom_point(frame, mapping=aes(y = SP, x = Dat)) +
    xlim(0,1) + ggtitle(paste(lake$Water[h])) +
    ylab("Species") + 
    xlab("Proportion of C axis")+
    theme(text = element_text(size = 18)) +
    geom_point(mapping =aes(x = as.numeric(median_C[1,]), 
                            y = legend$Species[as.numeric(spec$sp)]),
               col = "red", pch = 2, size = 4)
  
  
  
  colnames(blank_vec_N)=legend$Species[as.numeric(spec$sp)]
  frame = blank_vec_N %>% as.data.frame() %>%
    pivot_longer(1:length(blank_vec_C[1,]),
                 names_to = "SP",
                 values_to = "Dat")   
  g = ggplot() + 
    geom_point(frame, mapping=aes(y = SP, x = Dat)) + 
    geom_point()+ geom_point() +
    xlim(0,1) + ggtitle(paste(lake$Water[h])) +
    ylab("Species") + 
    xlab("Proportion of N axis")+
    theme(text = element_text(size = 18))+
    geom_point(mapping =aes(x = as.numeric(median_N[1,]), 
                            y = legend$Species[as.numeric(spec$sp)]),
               col = "red", pch = 2, size = 4)
  
  print(p)
  print(g)
  
}



## Total Area different----------------------------
## dat[[1]] = siber example
## dat[[2]] = posterior
## dat[[3]] = data frame that gets subsetted
## dat[[4]] = combo that is subsetted
for(h in 1:8){
  
  dat = data_setup(data,h)
  
  ellipse.area = siberEllipses(dat[[2]])
  TA = rowSums(ellipse.area)
  ellipse.area = ellipse.area/TA

  names = unique(legend$Species[dat[[3]]$group])
  
  colnames(ellipse.area) = names
  vec = vector()
  for(i in 1:length(ellipse.area[1,])){
    cat= ellipse.area[ellipse.area[,i] >  quantile(x = ellipse.area[,i] ,probs = .05) &
                        ellipse.area[,i] < quantile(x = ellipse.area[,i] ,probs = .95),i]
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
    geom_point(p, mapping = aes(y = SP , x = Dat, col = SP)) + xlim(0,.7) + 
    ggtitle(lake$Name[h]) +
    ylab("Species") + 
    xlab("Relative Niche Area") + 
    theme(text = element_text(size = 18)) +
    geom_point(mapping =aes(x = as.numeric(median_area[1,]), 
                            y = unique(p$SP)),
               col = unique(legend$color[sort(unique(dat[[3]]$group))]), pch = "|", size = 5)+ 
    scale_color_manual(values = legend$color[sort(unique(dat[[3]]$group))], 
                       
                       name = "Species")
  
  print(g)
}


## Proportion of axes - different -------------------

for(h in 1:8){
  
  # Define community - can delete 
  dat = data_setup(data,h) ## Set up the data 
  ellipse.area = siberEllipses(dat[[2]]) # Run SIBER to get ellipse area
  TA = rowSums(ellipse.area) ## Get Total Area
  ellipse.area = ellipse.area/TA # Get total area of each ellipse
  names = unique(legend$Species[dat[[3]]$group])
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
   b = ggplot() + 
     geom_point(frame_C,
                mapping=aes(y = Species,
                            x = Dat_C_mean,
                            col = Species))+
     xlim(0,1) + ggtitle(paste(lake$Water[h])) +
     ylab("Species") + 
     xlab("Relative d13C position")+
     theme(text = element_text(size = 18)) +
     geom_point(mapping =aes(x = as.numeric(median_C_axis[1,]), 
                             y = names),
                col = unique(legend$color[sort(unique(dat[[3]]$group))]),
                pch = "|", size = 4) + 
     scale_color_manual(values = legend$color[sort(unique(dat[[3]]$group))], 
                        
                        name = "Species")
  print(b)
}

