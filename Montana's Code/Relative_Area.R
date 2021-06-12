library(easypackages)  
libraries("snow","plotrix", "SIBER","ggplot2", "tidyr","ellipse","mixtools",
          "mvtnorm","plot3D","scatterplot3d","scales","viridis","ggplot2",
          "gridExtra", "dplyr","RColorBrewer")
source("isotope_functions.R")

siber.example <- createSiberObject(x) 
posterior <- siberMVN(siber.example, parms, priors)
mu.post <- extractPosteriorMeans(siber.example, posterior)
layman.B <- bayesianLayman(mu.post)

## Proportion of axes



for(h in 3){
  d = data_setup(x,h,11)
  names = legend$color[sort(unique(x$group))]
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
  frame_C = blank_vec_C %>% as.data.frame() %>%
    pivot_longer(1:length(blank_vec_C[1,]),
                 names_to = "SP",
                 values_to = "Dat_C") 
  p = ggplot() + 
    geom_point(frame_C, mapping=aes(y = SP, x = Dat)) +
    xlim(0,1) + ggtitle(paste(lake$Water[h])) +
    ylab("Species") + 
    xlab("Proportion of C axis")+
    theme(text = element_text(size = 18)) +
    geom_point(mapping =aes(x = as.numeric(median_C[1,]), 
                            y = legend$Species[as.numeric(spec$sp)]),
               col = "red", pch = 2, size = 4)
  colnames(blank_vec_N)=legend$Species[as.numeric(spec$sp)]
  frame_N = blank_vec_N %>% as.data.frame() %>%
    pivot_longer(1:length(blank_vec_C[1,]),
                 names_to = "SP_N",
                 values_to = "Dat_N")   
  g = ggplot() + 
    geom_point(frame_N, mapping=aes(y = SP, x = Dat)) + 
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


# Graph comparing proportions of each axis as plots 
cbind(frame_C, frame_N) %>%
  as.data.frame() %>%
  select(-SP_N) %>%
  ggplot(aes(x = Dat_C, y = Dat_N, col = SP)) +
  stat_ellipse(level = .9)
# Proportions as single numbers may be more useful for comparing between lakes
cbind(frame_C, frame_N) %>%
  as.data.frame() %>%
  select(-SP_N) %>% 
  mutate(proportion = Dat_N / Dat_C) %>%
  ggplot(aes(x = proportion, y = SP)) + 
  geom_point()


### Proportion of axes HRM
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
  
  
  d = data_setup(x,h,11)
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


## Total Area 
for(h in 2:4){
  dat = data_setup(x,h,11)
  
  ellipse.area = siberEllipses(dat[[2]])
  TA = rowSums(ellipse.area)
  ellipse.area = ellipse.area/TA
  spec = names(dat[[2]]) %>%
    as.data.frame() %>%
    separate(1, into = c("com","sp"))
  names = legend$Species[as.numeric(spec$sp)]
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
    geom_point(p, mapping = aes(y = SP , x = Dat)) + xlim(0,.7) + 
    ggtitle(lake$Water[h]) +
    ylab("Species") + 
    xlab("Relative Niche Area") + 
    theme(text = element_text(size = 18)) +
    geom_point(mapping =aes(x = as.numeric(median_area[1,]), 
                            y = legend$Species[as.numeric(spec$sp)]),
               col = "red", pch = 2, size = 4)
  print(g)
}
