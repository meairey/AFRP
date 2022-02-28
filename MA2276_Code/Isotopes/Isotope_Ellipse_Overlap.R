library(dplyr)
library(SIBER)
source("MA2276_Code/Isotopes/isotope_functions.R")

new_list = list()

for(i in 1:8){
  new_list[[i]] = overlap(data,i,20)
}


overlap_data = full_join(as.data.frame(new_list[[1]]),
                    as.data.frame(new_list[[2]])) %>%
  full_join(as.data.frame(new_list[[3]]),.) %>%
  full_join(as.data.frame(new_list[[4]]),.) %>%
  full_join(as.data.frame(new_list[[5]]),.) %>%
  full_join(as.data.frame(new_list[[6]]),.) %>%
  full_join(as.data.frame(new_list[[7]]),.) %>%
  full_join(as.data.frame(new_list[[8]]),.) %>%
  distinct(`Com 1`,`Com 2`, `Com 3`, `Com 4`, `Com 5`,
           `Com 6`, `Com 7`, `Com 8`, .keep_all = T) %>%
  select(`Spp Pair`,`Com 1`,`Com 2`,`Com 3`,`Com 4`,
         `Com 5`,`Com 6`,`Com 7`,`Com 8`)
  
colnames(overlap_data) = c("Species",lake$Water)

overlap_averages = overlap_data %>% select(-Species) %>% 
  mutate(across(everything(), ~as.numeric(.))) %>%
  mutate(across(everything(), function(x) replace(x,x==1,NA))) %>%
  summarise(across(everything(),list(mean = ~ mean(., na.rm = TRUE))))
  




#### This is partial code from the Webs_Isotopes.RMD you should go look over there to get the rest of it. Sorry. 

for(h in 2:2){
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
  p = vec %>%
    as.data.frame() %>%
    pivot_longer(1:length(names(dat[[2]])),names_to = "SP",values_to = "Dat") %>%
    ggplot(aes(y = SP , x = Dat, col = SP)) + geom_point() + xlim(0,.7) + ggtitle(lake$Water[h]) +
    ylab("Species") + xlab("Relative Niche Area") + theme(text = element_text(size = 18))
  print(p)
}


### Trying something else with combo as the data frame ----------------------
for(h in 2:2){
  
  h= 2
  
  combo = combined %>% 
    mutate(group = as.factor(as.numeric(as.factor(Species)))) %>%
    mutate(community = as.factor(as.numeric(as.factor(Water)))) %>%
    filter(Species != "PDD", Species != "PD") %>%
    mutate(iso1 = d13C) %>%
    mutate(iso2 = d15N) %>% 
    select(iso1, iso2, group, community) 
  
  
  data = combo %>% 
    filter(community == h)
  data = data[order(data$group),] %>% as.data.frame()
  siber.example <- createSiberObject(data)
  posterior <- siberMVN(siber.example, parms, priors)
  
  dat = list(siber.example, posterior)
   
### --------------------------
  
for(h in lake$Water[2:4]){
 
  combo = combined %>% 
    mutate(group = as.factor(as.numeric(as.factor(Species)))) %>%
    filter(Water == h, Species != "PDD", Species != "PD", Species != "NA") %>% select(Species,
                                                                     group,
                                                                     Site,
                                                                     Water,
                                                                     d15N, 
                                                                     d13C)
  data = combo  %>%
    data.frame(iso1 = .$d13C,
               iso2 = .$d15N,
               group = as.numeric(as.factor(.$Species)),
               community = as.numeric(as.factor(.$Water)))   %>% 
    
    select(iso1, iso2, group, community) %>%
    group_by(group, community) %>%
    filter(n()>=2) %>%
    na.omit() 
  
  data = data[order(data$group),] %>% as.data.frame()
  
  
  spp=length(unique(data$group))
  siber.example <- createSiberObject(data) 
  posterior <- siberMVN(siber.example, parms, priors)
  
  
  
  ellipse.area = siberEllipses(posterior)
  TA = rowSums(ellipse.area)
  ellipse.area = ellipse.area/TA
  spec = names(posterior) %>%
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
  p = vec %>%
    as.data.frame() %>%
    pivot_longer(1:length(names(posterior)),names_to = "SP",values_to = "Dat") %>%
    ggplot(aes(y = SP , x = Dat)) + geom_point(aes(colour = SP)) + xlim(0,.7) + ggtitle(h) +
    ylab("Species") + xlab("Relative Niche Area") + theme(text = element_text(size = 19)) +
    scale_color_manual(values = legend$color[sort(unique(combo$group))][-2], name ="Species")
  

  print(p)
}
  ## Who the fuck knows what these colors are doing
  
}
