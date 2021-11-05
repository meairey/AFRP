library(dplyr)
library(SIBER)
source("isotope_functions.R")

PRL_overlap = overlap(x,3,20)

LOP_overlap = overlap(x,2,20)


TPP_overlap = overlap(x,4,20)


overlap = full_join(as.data.frame(TPP_overlap),
                    as.data.frame(LOP_overlap)) %>%
  full_join(as.data.frame(PRL_overlap),.) %>%
  distinct(`Com 2`,`Com 4`, `Com 3`, .keep_all = T) %>%
  select('Spp Pair','Com 2',`Com 4`,`Com 3`)

### Heron Marsh overlap function 

HRM_dat = X %>% 
  data.frame(iso1 = .$d13C,
             iso2 = .$d15N,
             group = as.numeric(as.factor(.$Species)),
             water = as.numeric(as.factor(.$Water)), 
             community = .$Site) %>%
  filter(water == 1) %>%
  select(iso1, iso2, group, community) %>%
  group_by(group, community)%>%
  filter(n()>=exclude)


  

HRM1_O= overlap(HRM_dat,1,20)
HRM2_O = overlap(HRM_dat,2,20) 
HRM3_O = overlap(HRM_dat,3,20)
HRM4_O = overlap(HRM_dat,4,20)
HRM5_O = overlap(HRM_dat,5,20)



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
