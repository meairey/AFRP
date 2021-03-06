

## Functions -----------------------------------------------------------
overlap = function(data_input, comm, dr){
  ## Remove this later
  data = data_input %>% 
    filter(community == comm)
  
  data = combo %>% filter(community == 1)
  spp=length(unique(data$group))
  print(data)
  
  
  siber.example <- createSiberObject(as.data.frame(data)) 
  
  posterior <- siberMVN(siber.example, parms, priors)
  
  name_list = names(posterior) 
  
  cat = name_list %>% 
    replace(., .%in% c(2.1,4.1), NA) %>%
    as.data.frame(.) %>%
    separate(col = 1,into =c("com","group")) %>%
    mutate(group = as.numeric(group)) %>%
    left_join(., legend) %>%
    select(group, Species)
  
  dog = expand.grid(cat$Species, cat$Species)
  
  name_matrix = expand.grid(name_list, name_list) # this needs to not be touched
  
  name_matrixy = expand.grid(name_list, name_list) %>%
    separate(Var1, into = c("com1", "spp1")) %>%
    separate(Var2, into = c("com1", "spp2")) %>%
    select(spp1, spp2)
  
  names_modified = dog %>%
    as.data.frame() %>%
    unite(col = Name, c(Var1, Var2), sep = " v " )
  
  ma = matrix(NA,nrow = dr,ncol = length(name_matrix[,1]))
  
  for(i in 1:length(name_matrix[,1])){ 
    
    overlap = bayesianOverlap(ellipse1 = as.character(name_matrix[i,1]), 
                              ellipse2 = as.character(name_matrix[i,2]), 
                              posterior, 
                              draws = dr,
                              p.interval = 0.95,
                              n = dr)
    
    bayes.prop <- (overlap[,3] / (overlap[,2] + overlap[,1] - overlap[,3]))
    
    ma[,i] = bayes.prop
    
    
    
  }
  
  olap_mat = ma %>% 
    as.data.frame() %>%
    summarise_all(.funs = c(mean="median")) %>%
    round(., digits = 5)
  
  olap_mat[2,] = names_modified[,1]
  
  rownames(olap_mat) = c(paste("Com",comm),"Spp Pair")
  
  return(as.data.frame(t(olap_mat))) ## This is a reminder to pull 90% CI intervals out of this at a point when i can play with code and dont need to be writing
  
  
}



## Data function ----------------------------

## Removed PD because not sure identification 
library(abind)

data_setup = function(data_input, com_num, remove_species){
  data = data_input %>% 
    filter(community == com_num, group != as.numeric(remove_species))
  data = data[order(data$group),] %>% as.data.frame()
  siber.example <- createSiberObject(data)
  posterior <- siberMVN(siber.example, parms, priors)

  both = list(siber.example, posterior)
  return(both)
}


## Ellipse data function ---------------------
ellip_data = function(numb_species, numb_posts, posterior){
  for(i in 1:numb_species){
    dat = vector()
  for(j in 1:numb_posts){
    ellipse_data =  ellipse::ellipse(x = matrix(c(posterior[[2]][[i]][j,1],
                                                  median(posterior[[2]][[i]][j,2]),
                                                  median(posterior[[2]][[i]][j,3]),
                                                  median(posterior[[2]][[i]][j,4])),
                                                2,2), 
                                     centre = c(median(posterior[[2]][[i]][j,5]),
                                                median(posterior[[2]][[i]][j,6])), level = .95, 
                                     npoints = n.points) %>% as.data.frame() %>%
      summarise_all(list("min"=min, "max"=max)) %>% as.matrix()
    
    dat = rbind(dat, ellipse_data)
  }
  
  ellip[,,i] = dat

  }
  
  return(ellip)
}

## Data setup ---------------------

combined = read.csv("SIA_Data.csv", header=T) %>%
  mutate(Sample.ID = as.numeric(Sample.ID)) %>% 
  left_join(read.csv("ADKwebs_Data.csv"),.)

## Clean Setup for baesyain runs 
# TL has nas so if you na omit it causes issues
exclude =3
X=combined %>% 
  select(Sample.ID, d13C, d15N, Water, Species, Weight, Site) %>%
  group_by(Species, Water) %>%
  filter(n()>=exclude) %>%
  na.omit()

x = data.frame(iso1 = X$d13C,
               iso2 = X$d15N,
               group = as.numeric(as.factor(X$Species)),
               community = as.numeric(as.factor(X$Water))) 

species_legend = data.frame(Species = unique(X$Species),
                            group = unique(x$group)) 


legend  = species_legend[order(species_legend$group),]

lake = data.frame(Water = unique(X$Water), 
                  Community = unique(x$community))

lake = lake[order(lake$Community),]

# Params --- 

x.p = seq(-38,-20,length.out=100); 
y.p = seq(0,9, length.out = 100) 
parms <- list();
parms$n.iter=2*10^4; 
parms$n.burnin=1*10^3; 
parms$n.thin=10
parms$n.chains=2     
priors=list();
priors$R=1*diag(2);
priors$k=2; 
priors$tau.mu=1.0E-3 # Vague priors 
Nsamples=1000
n.posts <- 1000;
p.ell <- 0.90 # How much data to include? Standard ellipses --> p.ell = .9
n.points = 1000

## Colors and legends 
COLORS = c(brewer.pal(12, "Paired"),brewer.pal(8, "Set2"))

species_legend = data.frame(Species = unique(combined$Species),
                            group = unique(as.numeric(as.factor(combined$Species))))


legend  = species_legend[order(species_legend$group),]

legend$color = COLORS[1:18]


### Legend
species_legend = data.frame(Species = unique(X$Species),
                            group = unique(x$group)) 


legend  = species_legend[order(species_legend$group),]
legend$color = COLORS[1:15]


## combined legend 

species_legend = data.frame(Species = unique(combined$Species),
                            group = unique(x$group))


