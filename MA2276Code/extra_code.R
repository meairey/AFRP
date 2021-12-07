## Extra code from Web_Isotopes.Rmd


## ggplot with Stat Ellipses-------------------------------------------
```{r, message = F, warning = F}
lakes = unique(combined$Water)
for(i in lakes[1:4]){
  g = combined %>%
    filter(Water == i) %>%
    select(Water, Species, d13C, d15N) %>%
    na.omit() %>%
    ggplot(aes(x = d13C, y = d15N, color = Species)) + 
    geom_point() +
    stat_ellipse(level = .9) +
    ggtitle(paste(i))
  print(g)
}
```

## Looped base graphs with baeysian ellipses -----------------------------------------
```{r}
for(h in lake$Community){
  
  data = x %>% 
    filter(community == h)
  
  
  spp=length(unique(data$group))
  
  siber.example <- createSiberObject(data) 
  
  posterior <- siberMVN(siber.example, parms, priors)
  
  plot(1, type="n", xlab= expression({delta}^13*C~'\u2030'), 
       ylab=expression({delta}^15*N~'\u2030'),
       ylim = c(-1,13), xlim = c(-42,-20), main = lake$Water[h],xaxs = "i", 
       yaxs = "i" , frame.plot = F)
  
  legend("topleft", legend[unique((data$group)),1], 
         lty = rep(1, spp), lwd = 2, 
         col = COLORS)
  
  for(i in 1:spp){
    
    ellipse(mu = c(median(posterior[[i]][,5]),
                   median(posterior[[i]][,6])),
            sigma = matrix(c(median(posterior[[i]][,1]),
                             median(posterior[[i]][,2]),
                             median(posterior[[i]][,3]),
                             median(posterior[[i]][,4])),
                           2,2),
            newplot=FALSE, type="l", 
            alpha = 1 - p.ell,col = COLORS[i], lwd = 2);
    
    
  }
}




## Size investigations ----------------------------------------------

# Size by dN
for(h in unique(X$Water)){
  G = X %>% filter(Water == h)
  for(i in unique(G$Species)){
    c = G %>% filter(Species == i) %>%
      ggplot(aes(x = Weight, y = d15N)) + geom_point() + ggtitle(paste(h, " ", i))
    print(c)
    
  }
}

# Size by dC
for(h in unique(X$Water)){
  G = X %>% filter(Water == h)
  for(i in unique(G$Species)){
    c = G %>% filter(Species == i) %>%
      ggplot(aes(y = Weight, x = d13C)) + geom_point() + ggtitle(paste(h, " ", i))
    print(c)
    
  }
}

# dN by dC
for(h in unique(X$Water)){
  G = X %>% filter(Water == h)
  for(i in unique(G$Species)){
    c = G %>% filter(Species == i) %>%
      ggplot(aes(y = d15N, x = d13C)) + geom_point() + ggtitle(paste(h, " ", i))
    print(c)
    
  }
}


#dN by dC
for(h in unique(X$Water)){
  G = X %>% filter(Water == h)
  for(i in "ST"){
    c = G %>% filter(Species == i) %>%
      ggplot(aes(y = d15N, x = d13C, size = Weight)) + geom_point() + ggtitle(paste(h, " ", i))
    print(c)
    
  }
}


## Overlap function -------------------------------------

```{r}

overlap = function(comm){
  
  data = x %>% 
    filter(community == comm)
  
  spp=length(unique(data$group))
  
  siber.example <- createSiberObject(data) 
  
  posterior <- siberMVN(siber.example, parms, priors)
  
  name_list = names(posterior)
  
  name_matrix = expand.grid(name_list, name_list)
  
  name_matrixy = expand.grid(name_list, name_list) %>%
    separate(Var1, into = c("com1", "spp1")) %>%
    separate(Var2, into = c("com1", "spp2")) %>%
    select(spp1, spp2)
  
  names_modified = name_matrixy %>%
    as.data.frame() %>%
    unite(col = Name, c(spp1, spp2), sep = " v " )
  
  ma = matrix(NA,nrow = 10,ncol = length(name_matrix[,1]))
  
  for(i in 1:length(name_matrix[,1])){ 
    
    overlap = bayesianOverlap(ellipse1 = as.character(name_matrix[i,1]), 
                              ellipse2 = as.character(name_matrix[i,2]), 
                              posterior)
    
    bayes.prop <- (overlap[,3] / (overlap[,2] + overlap[,1] - overlap[,3]))
    
    ma[,i] = bayes.prop
    
    
    
  }
  
  olap_mat = ma %>% 
    as.data.frame() %>%
    summarise_all(.funs = c(mean="median")) %>%
    round(., digits = 5)
  
  olap_mat[2,] = names_modified[,1]
  
  rownames(olap_mat) = c(paste("Com",comm),"Spp Pair")
  
  return(t(olap_mat))
  
  
}


PRL_overlapnum = overlap(3)

LOP_overlapnum = overlap(2)


TPP_overlapnum = overlap(4)

cat = name_list %>% 
  as.numeric(.) %>%
  as.data.frame(.) %>%
  separate(col = 1,into =c("com","group")) %>%
  mutate(group = as.numeric(group)) %>%
  left_join(., legend) %>%
  select(group, Species)

dog = expand.grid(cat$Species, cat$Species)

overlap = full_join(as.data.frame(TPP_overlapnum),
                    as.data.frame(LOP_overlapnum)) %>%
  full_join(as.data.frame(PRL_overlapnum),.) %>%
  #distinct(`Com 2`,`Com 4`, `Com 3`, .keep_all = T) %>%
  select('Spp Pair','Com 2',`Com 4`,`Com 3`)





## Wrong but just in case --------------------------------------------------

```{r, message = F, error = F}

COLORS = RColorBrewer::brewer.pal(20, "Paired")
for(h in 1:1){
  
  data = X  %>%
    data.frame(iso1 = .$d13C,
               iso2 = .$d15N,
               group = as.numeric(as.factor(.$Species)),
               community = as.numeric(as.factor(.$Site)))   %>% 
    filter(Water == "HRM", Site == h) %>%
    select(iso1, iso2, group, community) %>%
    group_by(group, community) %>%
    filter(n()>=exclude) %>%
    na.omit()
  
  data = as.data.frame(data)
  
  
  spp=length(unique(data$group))
  
  siber.example <- createSiberObject(data) 
  
  posterior <- siberMVN(siber.example, parms, priors)
  
  p = ggplot() +
    geom_point(data,
               mapping = aes(x = iso1, y= iso2, col = as.factor(group))) +
    ylim(5,11) + xlim(-36,-26)
  ellip = array(0, dim=c((n.points),length(c(0,1)),spp))
  for(i in 1:spp){
    ellipse_data =  ellipse::ellipse(x = matrix(c(median(posterior[[i]][,1]),
                                                  median(posterior[[i]][,2]),
                                                  median(posterior[[i]][,3]),
                                                  median(posterior[[i]][,4])),
                                                2,2), 
                                     centre = c(median(posterior[[i]][,5]),
                                                median(posterior[[i]][,6])), level = .9, 
                                     npoints = n.points)
    ellip[,,i] = ellipse_data
    if(i == 1){
      vector = vector()
      vector = rep(unique(data$group)[i], n.points)
    }else{
      new = rep(unique(data$group)[i], n.points)
      vector = c(vector,new )
    }
  }
  
  p = p + geom_point(aes(x = ellip[,1,], y = ellip[,2,], col =  as.factor(vector)),size = 01, alpha = .09) +
    ggtitle(paste("Site", paste(h))) + 
    ylab("d15N") + xlab("d13C") + 
    scale_color_discrete(name = "Species" , labels = legend[sort(unique(data$group)),1]) + 
    scale_color_manual(values = c("CC" = 'green', "NRD" = , "GS" = rhg_cols[3]))
  print(p)
}


```


## Plotting just panther ----------------------------
```{r}
plot(1, type="n", xlab= expression({delta}^13*C~'\u2030'), 
     ylab=expression({delta}^15*N~'\u2030'),
     ylim = c(4,13), xlim = c(-35,-20), main = "Panther Lake")


for(i in 1:spp){
  mu = c(median(posterior[[i]][,5]),
         median(posterior[[i]][,6]))
  ellipse(mu = c(median(posterior[[i]][,5]),
                 median(posterior[[i]][,6])),
          sigma = matrix(c(median(posterior[[i]][,1]),
                           median(posterior[[i]][,2]),
                           median(posterior[[i]][,3]),
                           median(posterior[[i]][,4])),
                         2,2),
          newplot=FALSE, type="l", 
          alpha = 1 - p.ell,col = COLORS[i]);
  
}

LLS = combined %>%
  filter(Species == "LLS", Water == "PRL") %>%
  select(Species, Water, d13C, d15N)
points(LLS$d13C, LLS$d15N, pch =2, col = COLORS[i+1])
legend("topleft", c("BND", "CC","ST","RS", "LLS"), lty = rep(1, length(spp+1)),
       col = COLORS)
```
