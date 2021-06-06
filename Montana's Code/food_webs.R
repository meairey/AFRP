## Splitting up Heron Marsh 

source("isotope_functions.R")



for(h in 1:5){
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
  
  color_scheme = COLORS[unique(data$group)]
  
  spp=length(unique(data$group))
  siber.example <- createSiberObject(data) 
  posterior <- siberMVN(siber.example, parms, priors)
  df = data.frame(color = color_scheme, group = unique(data$group))
  data = left_join(data, df)
  combo = combined %>% 
    mutate(group = as.factor(as.numeric(as.factor(Species)))) %>%
    filter(Water == "HRM", Site == h) %>% select(Species, group, Site, d15N, d13C)
  color_scheme = COLORS[unique(combo$group)]
  df = data.frame(color = color_scheme, group = unique(combo$group))
    d = left_join(combo, df)
  
  p = ggplot() + geom_point(combo, mapping = aes(x = d13C, y =d15N , 
                                                 color = group))
  
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
  }
  
  p = p + geom_point(aes(x = ellip[,1,], y = ellip[,2,],color = rep(as.factor(unique(data$group)),each = 1000)),size = 01, alpha = .09) +
    ggtitle(paste("Site", paste(h))) + 
    ylab("d15N") + xlab("d13C") +
    scale_color_manual(values =legend$color[sort(unique(d$group))] , 
                       labels =legend[sort(unique(d$group)),1],
                       name = "Species"
    ) + theme(text = element_text(size = 19))
  print(p)
  
}




## Lake Webs ---------------------------------
n.posts = 100
COLORS = sample(COLORS, replace = FALSE, size = 18)
for(h in lake$Water){
  
  combo = combined %>% 
    mutate(group = as.factor(as.numeric(as.factor(Species)))) %>%
    filter(Water == h, Species != "PDD", Species != "PD") %>% select(Species,
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
    filter(n()>=exclude) %>%
    na.omit() 
  
  data = data[order(data$group),] %>% as.data.frame()
  
  
  spp=length(unique(data$group))
  siber.example <- createSiberObject(data) 
  posterior <- siberMVN(siber.example, parms, priors)
  color_scheme = COLORS[unique(data$group)]
  df = data.frame(color = color_scheme, group = unique(data$group))
  data = left_join(data, df)
  
  color_scheme = COLORS[unique(combo$group)]
  df = data.frame(color = color_scheme, group = unique(combo$group))
  d = left_join(combo, df)
  
  p = ggplot() + geom_point(combo, mapping = aes(x = d13C, y =d15N , 
                                                 color = group))
  
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
  }
  
  p = p + geom_point(aes(x = ellip[,1,], y = ellip[,2,],color = rep(as.factor(unique(data$group)),each = 1000)),size = 01, alpha = .09) +
    ggtitle(paste("Site", paste(h))) + 
    ylab("d15N") + xlab("d13C") +
    scale_color_manual(values = legend$color[sort(unique(d$group))], 
                       labels =legend[sort(unique(combo$group)),1],
                       name = "Species"
    ) + theme(text = element_text(size = 19))
  print(p)
  
}



