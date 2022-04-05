## Splitting up Heron Marsh 
library(dplyr)
library(ggplot2)
library(SIBER)
source("MA2276_Code/Isotopes/isotope_functions.R")


## Splitting it up by taxomony

p = ggplot() + geom_point(data, mapping = aes(x = d13C, y =d15N , 
                                               color = group))


p = p + geom_point(aes(x = ellip[,1,], y = ellip[,2,],color = rep(as.factor(unique(data$group)),each = 1000)),size = 01, alpha = .09) +
  ggtitle(paste("Site", paste(h))) + 
  ylab("d15N") + xlab("d13C") + 
  scale_color_manual(values = legend$tax_col[sort(unique(combined$group))], 
                     labels =legend[sort(unique(combined$group)),4],
                     name = "Species"
  )+ 
  theme(text = element_text(size = 13))

print(p)


## Defining colors for taxonomy -----------

legend$taxon = c("Ictaluridae", "Leuciscidae", "Leuciscidae","Leuciscidae","Leuciscidae","Leuciscidae","Leuciscidae","Leuciscidae","Leuciscidae", "Salmonidae", "Leuciscidae","Leuciscidae","Centrarchidae","Osmeridae","Salmonidae", "Catostomidae")

legend$tax_col = c("#A6CEE3",  "#FF7F00","#FF7F00","#FF7F00","#FF7F00" ,"#FF7F00", "#FF7F00","#FF7F00","#FF7F00", "#CAB2D6" , "#FF7F00", "#FF7F00","#FF7F00",  "#E31A1C" , "#B2DF8A","#A6D854")


### Lakes keep ------------------------
# Functional colors and includes taxonomy
## dat[[1]] = siber example
## dat[[2]] = posterior

n.posts = 100
COLORS = sample(COLORS, replace = FALSE, size = 18)
for(h in 1:8){
  
  dat = data_setup(data, h)
  
  spp=length(names(dat[[2]]))
  
  
  
  p = ggplot() + geom_point(dat[[4]], mapping = aes(x = d13C, y =d15N , 
                                                 color = group))
  
  ellip = array(0, dim=c((n.points),length(c(0,1)),spp))
  
  for(i in 1:spp){
    ellipse_data =  ellipse::ellipse(x = matrix(c(median(dat[[2]][[i]][,1]),
                                                  median(dat[[2]][[i]][,2]),
                                                  median(dat[[2]][[i]][,3]),
                                                  median(dat[[2]][[i]][,4])),
                                                2,2), 
                                     centre = c(median(dat[[2]][[i]][,5]),
                                                median(dat[[2]][[i]][,6])), 
                                     level = .9, 
                                     npoints = n.points)
    ellip[,,i] = ellipse_data
  }
  #,color = rep(as.factor(unique(data$group,each = 1000))
  ## trying this - this is modified check out previous versions if it stops working
  p = p + geom_point(aes(x = ellip[,1,], y = ellip[,2,], color = (rep(as.factor(unique((dat[[3]]$group))),each = 1000))),size = 01, alpha = .09) +
    ggtitle(lake$Name[h]) + 
    ylab("d15N") + xlab("d13C")   + 
    scale_color_manual(values = legend$color[sort(unique(dat[[4]]$group))], 
                       labels =legend[sort(unique(dat[[4]]$group)),1],
                       name = "Species"
    )+ 
    theme(text = element_text(size = 13))
 
  
  z = ggplot() + geom_point(dat[[4]], mapping = aes(x = d13C, y =d15N , 
                                                    color = group))
  
  #z = z + geom_point(aes(x = ellip[,1,], y = ellip[,2,], color = (rep(as.factor(unique((dat[[3]]$group))),each = 1000))),size = 01, alpha = .09) +
    #ggtitle(lake$Name[h]) + 
    #ylab("d15N") + xlab("d13C") + 
    #scale_color_manual(values = legend$tax_col[sort(unique(combined$group))], 
                       #labels =legend[sort(unique(combined$group)),4],
                       #name = "Species")
  
  #print(z)
  print(p)
}

## This is not yet working but i was trying to get rid of the multiple names of families on the legend
z = ggplot() + geom_point(dat[[4]], mapping = aes(x = d13C, y =d15N, color = ))

z = z + geom_point(aes(x = ellip[,1,], y = ellip[,2,], 
                       color = (rep(as.factor(legend$taxon[unique((dat[[3]]$group))]),
                                    each = 1000))),
                       size = 01, 
                       alpha = .09) +
  ggtitle(lake$Name[h]) + 
  ylab("d15N") + xlab("d13C") + 
  scale_color_manual(values = unique(legend$tax_col[sort(unique(dat[[3]]$group))]), 
                     labels =unique(legend$taxon[sort(unique(dat[[3]]$group))]),
                     name = "Species")
z


