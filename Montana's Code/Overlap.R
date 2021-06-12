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

