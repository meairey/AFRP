library(dplyr)
library(tidyr)
library(vegan)
library(tidyverse)
library(kohonen)

#install.packages("kohonen")
setwd("C:/Users/monta/OneDrive - Airey Family/Cornell/ALSC Analysis")
## Setting up species groups
#NatPred = c("ST","NP","RB","LT","WE")
NatPred = c("NP","RB","WE")


#IntroPred = c("BT","RT","YP","LMB","SMB", "SPL", "KOK", "LLS", "TGT","SAL")


NatMin = c("CLM", "CS", "CC","NRD","BM","BND", "LC","FF","PD", "FHM", "LND","FSD")
IntroMin = c("GS","BS","MCS", "ESM", "BRM", "BDS")
#NatOth= c("CP" ,"BB","WS","PS","RBS", "BC","LNS", "BG", "YB", "RWF", "SS","LWF","CIS","CCS", "MM", "AE")
NatOth= c("CP" , "BG", "YB", "RWF","LWF","CIS","CCS", "AE")
IntOth = c("BK", "CRP","TM", "RS", "AW")

dat_spec = data.frame(Species = c(NatPred,
                                  NatMin, 
                                  IntroMin,
                                  NatOth, 
                                  IntOth,
                                  "BB",
                                  "SS",
                                  "PS",
                                  "MM",
                                  "WS", 
                                  "RBS",
                                  "LNS",
                                  "BC",
                                  "SMB",
                                  "LMB",
                                  "ST",
                                  "LT",
                                  "LLS","SPL", "KOK",  "TGT","SAL","BT","RT",
                                  "YP"),
                      class = c(rep("Pred", length(NatPred)),
                                rep("Min", length(NatMin)),
                                rep("IntMin", length(IntroMin)),
                                rep("Other", length(NatOth)),
                                rep("IntOther", length(IntOth)),
                                "BB",
                                "SS",
                                "Centrarchid",
                                "Umbrid",
                                "Catostomid",
                                "Centrarchid",
                                "Catostomid",
                                "Centrarchid",
                                "Centrarchid",
                                "Centrarchid",
                                "Salmonid",
                                "Salmonid",
                                "Salmonid",
                                "Salmonid",
                                "Salmonid",
                                "Salmonid",
                                "Salmonid",
                                "Salmonid",
                                "Salmonid",
                                "Percidae")
                      )

#write.csv(dat_spec, file = "species_alsc.csv")
#dev.off()


setwd("C:/Users/monta/OneDrive - Airey Family/Cornell/ALSC Analysis")

dat_spec = read.csv("species_alsc.csv")
# ------- Set up Data Frame ---

## This is presence absence 
## Pres/abs ---------
alsc = read.csv("ALSCDATA_fish_altered.csv", header=T)[,1:54] %>% 
  replace(!is.na(.),1)  %>%
  replace(is.na(.),0) %>%
  mutate(Richness = rowSums(.)) %>%
  cbind(read.csv("ALSCDATA_fish_altered.csv", header=T)[,56:60]) %>%
  cbind(read.csv("ALSCDATA_chem_altered.csv",header=T)) %>%
  #rename(Long = LONG..NAD83.DD.)%>%
  mutate(LONG = LONG*-1) %>%
  filter(Richness > 0)


end_spp = which(colnames(alsc)=="Volume.m3.")-1 
sp_waters = (alsc[,1:end_spp-1] %>% colSums(.))
common_spp =which(sp_waters > 10)

### This has abundance data
# Abundance -----------
alsc = read.csv("ALSCDATA_fish_altered.csv", header=T)%>%
  replace(is.na(.),0 ) %>%
  mutate(Richness = rowSums(.[,1:54])) %>%
  cbind(.,read.csv("ALSCDATA_chem_altered.csv",header=T)) %>%
  select(-NF) %>%
  #rename(Long = LONG..NAD83.DD.)%>%
  mutate(LONG = LONG*-1)%>%
  filter(Richness != 0 )


grouped_long = alsc %>% rownames_to_column(var = "lake_id") %>%
  pivot_longer(ST:AW, names_to ="Species", values_to =  "Abundance") %>%
  select(lake_id,Species, Abundance) %>% 
  filter(Abundance > 0) %>% 
  group_by(Species) 

class_totals = left_join(grouped_long, dat_spec) %>% 
  group_by(lake_id, class) %>%
  summarize(class_totals = sum(Abundance))

sums = class_totals %>% ungroup() %>% group_by(lake_id) %>% 
  summarise(total_abundance = sum(class_totals)) 

left_join(class_totals, sums) %>% 
  mutate(percent = class_totals / total_abundance) %>% 
  group_by(class) %>% 
  summarise(class_average = median(percent))


# # Creating an easy marker for plotting


## This removes rare species

rare = alsc[,common_spp] %>%
  mutate(Richness = rowSums(.)) %>%
  filter(Richness != 0 ) %>%
  select(-Richness) %>% 
  
  mutate(row.sum = rowSums(.)) %>% 
  filter(row.sum > 0) %>% 
  select(-row.sum)

data=sample(c(1:length(rare[,1])), size=900, replace=F)

minnows = alsc %>% 
  select(NatMin, Volume.m3.,SO4..mg.L.1.,DIC..mg.L.1., DOC..mg.L.1.,Ca..mg.L.1.,LAB_pH, TOTAL.P1..mg.L.1.) %>%
  mutate(m_rich = rowSums(.[,1:length(NatMin)])) %>%
  filter(m_rich != 0)

#alsc = alsc %>%
#select(-colnames(alsc)[which(rare<20)])


#Species = colnames(alsc)[1:which(colnames(alsc)=="Richness")-1]
#rare_species = Species[which(rare<20)]
#common_species = Species[which(rare>200)]
#end_spp = which(colnames(alsc)=="sums")-1

## NMDS ----------------
dat_spec.frame = dat_spec %>% as.data.frame() %>% mutate(nat_therm = paste(upland_invasive, thermal)) %>%
  mutate(group_therm = paste(upland_invasive,group))
nmds_codes = alsc_fish_codes %>% rename( "Scientific" = Species ) %>% 
  rename("Species" = code)

nmds=metaMDS(rare[data,], k=2)
nmds$species %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  mutate(Species = rownames(.)) %>% 
  left_join(dat_spec) %>% 
  left_join(nmds_codes) %>%
  as.data.frame() %>%
  ggplot(aes(x = MDS1, y = MDS2,
             label =(Common), color = upland_invasive)) + 

  theme_minimal() +
  geom_text(size =3.5,key_glyph = "rect")+ 
  stat_ellipse(level = .9, size = 1) + 
  #ggtitle("ALSC - pres/abs, rare = 10, k = 2, size = 900")+
  xlim(-.1,.05) +
  labs(col = "Status") +
  #scale_color_discrete( labels = c(int = "Invasive", nat = "Native", low_only = "Lowland Native"), ) +
  scale_color_manual(labels = c("Invasive","Lowland Native","Native"), values= c("#588157","#988F2A","#725752")) 
  


nmds_minnow = metaMDS(minnows[,1:length(NatMin)], k = 2, try = 100)

## what average sized body of water does each species inhabit
newest_bin = c(0,200000, 500000, 1000000, 5000000, 10000000, 50000000, 1500000000)
lake_size = alsc %>% rownames_to_column(var = "lake_id") %>%
  pivot_longer(ST:AW, names_to ="Species", values_to =  "Abundance") %>%
  group_by(Species) %>% 
  filter(Abundance>0) %>%
  summarize(mean_vol = median(Volume.m3.), sd_vol = sd(Volume.m3.)) %>%
  mutate(lake_size_bin = .bincode(mean_vol, newest_bin )) 


nmds$species %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  mutate(Species = rownames(.)) %>% 
  left_join(dat_spec.frame) %>% 
  as.data.frame() %>%
  left_join(lake_size) %>%
  ggplot(aes(x = MDS1, y = MDS2,
             label =(Species), col = as.factor(lake_size_bin))) + 
  
  geom_text(size =4)+ 
  stat_ellipse(level = .9) + 
  ggtitle("ALSC - abundance, rare = 10, k = 2, size = 900")+
  xlim(-.1,.025) + 
  labs(color = "Lake size: small (1) - big (5)")

### Count
## coocur matrix needs it to be presence absense 
coccur_matrix = alsc %>% select((ST:(end_spp-1))) %>% rownames_to_column() %>% pivot_longer(2:end_spp,names_to = "Species" ) %>% filter(value>0) %>% mutate(species = Species) %>% group_by(rowname) %>% complete( species,Species,value) %>% ungroup %>%  group_by(Species, species) %>% unique() %>% summarize(cooccur = n())  %>% rename(s2 = species)










prop_comm = alsc %>% rownames_to_column(var = "lake_id") %>%
  pivot_longer(ST:AW, names_to ="Species", values_to =  "Abundance") %>%
  group_by(Species) %>% 
  filter(Abundance>0) %>%
  summarize(count = n()) %>%
  mutate(proportion = count / 1149)

# This needs it to be an abundance matrix
average_abundance = alsc %>% rownames_to_column(var = "lake_id") %>%
  pivot_longer(ST:AW, names_to ="Species", values_to =  "Abundance") %>%
  group_by(Species) %>% 
  filter(Abundance>0) %>% 
  group_by(Species) %>% 
  summarize(average_abundance = mean(Abundance))

prop_abund = prop_comm %>% 
  left_join(average_abundance) %>%
  mutate(prop_abund_metric  = proportion*average_abundance)


# Relative area 
library(stringr)


prop_bin = c(-.1,.15,.9)
med_area_count = med_relative.data %>%
  mutate(Species = str_replace(Species, "NPD", "PD")) %>% 
  mutate(Species = str_replace(Species, "BNM", "BM")) %>%
  left_join(prop_comm) %>%
  left_join(dat_spec) %>% 
  left_join(prop_abund) %>%
  group_by(Species, family, proportion, prop_abund_metric) %>%
  mutate(proportion_bin = .bincode(proportion, prop_bin))


med_area_count %>%
  na.omit() %>% 
  ggplot(aes(x = as.numeric(proportion), y = as.numeric(med_area), col = as.factor(proportion_bin))) + 
  geom_boxplot() +
  geom_point() +
  theme_minimal() + 
  ylab("Relative Area") + xlab("Proportion of ALSC Lakes") +
  labs(col = "Frequency of Occurence") + 
  scale_color_discrete(labels = c("Rare","Commmon")) 
  
  
t.test(med_area_count$med_area[which(cat$proportion_bin == 1)],med_area_count$med_area[which(cat$proportion_bin == 2)] )


## It does not look like mean overlap is related to # of times coocur in park 

dat_spec = dat_spec %>% select(-s2, -family_s2)
dat_spec2 = dat_spec %>% mutate(s2 = Species, familys2 = family) %>% select(s2, familys2)

overlap_coocur = overlap_data %>% 
  pivot_longer(2:length(.[1,]), names_to = "Community", values_to = "Values") %>% na.omit() %>%
  rename("Species_Pair" = `Spp Pair`) %>% 
  group_by(Species_Pair) %>% 
  #summarize(mean = mean(as.numeric(Values), na.rm = T)) %>%
  separate(Species_Pair, into = c("s1", "v", "s2"), sep = " ") %>% 
  select(-v) %>%                                                                                                             
  filter(s1 != s2) %>% rename(Species = "s1") %>% 
  left_join(prop_comm) %>% 
  full_join(coccur_matrix) %>% left_join(dat_spec) %>% left_join(dat_spec) %>% left_join(dat_spec2) %>%
  unite("species_pair",family, familys2, remove = F) %>% 
  filter(Values >0) %>%
  filter(species_pair %in% c("Centrarchid_Centrarchid", "leuciscidae_leuciscidae", "salmonidae_salmonidae","Centrarchid_leuciscidae", "leuciscidae_Centrarchid"))


overlap_coocur %>%
  ggplot(aes(x = cooccur, y =as.numeric(Values), col = species_pair)) +
  geom_point() + 
  geom_smooth(method = lm)

overlap_coocur %>%
  filter(family %in% c("Centrarchid","leuciscidae","Catostomid"), familys2 %in% c("Centrarchid","leuciscidae","Catostomid")) %>%
  ggplot(aes(x = cooccur, y =as.numeric(Values))) +
  geom_point(aes(col = species_pair)) + 
  geom_smooth(method = lm, col = "black") +
  xlim(0,300) + 
  theme_minimal() +
  ylab("Pairwise overlap") + 
  xlab("Coocurance instances")


cat = overlap_coocur %>%
  filter(family %in% c("Centrarchid","leuciscidae","Catostomid"), familys2 %in% c("Centrarchid","leuciscidae","Catostomid"))

cor.test(as.numeric(cat$Values), cat$cooccur)
summary(lm(cat$Values~ cat$cooccur))
  
lm(data = .,Values~ cooccur) %>% summary()
summary(lm(overlap_coocur$Values ~ overlap_coocur$cooccur))
## Relative area -------------
med_area_prop = med_relative.data %>% left_join(prop_comm) %>% left_join(dat_spec.frame) %>% mutate(propotion_bin = (.bincode(proportion, c(0,.3,.9)))) %>% na.omit() %>%
  group_by(family, Species, propotion_bin) %>% 
  summarize(mean = mean(med_area))%>% filter(family %in% c("Centrarchid","leuciscidae","Catostomid")) 

#min = med_area_prop %>% filter(family == "leuciscidae")
summary(lm(min$med_area~min$proportion))
cor.test(min$med_area, min$proportion)
summary(lm(med_area_prop$med_area~med_area_prop$proportion))

med_area_prop%>% ggplot(aes(x = proportion, y = mean_area, col = family)) +xlim(0,.6)+ geom_point() + geom_smooth(method = lm, se = F) 


med_area_prop %>% na.omit() %>% 
  #filter(family %in% c("Centrarchid","leuciscidae","Catostomid")) %>%
  ggplot(aes(x = as.factor(propotion_bin), y = mean)) + geom_boxplot()

group_by(family, Species, propotion_bin) %>% 
  summarize(mean = mean(med_area)) 


wilcox.test(med_area_prop$mean[which(med_area_prop$propotion_bin == 1)], med_area_prop$mean[which(med_area_prop$propotion_bin == 2)])
wilcox.test(cat$med_area[which(cat$propotion_bin == 1)],cat$med_area[which(cat$propotion_bin == 3)] )

## full area not relative area 



area_prop %>% 
  group_by(Water, proportion, Species) %>% summarize(median_area = median(Overlap)) %>% 
  ggplot(aes(x = proportion, y = median_area)) + geom_point() + geom_smooth(method = lm)
  
  
  
  ggplot(aes(x = proportion, y = Overlap)) + geom_point() + geom_smooth( method = lm)

## Mantel test -------- 
comm.dist = vegdist(rare_dat, method = "bray")
vol.dist = dist(alsc$Volume.m3., method = "euclidean")
mantel(xdis = comm.dist, 
       ydis = vol.dist, 
       method = "spearman",
       permutations = 10, 
       na.rm = TRUE)

Ca.dist = dist(alsc$TOTAL.P1..mg.L.1., method = "euclidean")
mantel(xdis = comm.dist,
       ydis = Ca.dist,
       method = "spearman", 
       permutations = 10, 
       na.rm = TRUE)

aa = as.vector(cpue.dist)
tt = as.vector(time.dist)
gg = as.vector(temp.dist)


## Question about community type ------

summary = alsc %>%
  mutate(natmin = rowSums(.[NatMin])) %>%
  mutate(intrmin = rowSums(.[IntroMin])) %>%
  mutate(natpred = rowSums(.[NatPred])) %>%
  mutate(intropred = rowSums(.[IntroPred])) %>%
  mutate(natoth = rowSums(.[NatOth])) %>%
  mutate(intoth = rowSums(.[IntOth]))


cat = summary %>%
  select(intoth,intropred, intrmin, natpred, natmin, natoth) %>%
  replace(. > 0,1 )

rough_sum = cat %>%
  group_by_all %>%
  summarise(COUNT = n()) %>%
  arrange(-COUNT)

rough_sum %>%
  filter(intropred == 1) %>%
  ungroup() %>%
  select(COUNT) %>%
  sum()


rough_sum %>%
  filter(natmin == 1) %>%
  ungroup() %>%
  select(COUNT) %>%
  sum()


rough_sum %>%
  filter(natmin == 1, intropred == 1) %>%
  ungroup() %>%
  select(COUNT) %>%
  sum()



summary_summary = summary %>%
  mutate(min = natmin + intrmin) %>%
  mutate(pred = natpred + intropred) %>%
  mutate(comm = natoth + intoth) %>%
  select(min, pred, comm) %>%
  group_by_all %>%
  summarise(COUNT = n()) %>%
  arrange(-COUNT)

library(vegan)
plot(nestednodf(rough_sum[,1:6]),names=T)

library(ggplot2)
dog = as.data.frame(colSums(cat))
dog$names = rownames(dog)
ggplot(dog, aes(x = names, y = colSums(cat))) + geom_bar(stat = 'identity') + 
  ylab("# of lakes") 

colSums(rough_sum)

hist(summary$natmin)

hist(summary$natpred)









## unsupervised mapping --
alsc.som.data = alsc %>% 
  unite("Common", common_species, sep = "", remove = F) %>%
  select(Elevation..m., ANC..?eq.L.1.,DIC..mg.L.1., Common) %>%
  na.omit() 

training = sample(c(1:1087),size = (1087/2), replace = F)
alsc.som.data.training = alsc.som.data[training,]
alsc.som.data.test = alsc.som.data[-c(training),]

set.seed(7)
alsc.som = som(X = as.matrix(alsc.som.data.training[,c(1:3)]), grid = somgrid(10,10,"hexagonal"))
plot(alsc.som)


alsc.som$unit.classif

plot(alsc.som, type = "mapping", pchs = 20)

plot(alsc.som, type = "codes", main = "codes plot")


#if this continuously decreases, and is a curve, it means you need more iterations. It needs to reach a plateau
plot(alsc.som, type = "changes")

### Look at this you dont want all the nodes to be concentrated in specific parts of the map. Try to get at least 5-10 samples per node 
plot(alsc.som, type = "counts")

plot(alsc.som, type = "dist.neighbours")

## Clustering -- 
library(factoextra)
fviz_nbclust(alsc.som$codes[[1]], kmeans, method = "wss")
set.seed(100)
clust = kmeans(alsc.som$codes[[1]],4)

## Supervised maps--- 



set.seed(100)

train = as.matrix(alsc.som.data.training[,c(1:3)])
class = xyf(train, classvec2classmat(alsc.som.data.training[,4]), grid= somgrid(10,10,"hexagonal"))
## This shows the training progress
plot(class, type = "changes")


test = as.matrix(alsc.som.data.test[,c(1:3)])
testXY = list(indep = test, depend = as.factor(alsc.som.data.test[,4]))

pred = predict(class, newdata = testXY)
cat =table(Predict = pred$predictions[[2]],Actual = testXY$depend)

plot(alsc.som, type = "codes", bgcol = rainbow(9)[clust$cluster], main = "Cluster SOM")

add.cluster.boundaries(alsc.som, clust$cluster)


c.class <- kmeans(class$codes[[2]], 3)
par(mfrow = c(1,2))
plot(class, type = "codes",
     main = c("Unsupervised SOM", "Supervised SOM"), 
     bgcol = rainbow(3)[c.class$cluster])
add.cluster.boundaries(class, c.class$cluster)
















try = alsc %>%
  unite("Common",c("ST","BB","WS","GS"), sep = "", remove = F) %>%
  unite("Rare", rare_species, sep = "", remove = F)

try = alsc %>%
  unite("Common",c("ST","BB","WS","GS"), sep = "", remove = F) %>%
  mutate("Rare_Richness" = rowSums(.[rare_species])) %>%
  select(Common, Rare_Richness)
try = %>%
  ggplot(aes(x = (Common), y = Rare_Richness))+geom_point() +
  geom_smooth(method = "lm")

summary(lm(try$Rare_Richness ~ try$Common))


try %>%
  select(Common, Rare) %>%
  ggplot(aes(x =as.numeric(Common), y = (Rare))) + geom_point()

unique(try$Common)
table(try$Common)
table(try$Rare)


## Distinct rows = Distinct communities 
distinct(alsc[,c(1:end_spp)])


try = as.data.frame(alsc) %>% 
  unite("Community", Species, sep = "", remove = F)

class(try$Community)

hist = table(try$Community)


as.data.frame(alsc) %>% 
  filter(NatMin == 1)


## Maybe i should subset the communities and lakes that have communities that occur most frequently 



## With and without minnows 

alsc_trans =
  
  trout = alsc %>% rownames_to_column(var = "lake_id") %>% 
  filter(ST > 0) 

#%>%
# filter(SMB > 0) 

#select(ST, lake_id)

trout %>%  rownames_to_column(var = "lake_id") %>%
  pivot_longer(ST:AW, names_to ="Species", values_to =  "Abundance") %>%
  select(lake_id,Species, Abundance) 

ct = class_totals %>% pivot_wider(, names_from = class, values_from = class_totals)

left_join(trout, ct) %>% ggplot(aes(x = ST, y = BB)) + geom_point()

left_join(trout, class_totals) %>% 
  ggplot(aes(y = ST, x = class_totals)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~class)


ggplot(trout, aes(y=ST,x= CC)) + geom_point(alpha = .5) + xlim(0,1000)+ geom_smooth(method = "lm")


c = cat %>% group_by(lake_id) %>% summarize(richness = n()) 

left_join(trout, c) %>% ggplot(aes(x = richness, y = ST)) + geom_point() + geom_smooth(method = "lm") + xlim(0,3)

https://link-springer-com.proxy.library.cornell.edu/article/10.1007/s10530-022-02783-w#change-history

minnows arent always bad for trout Minnow introductions in mountain lakes result in lower salmonid densities













#--------------------------------------------------------- Trait biogeography

# Pearson corelation between chemical variables 

end_spp = which(colnames(alsc)=="Volume.m3.")-1 
sp_waters = alsc[,1:end_spp] %>% colSums(.)
common_spp =which(sp_waters > 5) 

### This has abundance data

alsc = read.csv("ALSCDATA_fish_altered.csv", header=T)%>%
  replace(is.na(.),0 ) %>%
  mutate(Richness = rowSums(.[,1:54])) %>%
  cbind(.,read.csv("ALSCDATA_chem_altered.csv",header=T)) %>%
  select(-NF) %>%
  #rename(Long = LONG..NAD83.DD.)%>%
  mutate(LONG = LONG*-1)%>%
  filter(Richness != 0 )

chem = read.csv("ALSCDATA_chem_altered.csv",header=T) %>% as.data.frame() %>% mutate(row_id = row_number())
chem %>% cor(use="complete.obs")
fish = read.csv("ALSCDATA_fish_altered.csv", header=T)%>%
  replace(is.na(.),0 ) 

## ANC and DIC (.88), AND and CA_mgl (.96, ), DIC and CA_mgL. Nothing else highly coorelated

whole_frame %>% cor(use = "complete.obs")

whole_frame %>%
  mutate(RepGuild1 = as.numeric(as.factor(RepGuild1))) %>% 
  mutate(RepGuild2 = as.numeric(as.factor(RepGuild2))) %>%
  select(RepGuild1)


data = left_join(whole_frame, fish_codes) %>% select(code, everything())
data 

trait_dat = fish[,1:54] %>% 
  mutate(row_id = row_number()) %>% 
  select(row_id, everything()) %>%
  pivot_longer(2:55,names_to = "code", values_to = "abundance") %>% 
  filter(abundance > 0) %>% 
  left_join(data)

p = trait_dat %>% filter(code %in% unique(data$code))  

## essentially like abundance of detritivores in a water body ~ environmental predictor
library(ggplot2)
p %>% filter(Herbivory2 == "plants/detritus+animals (troph. 2.2-2.79)")
p %>% group_by(row_id, Herbivory2) %>%
  summarise(trophic_richness = n()) %>% 
  as.data.frame() %>% 
  complete(row_id, Herbivory2) %>% 
  filter(is.na(Herbivory2) == FALSE) %>% 
  mutate(trophic_richness = replace_na(trophic_richness, 0)) %>% 
  filter(Herbivory2 == "mainly plants/detritus (troph. 2-2.19)") -> trait_frame
length_bins = c(0, 5,10,15,20)
cat = left_join(trait_frame, chem) %>% mutate(length_bin = .bincode(TL_cm, length_bins))

summary(lm(cat$trophic_richness~cat$P_total))

cat %>% ggplot(aes(x = trophic_richness, y =P_total)) + geom_point() + 
  geom_smooth(method = "lm")

p %>% select(row_id, code, DemersPelag) %>% group_by(row_id, DemersPelag) %>% 
  filter(DemersPelag == "benthopelagic") %>%
  summarise(DemersPelag = n()) %>% left_join(chem) -> dem 

summary(lm(dem$DemersPelag~dem$LAT)) 

dem %>% ggplot(aes(x = LAB_pH, y = DemersPelag)) + geom_point()
This connects to the fish_base R file                                                                                                        
## ----------------fishbaseR ----------------------


fish_species = c("Margariscus nachtriebi", "Margariscus margarita","Luxilus cornutus",
                 "Chrosomus eos","Notemigonus crysoleucas","Pimephales promelas",
                 "Rhinichthys atratulus","Semotilus atromaculatus" ,
                 "Couesius plumbeus","Rhinichthys cataractae",
                 "Hybognathus hankinsoni","Hybognathus regius",
                 "Pimephales notatus","Notropis bifrenatus", 
                 "Exoglossum maxillingua","Chrosomus neogaeus",
                 "Notropis volucellus","Semotilus corporalis")
length(fish_species)
          
fish_codes = c("PD", "PD","CS", "NRD","GS","FHM",
               "BND","CC","LC","LND","BM","SM",
               "BNM","BS","CLM","FSD","MS","FF") %>%
  cbind(fish_species) %>%
  as.data.frame() %>%
  rename(Species = fish_species) %>% 
  rename(code = '.')


species(fish_species, 
        fields = c(species_fields$habitat,
                   species_fields$longevity,
                   species_fields$depth, 
                   species_fields$morph,
                   species_fields$misc)) %>%
  colnames()

#spawning(fish_species) ## Doesnt have a lot of info for most of the species
#preds = predators(fish_species) # may also be useful

repro = reproduction(fish_species) %>%
  select(SpecCode, Species, RepGuild1, RepGuild2) # has useful reproduction guilds 

general = species(fish_species, 
                  fields = c(species_fields$id,species_fields$habitat,
                             species_fields$longevity, 
                             species_fields$depth, 
                             species_fields$morph,
                             species_fields$misc)) %>% 
  rename(spp = Species)


morpho = morphology(fish_species) %>% 
  select(SpecCode, Species, 
         BodyShapeI, BodyShapeII, 
         PosofMouth, TypeofMouth)



ecology = ecology(fish_species) %>% 
  select(SpecCode, Species, SoftBottom, Sand, Coarse, Fine, Level, Sloping,
         Silt, Mud, Ooze, Detritus, Organic, HardBottom, Rocky, Rubble, Gravel,
         Stream, Lakes, Herbivory2, FeedingType, DietTroph, FoodTroph)

whole_frame = full_join(general, repro) %>% 
  full_join(., morpho) %>% 
  full_join(ecology) %>% 
  select(c(-Electrogenic, -WeightFemale,
           -Weight, -LTypeComF, -CommonLengthF,
           -LTypeComM, -LTypeMaxF,-LengthFemale,
           -DepthRangeDeep, -DepthRangeComShallow, 
           - DepthRangeShallow, -LongevityCaptive,
           -Brack, -Saltwater, -TypeofMouth, -DepthRangeComDeep)) %>% 
  rename(TL_cm = Length) %>% 
  select(-LTypeMaxM) %>%
  select(Species, everything()) %>% 
  select(Species, DemersPelag, LongevityWild, TL_cm, RepGuild1,
         RepGuild2, BodyShapeI, Herbivory2, FeedingType, FoodTroph)




## Nestedness analysis --------------------

# Remove fishless lakes 

common_spp =which(sp_waters > 5)  ## filter out rare taxa 


fish_alsc = alsc %>% 
  select(ST:AW) %>%
  mutate(species_richness = rowSums(.)) %>%
  rownames_to_column(var = "lake_id") %>%
  filter(species_richness > 0)  %>% t()
nestednodf(fish_alsc)
plot(nestednodf(fish_alsc), names= T)

## nat names 


## Overall community nestedness
watershed_pres = alsc %>%
  pivot_longer(ST:AW, names_to ="Species", values_to =  "Presence") %>% 
  left_join(dat_spec) %>% 
  filter(upland_invasive == "nat") %>% ## filters to only include native species 
  select(Common, Presence, Watershed) %>% 
  group_by(Watershed, Common) %>%
  summarize(watershed_pres = sum(Presence)) %>% 
  mutate(watershed_pres = case_when(
    watershed_pres > 0 ~ 1, 
    TRUE   ~ watershed_pres)) %>%
  mutate(watershed_pres = as.numeric(watershed_pres)) %>%
  pivot_wider(names_from = Common, values_from = watershed_pres)%>% 
  column_to_rownames(var = "Watershed") %>% t()

k = nestednodf(watershed_pres)
k$statistic
plot(nestednodf(watershed_pres), names= T)
nestednodf(watershed_pres)

k$comm %>% as.data.frame %>%
  rownames_to_column(var = "rowname") %>%
  pivot_longer(2:9, names_to = "Watershed", values_to = "Nest") %>%
  rename(Species = rowname) %>%
  left_join(dat_spec) %>%
  ggplot(aes(y = factor(Species, 
                        level = rownames(k$comm)),
             x = factor(Watershed,
                        level = colnames(k$comm)), 
             fill = as.factor(Nest))) +
  theme_minimal() + 
  geom_tile()  +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size = 12)) + 
  theme(axis.text.y = element_text(size = 12)) + 
  scale_fill_manual(values = c(
    "0" = "white",
    "1" = "#7E9181"
  )) + 
  
  ylim(rev(rownames(k$comm))) +  
  ylab("") +
  xlab("") +
  theme(legend.position="none")


c = data.frame(Watershed = colnames(k$comm), nest =print(plot(nestednodf(watershed_pres), names= T)))

d.1 = alsc  %>% 
  group_by(Watershed) %>%
  summarize(count =n())
newest_bin = c(0,30000, 500000, 1000000, 5000000, 10000000, 50000000, 1500000000)

lit_bin = c(0,5, 20, 40,  60, 150)

d = alsc  %>% 
  group_by(Watershed) %>%
  mutate(lake_size_bin = .bincode(Volume.m3., newest_bin )) %>% 
  mutate(lit_area_bin = .bincode(Littoral.Area..ha., lit_bin)) %>%
  left_join(d.1) %>%
  group_by(Watershed, lit_area_bin, count) %>% 
  summarize(lit_count = n()) %>%
  mutate(prop = lit_count / count) %>%
  full_join(c) %>% 
  filter(lit_area_bin ==1) 

d %>% ggplot(aes(x = nest, y = prop)) + geom_point() + geom_smooth(method = lm) 
  
d = alsc  %>% 
  group_by(Watershed) %>% 
  sample_n(37) %>%
  summarize(mean_elev = mean(Elevation..m.),
            mean_cal = mean(Ca_mgL), 
            mean_vol = mean(Volume.m3.), 
            sd_vol = sd(Volume.m3.), 
            med_lit = median(Littoral.Area..ha.),
            mean_lit = mean(Littoral.Area..ha.),
            total_lit = sum(Littoral.Area..ha.),
            mean_ANC = mean(ANC), mean_lat = mean(LAT), 
            total_volume = sum(Volume.m3.), 
            total_lakes = n(), 
            med_volume = median(Volume.m3.))%>% full_join(c)

alsc %>% ggplot(aes(x = Littoral.Area..ha.)) + geom_histogram() + 
  facet_wrap(~Watershed) + xlim(0,20)

d %>%
  ggplot(aes( x = nest, y = mean_vol)) +
  geom_point() + geom_smooth(method = lm, col = "black") + 
  ylab("Mean Lake Volume (m3)") + xlab("Rank Nestedness")
cor.test(d$nest, d$mean_vol)

d %>%
  ggplot(aes( x = nest, y = total_volume)) + 
  geom_point() + 
  geom_smooth(method = lm, col = "black") + 
  ylab("Total Volume (m3)") + xlab("Rank Nestedness")
cor.test(d$nest, d$total_volume)

d %>%
  ggplot(aes( x = nest, y = total_lakes)) + 
  geom_point() +
  geom_smooth(method = lm, col = "black") + 
  ylab("Total # Lakes") + xlab("Rank Nestedness")

cor.test(d$nest, d$total_volume)


d %>% ggplot(aes(x = nest, y = med_volume)) + 
  geom_point() +
  geom_smooth(method = lm, col = "black")

cor.test(d$nest, d$med_volume, method = "spearman")


d %>% ggplot(aes(x = nest, y = mean_lit)) + 
  geom_point() +
  geom_smooth(method = lm, col = "black")

cor.test(d$nest, d$mean_lit)

d %>% ggplot(aes(x = nest, y = total_lit)) + 
  geom_point() +
  geom_smooth(method = lm, col = "black")
cor.test(d$nest, d$total_lit)

alsc %>% ggplot(aes(x = Littoral.Area..ha.)) + geom_histogram() + 
  facet_wrap(~Watershed)


## Mean volume is a beautiful relationship, sd_vol is not significant
# elevation shows nothing, mean_lit shows nothing, mean_ANC shows nothing, mean_lat
d %>% ggplot(aes(x = nest, y = prop)) + 
  geom_point() +
  geom_smooth(method = lm, col = "black") + ylab("proportion of lakes < 30K m3")
summary(lm(d$prop ~ d$nest))


cor.test(d$prop,d$nest, method = "spearman")

d %>% ggplot(aes(x = total_volume, y = total_lit)) + geom_smooth(method = lm) + geom_point()
cor.test(d$prop,d$nest, method = "pearson")
## This seems to say that higher proportion of small lakes is more nested, and lower number of lakes is more nested. I think that overall makes me believe the trend that says the watershed with the smaller mean lake volume is more nested than the others. 
d %>% ggplot(aes(x = nest, y = lake_count)) + 
  geom_point() +
  geom_smooth(method = lm, col = "black") + ylab("Total Number of Lakes")
summary(lm(d$count ~ d$nest))
cor.test(d$count,d$nest, method = "spearman")
cor.test(d$count,d$nest, method = "pearson")



## If you play around with total count of small lakes and the proportion of small lakes present - it looks like the least nested systems have a larger number of small lakes compared to the most nested systems. But, it seems like small lakes are a greater proportion of the overall lake 

summary(lm(d$mean_vol~d$nest))
cor.test(d$nest, d$mean_vol, method = "spearman")
cor.test(d$mean_vol, d$nest)



# Rare taxa filtered nestedness
watershed_pres_rare = alsc %>%
  select(as.vector(common_spp), Watershed, -Richness) %>% 
  pivot_longer(ST:FHM, names_to ="Species", values_to =  "Presence") %>% 
  select(Species, Presence, Watershed) %>% 
  group_by(Watershed, Species) %>%
  summarize(watershed_pres = sum(Presence)) %>% 
  mutate(watershed_pres = case_when(
    watershed_pres > 0 ~ 1, 
    TRUE   ~ watershed_pres)) %>%
  mutate(watershed_pres = as.numeric(watershed_pres)) %>%
  pivot_wider(names_from = Species, values_from = watershed_pres)%>% 
  column_to_rownames(var = "Watershed") %>% t()



k = nestednodf(watershed_pres_rare)
k$statistic
plot(nestednodf(watershed_pres_rare), names= T)
nestednodf(watershed_pres_rare)
## it seems like the overall community appears to not be very nested across the different basins within the ADK
## Maybe ill split this up by taxa and check it out 

## Nestedness of minnow communities -------------

watershed_pres_minnow = alsc %>%
  pivot_longer(ST:AW, names_to ="Species", values_to =  "Presence") %>% 
  filter(Species %in% c(NatMin, IntroMin)) %>%
  select(Species, Presence, Watershed) %>% 
  group_by(Watershed, Species) %>%
  summarize(watershed_pres = sum(Presence)) %>% 
  mutate(watershed_pres = case_when(
    watershed_pres > 0 ~ 1, 
    TRUE   ~ watershed_pres)) %>%
  mutate(watershed_pres = as.numeric(watershed_pres)) %>%
  pivot_wider(names_from = Species, values_from = watershed_pres)%>% 
  column_to_rownames(var = "Watershed") %>% t()

k = nestednodf(watershed_pres_minnow)
k$statistic
plot(nestednodf(watershed_pres_minnow), names= T)

nestednodf(watershed_pres_minnow)[[1]]

print(plot(nestednodf(watershed_pres_minnow), names= T))


c = data.frame(Watershed = colnames(k$comm), nest =print(plot(nestednodf(watershed_pres_minnow), names= T)))
c
## Nestedness within each watershed -------------

for(i in 1:length(unique(alsc$Watershed))){
  quad_nested_data = alsc %>%
    filter(Watershed == unique(alsc$Watershed)[i]) %>%
   
    pivot_longer(ST:AW, names_to ="Species", values_to =  "Presence") %>% 
    
    #filter(Species %in% c(NatMin, IntroMin)) %>%
    select(Species, Presence, Watershed, Quad) %>%
  
  
    group_by(Species, Quad) %>%
    summarize(quad_pres = sum(Presence)) %>% 
    mutate(quad_pres = case_when(
      quad_pres > 0 ~ 1, 
      TRUE   ~ quad_pres)) %>%
    mutate(quad_pres = as.numeric(quad_pres)) %>%
    pivot_wider(names_from = Species, values_from = quad_pres) %>% 
    column_to_rownames(var = "Quad") %>%  t() %>% 
    as.data.frame() %>% 
    mutate(row = rowSums(.)) %>% filter(row > 0) %>% 
    select(-row)
  
  
  #print(nestednodf(quad_nested_data))
  #plot(nestednodf(quad_nested_data), names = T)
  k = nestednodf(quad_nested_data)
  
  c = data.frame(Quad = colnames(k$comm), nest =print(plot(nestednodf(quad_nested_data), names= T)))
  
  d = alsc %>% filter(Watershed == unique(alsc$Watershed)[i]) %>% group_by(Quad) %>%
    summarize(mean_volume = mean(Volume.m3.)) %>% full_join(c)
  
  #print(d %>%
    #ggplot(aes( x = nest, y = mean_volume)) +
    #  geom_point() + 
     ## geom_smooth(method = lm, col = "black") +
     # ylab("Mean Lake Volume (m3)") + xlab("Rank Nestedness")) 
  
  
  #print(summary(lm(d$mean_volume~d$nest)))
    
  print(cor.test(d$mean_volume, d$nest, method = "spearman"))
  
  
  j = k$comm %>% as.data.frame %>% rownames_to_column(var = "rowname") %>% pivot_longer(2:9, names_to = "Watershed", values_to = "Nest") %>%
    ggplot(aes(y = factor(rowname, level = rownames(k$comm)), x = factor(Watershed, level = colnames(k$comm)), fill = as.factor(Nest))) + geom_tile()  +
    theme(axis.text.x = element_text(angle = 90), text = element_text(size = 9)) + 
    scale_fill_manual(values = c(
      "0" = "white",
      "1" = "black"
    )) + 
    ylim(rev(rownames(k$comm))) +  
    ylab("Species") + 
    xlab(paste(unique(alsc$Watershed)[i], "Quads")) +
    theme(legend.position="none")
  
  #print(j)
  #print(k)
    
}

## Metrics to go along with the nestedness analysis 

c = data.frame(Watershed = colnames(k$comm), nest =print(plot(nestednodf(watershed_pres_minnow), names= T)))
d = alsc %>% group_by(Watershed) %>% 
  summarize(mean_elev = mean(Elevation..m.),
            mean_cal = mean(Ca_mgL), 
            mean_vol = mean(Volume.m3.), 
            sd_vol = sd(Volume.m3.), 
            mean_lit = mean(Littoral.Area..ha.),
            mean_ANC = mean(ANC), mean_lat = mean(LAT)) %>% full_join(c)

d %>%
  ggplot(aes( x = nest, y = mean_lat)) + geom_point() + geom_smooth(method = lm)

## Mean volume is a beautiful relationship, sd_vol is not significant
# elevation shows nothing, mean_lit shows nothing, mean_ANC shows nothing, mean_lat
summary(lm(d$mean_lit~d$nest))



### https://esajournals-onlinelibrary-wiley-com.proxy.library.cornell.edu/doi/full/10.1890/13-1424.1
re gl 
