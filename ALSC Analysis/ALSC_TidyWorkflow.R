library(dplyr)
library(tidyr)
library(vegan)
library(tidyverse)
library(kohonen)
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
                                "Percidae"
                                ))

# ------- Set up Data Frame ---

## This is presence absence 
## Pres/abs ---------
alsc = read.csv("ALSCDATA_fish_altered.csv", header=T)[,1:54] %>% 
  replace(!is.na(.),1)  %>%
  replace(is.na(.),0) %>%
  mutate(Richness = rowSums(.)) %>%
  cbind(read.csv("ALSCDATA_fish_altered.csv", header=T)[,56:59]) %>%
  cbind(read.csv("ALSCDATA_chem_altered.csv",header=T)) %>%
  #rename(Long = LONG..NAD83.DD.)%>%
  mutate(LONG = LONG*-1) %>%
  filter(Richness > 0)


end_spp = which(colnames(alsc)=="Volume.m3.")-1 
sp_waters = alsc[,1:end_spp] %>% colSums(.)
common_spp =which(sp_waters > 5) 

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
  
NatMin %in% names(common_spp)


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



length(unique(sums$lake_id))

class_totals %>% group_by(class) %>% summarise(num_lakes = n()) %>% mutate(num_lakes / 1116)

# # Creating an easy marker for plotting


## This removes rare species

rare = alsc[,common_spp] %>%
  mutate(Richness = rowSums(.)) %>%
  filter(Richness != 0 ) %>%
  select(-Richness)

data=sample(c(1:length(rare[,1])), size=700, replace=F)

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
nmds=metaMDS(rare[data,], k=2)
nmds$species %>% as.data.frame()  %>%
  select(MDS1, MDS2) %>% 
  as.data.frame() %>%
  ggplot(aes(x = MDS1, y = MDS2,
             label = rownames(nmds$species))) + 
  geom_text(size =4)+ 
  ggtitle("Species - averaged across sites") + 
  xlim(-.09, .05) + 
  ylim(-.05, .05)

nmds_minnow = metaMDS(minnows[,1:length(NatMin)], k = 2, try = 100)

## Mantel test -------- 
comm.dist = vegdist(rare, method = "bray")
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


fish_species = c("Margariscus nachtriebi", "Margariscus margarita","Luxilus cornutus","Chrosomus eos","Notemigonus crysoleucas","Pimephales promelas","Rhinichthys atratulus","Semotilus atromaculatus" ,"Couesius plumbeus","Rhinichthys cataractae","Hybognathus hankinsoni","Hybognathus regius","Pimephales notatus","Notropis bifrenatus", "Exoglossum maxillingua","Chrosomus neogaeus","Notropis volucellus","Semotilus corporalis")
length(fish_species)

fish_codes = c("PD", "PD","CS", "NRD","GS","FHM","BND","CC","LC","LND","BM","SM","BNM","BS","CLM","FSD","MS","FF") %>% cbind(fish_species) %>% as.data.frame() %>% rename(Species = fish_species) %>% rename(code = '.')
cbind(fish_codes, fish_species)


length(fish_codes)
species(fish_species, 
        fields = c(species_fields$habitat, species_fields$longevity, species_fields$depth, species_fields$morph, species_fields$misc)) %>% colnames()

#spawning(fish_species) ## Doesnt have a lot of info for most of the species
#preds = predators(fish_species) # may also be useful

repro = reproduction(fish_species) %>% select(SpecCode, Species, RepGuild1, RepGuild2) # has useful reproduction guilds 

general = species(fish_species, 
                  fields = c(species_fields$id,species_fields$habitat,
                             species_fields$longevity, 
                             species_fields$depth, species_fields$morph, species_fields$misc)) %>% 
  rename(spp = Species)


morpho = morphology(fish_species) %>% 
  select(SpecCode, Species, 
         BodyShapeI, BodyShapeII, 
         PosofMouth, TypeofMouth)



ecology = ecology(fish_species) %>% select(SpecCode, Species, SoftBottom, Sand, Coarse, Fine, Level, Sloping, Silt, Mud, Ooze, Detritus, Organic, HardBottom, Rocky, Rubble, Gravel, Stream, Lakes, Herbivory2, FeedingType, DietTroph, FoodTroph)

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
  select(Species, DemersPelag, LongevityWild, TL_cm, RepGuild1, RepGuild2, BodyShapeI, Herbivory2, FeedingType, FoodTroph)




## Nestedness analysis --------------------

# Remove fishless lakes 

common_spp =which(sp_waters > 5)  ## filter out rare taxa 


fish_alsc = alsc %>% 
  rownames_to_column(var = "lake_id") %>% 
  mutate(species_richness = rowSums(.)) %>% 
  filter(species_richness > 0) %>% select(ST:AW) %>% t()
nestednodf(fish_alsc)
plot(nestednodf(fish_alsc), names= T)


## Overall community nestedness
watershed_pres = alsc %>%
  pivot_longer(ST:AW, names_to ="Species", values_to =  "Presence") %>% 
  select(Species, Presence, Watershed) %>% 
  group_by(Watershed, Species) %>%
  summarize(watershed_pres = sum(Presence)) %>% 
  mutate(watershed_pres = case_when(
    watershed_pres > 0 ~ 1, 
    TRUE   ~ watershed_pres)) %>%
  mutate(watershed_pres = as.numeric(watershed_pres)) %>%
  pivot_wider(names_from = Species, values_from = watershed_pres)%>% 
  column_to_rownames(var = "Watershed") %>% t()
k = nestednodf(watershed_pres)
k$statistic
plot(nestednodf(watershed_pres), names= T)
nestednodf(watershed_pres)
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
nestednodf(watershed_pres_minnow)
### https://esajournals-onlinelibrary-wiley-com.proxy.library.cornell.edu/doi/full/10.1890/13-1424.1
