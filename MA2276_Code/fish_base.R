library(rfishbase)
library(tidyr)
library(dplyr)
#install.packages("rfishbase")

## Important 
# There are 270 species of minnnows in USA, 265 of these are native 
# ## There are 667 species of minnows world wide
# There are 6 species of cyprinid in the USA, 0 of these are native 
# There are 220 specie of percidae 
# There are 68 specie of suckers 
# There are 35 species of trout
# 

global_minnows = species_list(Family = "Leuciscidae")
length(global_minnows)

USA_minnows = country(species_list(Species = 
                                     species_list(Family = "Leuciscidae"))) %>%
  as.data.frame()  %>% 
  filter(C_Code=="840")


cat = USA_minnows %>% select(Species) %>% 
  mutate(sep = " OR ") %>% 
  unite("cat", Species:sep, sep = " ")


cat[2,] = "OR"
write.table(as.character(cat), "trying.txt", sep = " OR ")

write.csv(cat, "trying.csv")

USA_minnows %>% select(Bait, Game, Species, Status, Threatened) %>% 
  filter(Bait == 1)



length(unique(USA_minnows$Species))
length(unique(USA_minnows %>% filter(Status %in% c("native", "endemic")))$Species)


USA_cyprinidae = country(species_list(Species = 
                                     species_list(Family = "Cyprinidae"))) %>%
  as.data.frame()  %>% 
  filter(C_Code=="840")

length(unique(USA_cyprinidae %>% filter(Status %in% c("native", "endemic")))$Species)




USA_cypriniformes = country(species_list(Species = 
                                           species_list(Order = "Cypriniformes"))) %>%
  as.data.frame()  %>% 
  filter(C_Code=="840") 

USA_cypriniformes %>% filter(Status %in% c("native", "endemic")) %>% unique()
