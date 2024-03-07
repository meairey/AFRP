library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)

### Notes --------------------------------------------------------------
# I checked the new measurement MAIDs agaist the old data sheets that haven't been messed with and it looks like its all good. 


##----------------------------------------------------------------------

setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP")
## Measurement Sheet ---------------

m = read.csv("MA2276_Code/Data/Measurement_FINAL_Rscript.csv") 


## Sample Sheet --------------------
# subset columns
s = read.csv("MA2276_Code/Data/SAMP_NEW_FINAL_MAID.csv.",header=T) %>%
  select(YSAMP_N,MA_ID)
# expanded columns    
s_whole = read.csv("MA2276_Code/Data/SAMP_NEW_FINAL_MAID.csv.",header=T) %>%
  select(YSAMP_N, MA_ID,WATER, DATE_COL, TIME_END)

## Fixing the y_samp# ------------------------
new = left_join(m,s) %>%
  select(YSAMP_N, everything())
# Appropriate file is Measurement_2021_Updated.csv this should be the new Measurement 2021
#write.csv(new, file = "Measurement_2021_Updated_v2.csv")


## Fixing the Y_samps on the dissection spreadsheet 
d = read.csv("MA2276_Code/Data/dissection_samples_updated.csv") # read in original file



dis = left_join(d, s_whole) %>%
  select(MA_ID, everything()) %>%
  select(-NEW_SAMP)



d = read.csv("MA2276_Code/Data/dissection_samples_updated.csv") %>% 
  left_join(read.csv("MA2276_Code/Data/SAMP_NEW_FINAL_MAID.csv.",header=T)) %>%
  select(MA_ID, everything()) %>%
  select(-YSAMP_N) %>%
  left_join(s) %>%
  replace_na(list(TISS_SAMP = 1, OTOLITH = 1, FIN = 1, GUT = 1)) %>%
  select(NEW_SAMP, everything(),-X.1, -X) %>%
  select(-MA_ID, -LENGTH, - SPECIES, - COMMENTS) # These are removed for makign the measurement sheet 
  



# Creating FISH_N
FISH_N = full_join(m,s_whole)  %>%
 select(WATER, MA_ID, DATE_COL, TIME_END, GEN_N, SPECIES, WEIGHT, LENGTH) %>%
  filter(GEN_N != "") %>%
  unite("TIME_DAY", c(DATE_COL, TIME_END), sep = " ", remove= F) %>%
  mutate(TIME_DAY = as.character(TIME_DAY)) %>%
  mutate(TIME_DAY = mdy_hm(TIME_DAY, tz = "EST")) %>%
  group_by(WATER,DATE_COL) %>%
  arrange(TIME_DAY) %>%
  group_by(DATE_COL, SPECIES) %>%
  mutate(fish_initialize = 1 ) %>%
  mutate(fish_num = cumsum(fish_initialize)) %>%
  mutate(GEAR = "MWT") %>%
  separate(DATE_COL, into = c("m","d","y")) %>%
  mutate(add_0 = 0) %>%
  unite("DATE_COL", c(add_0, m, d,y),sep="") %>%
  mutate(fish_num = str_pad(fish_num, width = 3, pad = "0")) %>%
  unite("FISH_N", SPECIES, WATER, DATE_COL, GEAR, fish_num,
        remove = F) %>%
  ungroup() %>%
  group_by(WATER, DATE_COL) %>%
  mutate(gen_numb = cumsum(fish_initialize)) %>%
  mutate(gen_numb = str_pad(gen_numb, width = 3, pad = "0")) %>%
  unite("GEN_WHOLE", SPECIES, WATER, DATE_COL, GEAR, gen_numb, remove = F) %>%
  ungroup() %>%
  select(GEN_N, FISH_N, GEN_WHOLE)

## Adding FISH_N to measurement spreadsheet

# This is the full measurement data sheet
M_FULL = left_join(m, FISH_N) %>%
  select(-OTOLITH, -GUT, - TISS_SAMP, -FIN, -WEIGHT) %>%
  left_join(d, by = "GEN_N") %>%
  select(FISH_N,GEN_WHOLE,colnames(m)) %>%
  left_join(s) %>%
  select(YSAMP_N, GEN_WHOLE,everything()) %>%
  mutate(WHOLE_FISH = NA) 
  
write.csv(M_FULL, file = "Measurement_FINAL_v2.csv")
series = c(1:554)
which(series%in%cat$GEN_N ==FALSE)
M_FULL %>% group_by(GEN_N) %>%
  summarize(n = n()) %>%
  filter(n > 2)

cat = M_FULL %>% filter(GEN_N >=1) %>%
  select(GEN_N, FISH_N) %>%
  mutate(GEN_Nx = as.numeric(GEN_N)) %>%
  arrange(GEN_N)



# Setting up D_SAMPs 

d_samp_matrix = s_whole %>%
  mutate(initialize = 1) %>%
  group_by(WATER, DATE_COL) %>%
  mutate(D_SAMP = cumsum(initialize)) %>%
  mutate(D_SAMP = str_pad(D_SAMP, width = 3, pad = "0")) %>%
  mutate(D_SAMP = as.character(D_SAMP)) %>%
  separate(DATE_COL, into = c("m","d","y"))

write.csv(d_samp_matrix, file = "DSAMP.csv")


### Setting up effort 
fish = read.csv("MA2276_Code/Data/Measurement_2021_Updated.csv")
sample = read.csv("MA2276_Code/Data/SAMP_NEW_FINAL_MAID.csv") %>%
  mutate(DATE_COL = mdy(DATE_COL)) %>%
  mutate(DATE_SET = mdy(DATE_SET)) %>%
  mutate(DAYS = DATE_COL - DATE_SET) %>%
  separate(TIME_START, into = c("hour","min")) %>%
  mutate(min = round(as.numeric(min)/60*100)) %>%
  unite(TIME_START, c(hour, min), sep = ".") %>%
  separate(TIME_END, into = c("hour","min")) %>%
  mutate(min = round(as.numeric(min)/60*100)) %>%
  unite(TIME_END, c(hour, min), sep = ".") %>%
  mutate(TIME_START = as.numeric(TIME_START)) %>%
  mutate(TIME_END = as.numeric(TIME_END)) %>%
  mutate(HOURS = TIME_END - TIME_START) %>% 
  mutate(DAYS = case_when(
    .$DAYS == 1 ~ 24,
    .$DAYS == 0 ~ 0)) # for some reason this line of code appears to break the pipeline?
sample = sample %>% mutate(
  effort_soak = DAYS + HOURS
)

write.csv(sample, "effort_matrix.csv")

##write.csv(dissection_samples,"dissection_samples_updated.csv")


## GLN samples
gln_sample = read.csv("Data/GLN_SAMPLE.csv") %>% 
  filter(MONTH > 1)
gln_measurement = read.csv("Data/GLN_MEASUREMENT.csv")

comb = left_join(gln_measurement, gln_sample) %>%
  filter(GEN_N > 1) %>%
  select(WATER, DATE_COL, TIME_END, GEN_N, SPECIES) %>%
  group_by(WATER,DATE_COL) %>%
  arrange(TIME_END) %>%
  group_by(DATE_COL, SPECIES) %>%
  mutate(fish_initialize = 1 ) %>%
  mutate(fish_num = cumsum(fish_initialize))  %>%
  separate(DATE_COL, into = c("m","d","y")) %>%
  unite("DATE_COL",c(m,d,y), sep = "") %>%
  mutate(GEAR = "GLN") %>%
  mutate(fish_num = str_pad(fish_num, width = 3, pad = "0")) %>%
  unite("fish", SPECIES, WATER, DATE_COL, GEAR, fish_num, remove = F) %>%
  ungroup() %>%
  group_by(WATER, DATE_COL) %>%
  mutate(gen_numb = cumsum(fish_initialize)) %>%
  mutate(gen_numb = str_pad(fish_num, width = 3, pad = "0")) %>%
  unite("GEN_WHOLE", SPECIES, WATER, DATE_COL, GEAR, gen_numb, remove = F) %>%
  select(fish, GEN_WHOLE, GEN_N)
  
  

gln_m_update = left_join(gln_measurement, comb) %>%
  select(fish, GEN_WHOLE, everything())
write.csv(gln_m_update, file = "gln_measurement_update.csv")

m$MA_ID %>% parse_number() %>% unique()
c(1:629) %in% (m$MA_ID %>% parse_number() %>% unique())

comb = comb %>% ungroup() %>% select(GEN_WHOLE, GEN_N,fish) %>%
  rename(FISH_N = fish)


print = M_FULL %>% select(GEN_WHOLE, GEN_N, FISH_N) %>% filter(GEN_N > 0) %>%
  mutate(GEN_N = as.numeric(GEN_N)) %>%
  full_join(comb) %>%
  arrange(GEN_N)

write.csv(print, "tube_lables.csv")

