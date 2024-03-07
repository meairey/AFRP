### Libraries --------------

library(dplyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ecp)

# Load in data and prep for analysis -------
x = read.csv("MA2276_Code/LML_community_files/LML_SMB_linkage_data.csv")

x = x[-6,]
#x =  x %>% pivot_longer(2:length(x[1,]),names_to = "Allele", values_to = "Value") %>% filter(Value < 4.001) %>% pivot_wider(names_from = Allele, values_from = Value) 


## Data for cluster analysis: remove year
cluster_data = x %>% select(-year)

## Clean data frame with unique years 
years = x %>% select(year) %>% unique() %>% as.vector()

## Run changepoint analysis --------------

output = e.divisive(cluster_data, 
                    R = 499, # iterations to run
                    alpha = 1, # looks for change in variance + mean
                    min.size = 2, # minimum number of years between any two change points must be 2yrs
                    sig.lvl = .05) # significance cutoff for a change point 

## Visualization --------------
## Create dataframe to join with graph data 
cluster_year = cbind(year = years$year,cluster = output$cluster) %>% as.data.frame()


## Graph results 
x %>% 
  pivot_longer(2:length(x[1,]),names_to = "Allele", values_to = "Value") %>% 
  left_join(cluster_year) %>%
  ggplot(aes(x = as.factor(year),
             y = Value,
             col = as.factor(cluster))) +
  theme_minimal() + 
  geom_jitter() + xlab("Year") + ylab("Allelic Richness") + 
  labs(col = "Cluster") + 
  theme(axis.text.x = element_text(angle = 90))

