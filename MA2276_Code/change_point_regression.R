# Read in OBFL data



############################################################
library(lattice)
library(MASS)
library(dplyr)
#install.packages("pscl")
require(pscl) # alternatively can use package ZIM for zero-inflated 
library(lmtest)

library(tidyr)


## checking for zero inflation in the data --------

try = v %>% filter(Species == "ST")
length(which(try$value == 0)) / nrow(try) * 100 

try.r = try %>% mutate(value = round(value*60*60)) %>% mutate(Year = as.numeric(Year)) %>% mutate(Year = scale(Year)[,1])
try.r


# Around 40% of WS data is a zero


## We may not have to use zero inflation corrected models


## Trying a poisson GLM 


M1 = glm(value~as.numeric(Year), data = try.r, family = "poisson")
summary(M1)


## Checking for overdisperson 
E2 <- resid(M1, type = "pearson")

N  <- nrow(try)
p  <- length(coef(M1))   
sum(E2^2) / (N - p)

#install.packages("AER")
library(AER)
dispersiontest(M1)

## Now were trying a negative binommial 

M2 = glm.nb(value ~ as.numeric(Year), data= try.r)
summary(M2)
E2 <- resid(M2, type = "pearson")

N  <- nrow(try)
p  <- length(coef(M2))   
sum(E2^2) / (N - p)
## Looks like there is underdisperson 

## Well first is overdispersed and the second is underdispersed so lets try the zero inflated models


M3 <- zeroinfl(value ~ scale(Year) | scale(Year),
               dist = 'poisson',
               data = try.r)

summary(M3)
E2 <- resid(M3, type = "pearson")

N  <- nrow(try)
p  <- length(coef(M3))   
sum(E2^2) / (N - p)
diag(M3$vcov)



try.r.sub = try %>% filter(Year > 2000) %>% mutate(Year = scale(as.numeric(Year))[,1])%>% mutate(value = round(value*60*60))


M4 <- zeroinfl(value ~ scale(Year) | scale(Year),
               dist = 'negbin',
               data = try.r)

summary(M4)
E2 <- resid(M4, type = "pearson")

N  <- nrow(try)
p  <- length(coef(M4))   
sum(E2^2) / (N - p)
diag(M3$vcov)


plot(try.r$Year, try.r$value)
abline(M1) 
abline(h = 1.31, col = 2)
abline(h = 3, col = 4)
plot(try.r$Year, try.r$value)
abline(M4, col = 2)
lrtest(M3, M4)

Pred<- predict(M1,newdata = try.r, type = "response")
Pred
lines(try.r$Year, Pred, col = 3, lwd =3)



## Not broken out at yr 2000
# maybe keep at minutes 
for(i in species){
  try = v %>% filter(Species == i)
  
  
  
  try.r = try   %>%
    mutate(value = round(value*60)) %>% 
    mutate(Year = as.numeric(Year)) %>%
    mutate(Year = scale(Year)[,1])
  
  M4 <- zeroinfl(value ~ scale(Year) | scale(Year),
                 dist = 'negbin',
                 data = try.r)
  
  plot(try.r$Year, try.r$value, ylab = paste(i, "Catch per hour"))
  Pred<- predict(M4,newdata = try.r, type = "response")
  lines(try.r$Year, Pred, col = 3, lwd =3)
}

t.test(try$value[which(try$Year == 2009)], try$value[which(try$Year == 2010)])

for(i in species){
  
  
  try = v_mod %>% filter(Species == i)%>%
    mutate(value = round(value))
  
  try.yr = try %>% filter(Year > 2000)
  
  
  
  try.r = try.yr    %>% 
    mutate(Year = as.numeric(Year)) %>%
    mutate(Year = scale(Year)[,1])
  
  M4 <- zeroinfl(value ~ (Year) | (Year),
                 dist = 'negbin',
                 data = try.r)
  
  #plot(try.r$Year, try.r$value, ylab = paste(i, "Catch per hour"))
  Pred<- predict(M4,newdata = try.r, type = "response")
  #lines(try.r$Year, Pred, col = 3, lwd =3)
  
  
  h = ggplot() + 
    geom_point(aes(x = as.numeric(try$Year), y = try$value, col = as.character(try$color))) + 
    geom_line(aes(x = as.numeric(try.yr$Year), Pred), size = 2) + 
    ylab("CPUE / Minute") + 
    labs(color = paste(i ,"Changepoint"))
  print(h)
}


## So it looks like the negative binomial is still a little bit overdispersed, but it is the most reasonable of all the different models aka the closest to 1. 

lm.model = lm(value~scale(Year), data = try.r)
summary(lm.model)
abline(lm.model, col = 2)


## Working on piecewise regressions -----------------------------------
# CPUE in minutes
v = CPUE.w.sec %>% 
  mutate(y_s = rownames(CPUE.w.sec)) %>%
  pivot_longer(1:length(species),
               names_to = "Species") %>%
  separate(y_s, 
           into = c("Year", "site")) %>%
  unite("ID", 
        c(site:Species), 
        sep = "_", 
        remove = F) %>%
  dplyr::select(-site) %>%
  mutate(value = value * 60 * 60 )

# Colorblind pallete
cbbPalette <- c("#000000",  "#56B4E9", "#D55E00","#009E73", "#F0E442", "#0072B2",  "#CC79A7","#E69F00")

for(i in species){
  
  
  # Set up data frame
  x = v %>% filter(Species == i) %>%
    mutate(value = as.numeric(value)) %>% 
    dplyr::select(-Species) %>%
    mutate(value = log10(value+1)) %>%
    pivot_wider(values_from = value,
                names_from = ID) %>%
    replace(is.na(.), 0) %>%
    dplyr::select(-Year) %>%
    as.matrix()
  rownames(x) = rownames(CPUE.w.sec.a)
  
  # Run changepoint analysis 
  output = e.divisive(x, 
                      R = 499, 
                      alpha = 1, 
                      min.size = 2,
                      sig.lvl = .05)
  
  # Format data
  dat = data.frame(Year = rownames(CPUE.w.sec.a), 
                   color = output$cluster)
  v_mod = left_join(v,dat)
  
  ## Regressions 
  try = v_mod %>% filter(Species == i)%>%
    mutate(value = round(value))
  h = ggplot() + 
    geom_point(aes(x = as.numeric(try$Year), 
                   y = try$value,
                   col = as.character(try$color)),alpha = .05)
  ## Adding in loop for regressions
  
  listy = list()
  pred_list = list()
  itter_list = list()
  itter_list[[2]] = c(NA, NA, NA)
  itter_list[[3]] = c(NA, NA, NA)
  itter_list[[4]] = c(NA, NA, NA)
  
  for(z in 1:length(unique(v_mod$color))){
 
    h = ggplot() + 
      geom_point(aes(x = as.numeric(try$Year), 
                     y = try$value,
                     col = as.character(try$color)), alpha = .1)

   
    try.yr = try %>% filter(Year %in% 
                              unique((v_mod %>% filter(color == z))$Year))
    
    
    
    try.r = try.yr    %>% 
      mutate(Year = as.numeric(Year)) %>%
      mutate(Year = scale(Year)[,1])
    
    
  
    
    
    try(M4 <- zeroinfl(value ~ (Year) | (Year),
                   dist = 'negbin',
                   data = try.r))
    
  
    Pred<- predict(M4,newdata = try.r, type = "response")
    #lines(try.r$Year, Pred, col = 3, lwd =3)
    
    
    itter_list[[z]] = data.frame(Year = try.yr$Year, pred = Pred, cluster = z )
    
    
    


  }
  
  
  data = rbind(itter_list[[1]], itter_list[[2]], itter_list[[3]], itter_list[[4]])  
  
  p = h + geom_line(aes(x = as.numeric(data$Year), y = data$pred)) + 
    labs(color = paste(i, "Cluster")) + 
    ylab(paste(i, "CPUE / Hour") )+ xlab("Year") + 
    theme(legend.position="none") + 
    xlim(1997, 2021)
  print(p)
 
  
}



# old regressions plot
h = ggplot() + 
  geom_point(aes(x = as.numeric(try$Year), y = try$value, col = as.character(try$color))) + 
  geom_line(aes(x = as.numeric(try.yr$Year), Pred), size = 2) + 
  ylab("CPUE / Minute") + 
  labs(color = paste(i ,"Changepoint"))
print(h)
# Plots chanepoing plot
pl = v_mod %>%
  filter(Species == i)%>% 
  ggplot(aes(x = as.numeric(Year), 
             y = value)) +
  geom_point(aes(col = as.character(color)), alpha = .2, size = 2) + 
  #geom_text(x= 2010, y= y_point, label="Scatter plot") + 
  geom_smooth(method = "lm") + 
  #xlab("Year") + 
  ylab(paste(i,"CPUE")) +
  xlim(1998, 2021) + 
  theme(text = element_text(size = 16)) + 
  theme(legend.position="none") + 
  theme(axis.title.x = element_blank()) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_colour_manual(values=cbbPalette)

print(pl)


