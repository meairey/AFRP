
library(ecp)
library(dplyr)
library(tidyr)


v = read.csv("PS_changepoint_data.csv") %>% 
  mutate(Year = as.character(Year))

## Change point using multiple sites (column  'ID') a year a variables  --------- 
for(i in "PS"){
 
  # Set up data frame
  x = v %>% filter(Species == i) %>%
    mutate(CPUE = as.numeric(CPUE)) %>% 
    select(-Species) %>%
    mutate(CPUE = log10(CPUE+1)) %>%
    pivot_wider(values_from = CPUE,
                names_from = ID) %>%
    replace(is.na(.), 0) %>%
    select(-Year) %>%
    as.matrix()
  
  rownames(x) = unique(v$Year)
  
  # Run changepoint analysis -------- Its just this one line of code
  output = e.divisive(x, 
                      R = 499, 
                      alpha = 1, 
                      min.size = 2,
                      sig.lvl = .05)
  
  # Format data
  dat = data.frame(Year = unique(v$Year), 
                   color = output$cluster)
  v_mod = left_join(v,dat)
  
  # Linear model to overlap on plots
  lin.reg = summary(lm(v_mod$CPUE~as.numeric(v_mod$Year)))
  p_value = round(lin.reg$coefficients[2,4],4)
  r_2 = round(lin.reg$adj.r.squared,4)
  y_point = max(x) 
  # CB pallete
  cbbPalette <- c("#000000",  "#56B4E9", "#D55E00","#009E73", "#F0E442", "#0072B2",  "#CC79A7","#E69F00")
  # Plots
  pl = v_mod %>%
    filter(Species == i)%>% 
    ggplot(aes(x = as.numeric(Year), 
               y = CPUE)) +
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
}
