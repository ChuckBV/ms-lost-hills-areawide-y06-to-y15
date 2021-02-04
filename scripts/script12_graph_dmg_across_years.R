#===========================================================================#
# script12_graph_dmg_across_years.R
#
#  1. Damage by treatment category (huller data, 10 year graph)
#  2. Damage by variety (NP v MO, huller data)
#  3. Damage by variety (NP v MO, interior windrow data)
#  4. Damage by variety (NP v MO, interior, insecticide only, 10 yr graph)
#  5. Damage by treatment category (windrow interior data, 10 year graph) 
#
# Use figure from #5, not #1
#
#===========================================================================#

library(tidyverse)
library(lubridate)

### Use huller data as coded/formatted for output to SAS
huller_dmg <- read_csv("./data/huller_dmg_y06_to_y15_out_to_sas.csv")

huller_dmg
# A tibble: 7,262 x 10
# Block Block2 Ranch2 Variety Treatment pctNOW pctTwigborer  Year pctTotReject Trt_cat          
#  <chr>  <dbl>  <dbl> <chr>   <chr>      <dbl>        <dbl> <dbl>        <dbl> <chr>            
# 1 13-1    13.1   3440 BU      Conv        0.18       0       2007         1.46 insecticide      
# 2 13-1    13.1   3440 NP      Conv        0.61       0.07    2007         1.36 insecticide      
# 3 13-1    13.1   3440 NP      1MD         0.32       0.0021  2008        NA    mating_disruption

#-- 1. Damage by treatment category --------------------------------------

yr_avg <- huller_dmg %>% 
  group_by(Year,Trt_cat) %>% 
  summarise(nObs = sum(!is.na(pctNOW)),
            mn = mean(pctNOW, na.rm = TRUE))

### Make names succinct but prettier 
yr_avg$Trt_cat[yr_avg$Trt_cat == "both"] <- "Both"
yr_avg$Trt_cat[yr_avg$Trt_cat == "insecticide"] <- "Chem"
yr_avg$Trt_cat[yr_avg$Trt_cat == "mating_disruption"] <- "MD"

yr_avg <- yr_avg %>% 
  rename(Type = Trt_cat,
         PctNOW = mn) %>% 
  mutate(Type = factor(Type, levels = c("Chem","Both","MD"))) %>% 
  select(-nObs)

p1 <- ggplot(yr_avg, aes(fill=Type, y=PctNOW, x=as.factor(Year))) + 
  geom_bar(position="dodge", stat="identity") +
  #scale_fill_manual(values = c("#E69F00","#009E73","#CC79A7")) 
  scale_fill_manual(values = c("red","black","blue")) +
  theme_bw() + 
  xlab("Year") +
  ylab("Percent NOW Damage\n Huller sample") +
  theme(axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 10),
        legend.position = c(0.25,0.75))
p1

ggsave(filename = "y06_y15_all_prod_dmg.jpg", 
       plot = p1, device = "jpg", path = "./results", 
       dpi = 300, width = 5.83, height = 3.9, units = "in")  

#-- 2. Damage by variety (NP v MO, huller data) ---------------------------------

unique(huller_dmg$Variety)
huller_dmg <- ungroup(huller_dmg)

var_by_yr <- huller_dmg %>% 
  filter(Variety %in% c("NP","MO")) %>% 
  group_by(Year,Variety) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE))

var_by_yr$Variety <- factor(var_by_yr$Variety, levels = c("NP","MO"))

p2 <- ggplot(var_by_yr, aes(fill=Variety, y=pctNOW, x=as.factor(Year))) + 
  geom_bar(position="dodge", stat="identity") +
  #scale_fill_manual(values = c("#E69F00","#009E73","#CC79A7")) 
  scale_fill_manual(values = c("red","black","blue")) +
  theme_bw() + 
  xlab("Year") +
  ylab("Percent NOW Damage\n Huller sample") +
  theme(axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 10),
        legend.position = c(0.25,0.75))
p2

#- 3. Damage by variety (NP v MO, huller data) -------------------------------

windrow_interior <- read_csv("./data/windr") 

var_by_yr2 <- windrow_interior %>% 
  filter(Variety %in% c("NP","MO")) %>% 
  group_by(Year,Variety) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE))

var_by_yr2$Variety <- factor(var_by_yr2$Variety, levels = c("NP","MO"))

p2b <- ggplot(var_by_yr2, aes(fill=Variety, y=pctNOW, x=as.factor(Year))) + 
  geom_bar(position="dodge", stat="identity") +
  #scale_fill_manual(values = c("#E69F00","#009E73","#CC79A7")) 
  scale_fill_manual(values = c("red","black","blue")) +
  theme_bw() + 
  xlab("Year") +
  ylab("Percent NOW Damage\n interior windrow sample") +
  theme(axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 10),
        legend.position = c(0.25,0.75))
p2b

windrow_interior

Desc(pctNOW ~ Variety, data = var_by_yr2)
# ----------------------------------------------------------------------------#
#   pctNOW ~ Variety (var_by_yr2)
# 
# Summary: 
#   n pairs: 20, valid: 20 (100.0%), missings: 0 (0.0%), groups: 2
# 
# 
# NP       MO
# mean      1.006    1.060
# median    0.970    0.688
# sd        0.655    0.946
# IQR       0.861    0.690
# n            10       10
# np      50.000%  50.000%
#   NAs           0        0
# 0s            0        0
# 
# Kruskal-Wallis rank sum test:
#  Kruskal-Wallis chi-squared = 0.051429, df = 1, p-value = 0.8206

#- Damage by variety (NP v MO, interior, insecticide only, 10 yr graph) -----

var_by_yr3 <- windrow_interior %>% 
  filter(Variety %in% c("NP","MO") & Trt_cat == "insecticide") %>% 
  group_by(Year,Variety) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE))

var_by_yr3$Variety <- factor(var_by_y32$Variety, levels = c("NP","MO"))

p2c <- ggplot(var_by_yr3, aes(fill=Variety, y=pctNOW, x=as.factor(Year))) + 
  geom_bar(position="dodge", stat="identity") +
  #scale_fill_manual(values = c("#E69F00","#009E73","#CC79A7")) 
  scale_fill_manual(values = c("red","black","blue")) +
  theme_bw() + 
  xlab("Year") +
  ylab("Percent NOW Damage\n interior windrow sample") +
  theme(axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 10),
        legend.position = c(0.25,0.75))
p2c

ggsave(filename = "y06_y15_dmg_np_vs_mo_in_insecticide.jpg", 
       plot = p2c, device = "jpg", path = "./burks/results", 
       dpi = 300, width = 5.83, height = 3.9, units = "in")  


Desc(pctNOW ~ Variety, data = var_by_yr3)
# ---------------------------------------------------------------------------# 
#   pctNOW ~ Variety (var_by_yr3)
# 
# Summary: 
#   n pairs: 20, valid: 20 (100.0%), missings: 0 (0.0%), groups: 2
# 
# 
# MO       NP
# mean      1.008    1.435
# median    0.638    1.004
# sd        1.000    1.206
# IQR       0.776    1.829
# n            10       10
# np      50.000%  50.000%
#   NAs           0        0
# 0s            0        0
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 0.46286, df = 1, p-value = 0.4963

var_by_yr3

#-- 5. Damage by year with interior data ------------------------------------

yr_avg2 <- windrow_interior %>% 
  group_by(Year,Trt_cat) %>% 
  summarise(nObs = sum(!is.na(pctNOW)),
            mn = mean(pctNOW, na.rm = TRUE))

### Make names succinct but prettier 
yr_avg2$Trt_cat[yr_avg2$Trt_cat == "both"] <- "Both"
yr_avg2$Trt_cat[yr_avg2$Trt_cat == "insecticide"] <- "Chem"
yr_avg2$Trt_cat[yr_avg2$Trt_cat == "mating_disruption"] <- "MD"

yr_avg2 <- yr_avg2 %>% 
  rename(Type = Trt_cat,
         PctNOW = mn) %>% 
  mutate(Type = factor(Type, levels = c("Chem","Both","MD"))) %>% 
  select(-nObs)
View(yr_avg2)

yr_avg2 <- yr_avg2 %>% 
  filter(!is.na(Year))
View(yr_avg2)

p1b <- ggplot(yr_avg2, aes(fill=Type, y=PctNOW, x=as.factor(Year))) + 
  geom_bar(position="dodge", stat="identity") +
  #scale_fill_manual(values = c("#E69F00","#009E73","#CC79A7")) 
  scale_fill_manual(values = c("red","black","blue")) +
  theme_bw() + 
  xlab("Year") +
  ylab("Percent NOW Damage\n interior windrow sample") +
  theme(axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 10),
        legend.position = c(0.25,0.75))
p1b

ggsave(filename = "y06_y15_all_prod_dmg.jpg", 
       plot = p1b, device = "jpg", path = "./burks/results", 
       dpi = 300, width = 5.83, height = 3.9, units = "in")  

