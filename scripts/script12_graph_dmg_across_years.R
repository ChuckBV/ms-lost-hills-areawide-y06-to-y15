#===========================================================================#
# script12_graph_dmg_across_years.R
#
#        Sections 1& 2 not used, removed
#  (1. Damage by treatment category, huller data, 10 year graph)
#  (2. Damage by variety, NP v MO, huller data)
#  3. Damage by variety (NP v MO, interior windrow data)
#  4. Damage by variety (NP v MO, interior, insecticide only, 10 yr graph)
#  5. Damage by treatment category (windrow interior data, 10 year graph) 
#
# Use figure from #5, not #1
#
#===========================================================================#

library(tidyverse)
library(lubridate)
library(DescTools)

### Use huller data as coded/formatted for output to SAS
#huller_dmg <- read_csv("./data/huller_dmg_y06_to_y15_out_to_sas.csv")

#-- 1. Damage by treatment category --------------------------------------

  # Removed

#-- 2. Damage by variety (NP v MO, huller data) ---------------------------------

# Removed

#- 3. Damage by variety (NP v MO, windrow interior data) -------------------------------

windrow_interior <- read_csv("./data/windrow_interior_dmg_y06_to_y15_out_to_sas.csv") 

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
        legend.position = c(0.15,0.75))
p2b

ggsave(filename = "y06_y15_windrow_interior_dmg.jpg", 
       plot = p1, device = "jpg", path = "./results", 
       dpi = 300, width = 5.83, height = 3.9, units = "in")  

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
# NAs           0        0
# 0s            0        0
# 
# Kruskal-Wallis rank sum test:
#  Kruskal-Wallis chi-squared = 0.051429, df = 1, p-value = 0.8206

#- Damage by variety (NP v MO, interior, insecticide only, 10 yr graph) -----

var_by_yr3 <- windrow_interior %>% 
  filter(Variety %in% c("NP","MO") & Trt_cat == "insecticide") %>% 
  group_by(Year,Variety) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE))

var_by_yr3$Variety <- factor(var_by_yr3$Variety, levels = c("NP","MO"))

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
        legend.position = c(0.15,0.75))
p2c

ggsave(filename = "y06_y15_dmg_np_vs_mo_in_insecticide.jpg", 
       plot = p2c, device = "jpg", path = "./results", 
       dpi = 300, width = 5.83, height = 3.9, units = "in")  


Desc(pctNOW ~ Variety, data = var_by_yr3)
# --------------------------------------------------------------------------------------------------- 
#   pctNOW ~ Variety (var_by_yr3)
# 
# Summary: 
#   n pairs: 20, valid: 20 (100.0%), missings: 0 (0.0%), groups: 2
# 
# 
#              NP       MO
# mean      1.435    1.008
# median    1.004    0.638
# sd        1.206    1.000
# IQR       1.829    0.776
# n            10       10
# np      50.000%  50.000%
# NAs           0        0
# 0s            0        0
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 0.46286, df = 1, p-value = 0.4963


var_by_yr3 %>% 
  group_by(Variety) %>% 
  summarise(nObs = n(),
            mn = mean(pctNOW),
            sem = se(pctNOW))

t.test(pctNOW ~ Variety, data = var_by_yr3, var.equal = TRUE)

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

ggsave(filename = "y06_y15_all_windrow_internal_dmg.jpg", 
       plot = p1b, device = "jpg", path = "./results", 
       dpi = 300, width = 5.83, height = 3.9, units = "in")  

### Overall average by year
yr_avg2 %>% 
  group_by(Year) %>% 
  summarise(PctNOW = mean(PctNOW)) %>% 
  arrange(PctNOW)
# A tibble: 10 x 2
# Year PctNOW
#   <dbl>  <dbl>
# 1  2010  0.355
# 2  2013  0.446
# 3  2007  0.468
# 4  2011  0.594
# 5  2006  1.20 
# 6  2009  1.31 
# 7  2008  1.59 
# 8  2014  2.15 
# 9  2012  2.44 
# 10  2015  5.40 

