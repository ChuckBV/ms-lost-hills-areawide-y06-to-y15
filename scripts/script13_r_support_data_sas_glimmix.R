#===========================================================================#
# script13_r_support_data_sas_glimmix.R
#
# 0. Upload and initial cleaning, interior windrow data
# 1. Output 10-year data set as C, MD, or CMD for GLIMMIX
# 2. Output experiment 2006 and 2007 for GLIMMIX (Kruskal-Wallis is suff.)
# 3. Output experiment 2008 to 2011 for GLIMMIX
# 4. Output experiment 2012 to 2014 for GLIMMIX
# 5. Output experiment 2015 for GLIMMIX
#
#===========================================================================#

library(tidyverse)
library(lubridate)
library(DescTools)
library(FSA) # for their se() function
library(Hmisc) # for describe()

#--  0. Upload and initial cleaning, interior windrow data ------------------
windrow_dmg <- read_csv("./data/dmg_wndrw_y06_to_y15.csv")
windrow_dmg # 1862 obs

windrow_dmg <- windrow_dmg %>% # 1832 obs 
  mutate(Year = year(date)) %>%
  filter(!is.na(Year))
  # 30 obs with no data and no observations, apparently merge artifact
  
windrow_dmg %>% # 0 obs
  filter(is.na(ranch))

unique(windrow_dmg$ranch)
# [1] [1]  345  344  346 3450 3440 3460

windrow_dmg <- windrow_dmg %>% 
  mutate(ranch = ifelse(ranch < 1000,10*ranch,ranch))

unique(windrow_dmg$ranch)
# [1] 3450 3440 3460

### Reduce to interior
interior <- windrow_dmg %>% 
  filter(plot == "Interior") 

unique(interior$ranch) # 957 obs
# [1] 3450 3440 3460   NA

describe(interior)
  # vars w NA: position, sample_id 

interior <- interior %>% 
  select(-c(position,sample_id))

nrow(interior)
# [1] 941

nrow(interior[complete.cases(interior), ])
#[1] 941, no NAs

### Examine variables, clean data
unique(interior$type)
# [1] "MD"   "Conv" "CMD" 

unique(interior$variety)
# [1] "NP"        "Monterrey" "Fritz"     "WC"        "Price"     "Carmel"    "Wood Col"  "Monterey" 
# [9] NA          "Ruby"      "Fr"        "Ca"        "Mo"        "Bu"        "Pr"        "Ru"       
# [17] "Mi"   

interior <- interior %>% 
  mutate(Variety = case_when(
    variety == "Bu" ~ "BU",
    variety %in% c("Ca","Carmel") ~ "CA",
    variety %in% c("Fr","Fritz") ~ "FR",
    variety == "Mi" ~ "MI",
    variety %in% c("Mo","Monterey","Monterrey") ~ "MO",
    variety == "NP" ~ "NP",
    variety %in% c("Pr","Price") ~ "PR",
    variety %in% c("Ru","Ruby") ~ "RY",
    variety %in% c("WC","Wood Col") ~ "WD",
    TRUE ~ "wtf"
  )) %>% 
  filter(Variety != "wtf") # drops 9 empty observations

### Need to merge on reps (Tiers)
interior
#   date       type  plot    treatment variety ranch block position tot_nuts inf_now dmg_now sample_id Variety
#   <date>     <chr> <chr>   <chr>     <chr>   <dbl> <dbl> <chr>       <dbl>   <dbl>   <dbl>     <dbl> <chr>  
# 1 2006-08-12 MD    Interi~ MD        NP        345   2.2 A1            523       0       0        NA NP     
# 2 2006-08-12 MD    Interi~ MD        NP        345   2.1 A2            598       4       4        NA NP    

glimpse(interior)
head(interior,2)


### Compare variable names and make them compatible
Rep_blocks <- read_csv("./data/lost_hills_arrangement.csv")

Rep_blocks
# A tibble: 33 x 4
#    Ranch  Tier Block WestToEast
#    <dbl> <dbl> <dbl>      <dbl>
# 1  3450     1   2.2          1
# 2  3450     1   2.1          2

sort(unique(Rep_blocks$Block))
# [1]  1.2  1.3  2.1  2.2  2.3  2.4 11.1 11.2 11.3 11.4 12.1 12.2 12.3 12.4 13.1 13.2 13.3 13.4 14.1 14.4 24.1
# [22] 24.2 24.3 24.4 25.1 25.3 25.4 30.1 30.2 30.3 30.4

Rep_blocks %>% 
  filter(Block == 24.4)
# Ranch  Tier Block WestToEast
#   <dbl> <dbl> <dbl>      <dbl>
# 1  3440     8  24.4          2

Rep_blocks <- Rep_blocks%>%
  rename(block = Block) %>%  # leave Ranch in cap to limit key field to block
  filter(Ranch < 4000) %>% 
  select(-Ranch)
Rep_blocks

interior # 941 obs
interior <- left_join(interior,Rep_blocks)

interior[!complete.cases(interior), ]
# A tibble: 3 x 14
# date       type  plot  treatment variety ranch block tot_nuts inf_now dmg_now  Year Variety  Tier
#   <date>     <chr> <chr> <chr>     <chr>   <dbl> <dbl>    <dbl>   <dbl>   <dbl> <dbl> <chr>   <dbl>
# 1 2006-08-24 Conv  Inte~ C         NP       3460  24.4      663       4       4  2006 NP         NA
# 2 2006-10-08 Conv  Inte~ C         Monter~  3460  24.4      530      13      10  2006 MO         NA
# 3 2006-10-08 Conv  Inte~ C         Fritz    3460  24.4      686       4       4  2006 FR         NA
  # Three cases of 24.4 in 2006 with the wrong rep number

glimpse(interior) # looks good from this view
interior %>% 
  filter(is.na(Tier)) # 0 obs--good!

interior # still 941 obs


#-- 1. Output 10-year data set as C, MD, or CMD for GLIMMIX -----------------

interior2 <- interior %>% 
  group_by(Year,Tier,type) %>% 
  summarise(dmg_now = sum(dmg_now, na.rm = TRUE),
            tot_nuts = sum(tot_nuts, na.rm = TRUE)) %>% 
  mutate(pctNOW = 100*dmg_now/tot_nuts)

Desc(interior2$pctNOW)
  # No NAs, no 0s. 97 obs

Desc(pctNOW ~ type, data = interior2)
# ----------------------------------------------------------------------------------------------------------- 
#   pctNOW ~ type (interior2)
# 
# Summary: 
#   n pairs: 97, valid: 97 (100.0%), missings: 0 (0.0%), groups: 3
# 
# 
# CMD     Conv       MD
# mean      0.988    1.923    1.845
# median    0.490    1.241    0.997
# sd        1.010    2.371    3.127
# IQR       1.171    2.126    1.533
# n            32       25       40
# np      32.990%  25.773%  41.237%
# NAs           0        0        0
# 0s            0        0        0
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 5.5496, df = 2, p-value = 0.06236


interior2 %>% 
  group_by(type) %>% 
  summarise(nObs = sum(!is.na(pctNOW)),
            mn = mean(pctNOW, na.rm = TRUE),
            sem = FSA::se(pctNOW))
# A tibble: 3 x 4
#   type   nObs    mn   sem
#   <chr> <int> <dbl> <dbl>
# 1 CMD      32 0.988 0.179
# 2 Conv     25 1.92  0.474
# 3 MD       40 1.84  0.494

interior2 <- interior2 %>% 
  ungroup() %>% 
  select(Year,Tier,type,dmg_now,tot_nuts,pctNOW)
View(interior2)


write.csv(interior2,
          "./data/dmg_wndrw_interior_y06_to_y15_to_glimmix.csv",
          row.names = FALSE)

#------------------------ STOPPED HERE        -------------------------------#

#-- 2. Output experiment 2006 and 2007 for GLIMMIX --------------------------

expt06_2yr <- interior %>% 
  mutate(Year = year(date),
         Rep = paste0("r",ranch,"_",block)) %>% 
  filter(Year %in% c(2006,2007)) %>% 
  select(Year,Rep,treatment,Variety,dmg_now,tot_nuts)

expt06_2yr <- expt06_2yr %>% 
  group_by(Year,Rep,treatment) %>% 
  summarise(dmg_now = sum(dmg_now, na.rm = TRUE),
            tot_nuts = sum(tot_nuts, na.rm = TRUE)) %>% 
  mutate(pctNOW = 100*dmg_now/tot_nuts)
expt06_2yr

Desc(pctNOW ~ treatment, data = expt06_2yr)
# -------------------------------------------------------------------------#
#   pctNOW ~ treatment (expt06_2yr)
# 
# Summary: 
#   n pairs: 43, valid: 43 (100.0%), missings: 0 (0.0%), groups: 3
# 
# 
#               C      CMD       MD
# mean      1.061    0.356    1.165
# median    0.800    0.298    0.872
# sd        0.849    0.305    0.856
# IQR       0.837    0.074    0.723
# n            21        9       13
# np      48.837%  20.930%  30.233%
# NAs           0        0        0
# 0s            0        0        0
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 10.456, df = 2, p-value = 0.005365

expt06_2yr$treatment <- factor(expt06_2yr$treatment, levels = c("MD","C","CMD"))

DescTools::DunnTest(pctNOW ~ treatment, data = expt06_2yr)
# 
# Dunn's test of multiple comparisons using rank sums : holm  
# 
#        mean.rank.diff   pval    
# C-MD        -1.084249 0.8067    
# CMD-MD     -15.846154 0.0095 ** 
# CMD-C      -14.761905 0.0095 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#-- 3. Output experiment 2008 to 2011 for GLIMMIX -----------------

expt08_4yr <- interior %>%  # 418 obs
  mutate(Year = year(date),
         Rep = paste0("r",ranch,"_",block)) %>% 
  filter(Year %in% c(2008,2009,2010,2011)) %>% 
  select(Year,Rep,treatment,Variety,dmg_now,tot_nuts)

unique(expt08_4yr$treatment)

### Remove conv to make this a straight factorial
expt08_4yr <- expt08_4yr %>%  # 335 obs
  filter(treatment != "Conv")

unique(expt08_4yr$treatment) 

expt08_4yr <- expt08_4yr %>% 
  group_by(Year,Rep,treatment) %>% 
  summarise(dmg_now = sum(dmg_now, na.rm = TRUE),
            tot_nuts = sum(tot_nuts, na.rm = TRUE))

expt08_4yr <- expt08_4yr %>% 
  mutate(disp_dens = ifelse(treatment %in% c("1MD","1CMD"),1,2),
         insecticide = ifelse(treatment %in% c("1MD","2MD"),"No","Yes"),
         pctNOW = 100*dmg_now/tot_nuts)

Desc(pctNOW ~ treatment, data = expt08_4yr)
# --------------------------------------------------------------------------#
#   pctNOW ~ treatment (expt08_4yr)
# 
# Summary: 
#   n pairs: 89, valid: 89 (100.0%), missings: 0 (0.0%), groups: 4
# 
# 
# 1CMD      1MD     2CMD      2MD
# mean      0.669    1.619    0.302    0.883
# median    0.428    0.773    0.259    0.659
# sd        0.894    2.972    0.305    0.815
# IQR       0.350    1.051    0.181    1.050
# n            22       23       22       22
# np      24.719%  25.843%  24.719%  24.719%
#   NAs           0        0        0        0
# 0s            2        0        1        1
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 10.683, df = 3, p-value = 0.01357

expt08_4yr$treatment <- factor(expt08_4yr$treatment, levels = c("1MD","1CMD","2MD","2CMD"))

DescTools::DunnTest(pctNOW ~ treatment, data = expt08_4yr)
  # Kruskal-Wallis finds no diff after multi-means test

write.csv(expt08_4yr,
          "./burks/data-intermediate/dmg_wndrw_interior_expt08_4yr_to_sas.csv",
          row.names = FALSE)

expt08_4yr %>% 
  group_by(insecticide,disp_dens) %>% 
  summarise(nObs = sum(!is.na(pctNOW)),
            mn = mean(pctNOW, na.rm = TRUE),
            sem = FSA::se(pctNOW))

#-- 4. Output experiment 2012 to 2014 for GLIMMIX ---------------------------

expt12_3yr <- interior %>%  # 165 obs
  mutate(Year = year(date),
         Rep = paste0("r",ranch,"_",block)) %>% 
  filter(Year %in% c(2013,2013,2014)) %>% 
  select(Year,Rep,treatment,Variety,dmg_now,tot_nuts)

unique(expt12_3yr$treatment)
# [1] "Conv"   "50%C"   "50%MD"  "100%C"  "100%MD"

expt12_3yr <- expt12_3yr %>%  # 130 obs
  filter(treatment != "Conv")

expt12_3yr <- expt12_3yr %>% 
  group_by(Year,Rep,treatment) %>% 
  summarise(dmg_now = sum(dmg_now, na.rm = TRUE),
            tot_nuts = sum(tot_nuts, na.rm = TRUE))

expt12_3yr <- expt12_3yr %>% 
  mutate(phero_conc = ifelse(treatment %in% c("50%MD","50%C"),"half","whole"),
         insecticide = ifelse(treatment %in% c("50%MD","100%MD"),"No","Yes"),
         pctNOW = 100*dmg_now/tot_nuts)

Desc(pctNOW ~ treatment, data = expt12_3yr)
# --------------------------------------------------------------------------#
#   pctNOW ~ treatment (expt12_3yr)
# 
# Summary: 
#   n pairs: 44, valid: 44 (100.0%), missings: 0 (0.0%), groups: 4
# 
# 
# 100%C   100%MD     50%C    50%MD
# mean      0.727    1.168    0.784    1.534
# median    0.307    0.881    0.235    1.488
# sd        0.852    0.977    1.099    1.317
# IQR       0.928    1.153    0.759    1.443
# n            11       11       11       11
# np      25.000%  25.000%  25.000%  25.000%
#   NAs           0        0        0        0
# 0s            0        0        0        0
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 5.3603, df = 3, p-value = 0.1472

expt12_3yr %>% 
  group_by(insecticide,phero_conc) %>% 
  summarise(nObs = sum(!is.na(pctNOW)),
            mn = mean(pctNOW, na.rm = TRUE),
            sem = FSA::se(pctNOW))

write.csv(expt12_3yr,
          "./burks/data-intermediate/dmg_wndrw_interior_expt12_3yr_to_sas.csv",
          row.names = FALSE)

#-- 5. Output experiment 2015 for GLIMMIX ---------------------------

expt15 <- interior %>%  # 82 obs
  mutate(Year = year(date),
         Rep = paste0("r",ranch,"_",block)) %>% 
  filter(Year == 2015) %>% 
  select(Year,Rep,treatment,Variety,dmg_now,tot_nuts)

unique(expt15$treatment)
# [1] "EarlyC"  "EarlyMD" "StandC"  "StandMD" "Conv" 

expt15 <- expt15 %>%  # 66 obs
  filter(treatment != "Conv")

expt15 <- expt15 %>% 
  group_by(Year,Rep,treatment) %>% 
  summarise(dmg_now = sum(dmg_now, na.rm = TRUE),
            tot_nuts = sum(tot_nuts, na.rm = TRUE))

expt15 <- expt15 %>% 
  mutate(start_when = ifelse(treatment %in% c("EarlyC","EarlyMD"),"early","standard"),
         insecticide = ifelse(treatment %in% c("EarlyMD","StandMD"),"No","Yes"),
         pctNOW = 100*dmg_now/tot_nuts)

Desc(pctNOW ~ treatment, data = expt15)
# --------------------------------------------------------------------------------------------------------- 
#   pctNOW ~ treatment (expt15)
# 
# Summary: 
#   n pairs: 22, valid: 22 (100.0%), missings: 0 (0.0%), groups: 4
# 
# 
# EarlyC  EarlyMD   StandC  StandMD
# mean      2.566    2.959    1.683    8.469
# median    1.753    3.195    1.585    1.014
# sd        2.145    1.402    1.881   10.276
# IQR       2.203    2.076    1.491   17.686
# n             6        6        5        5
# np      27.273%  27.273%  22.727%  22.727%
#   NAs           0        0        0        0
# 0s            0        0        0        0
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 1.4672, df = 3, p-value = 0.6899

expt15 %>% 
  group_by(insecticide,start_when) %>% 
  summarise(nObs = sum(!is.na(pctNOW)),
            mn = mean(pctNOW, na.rm = TRUE),
            sem = FSA::se(pctNOW))  
# A tibble: 4 x 5
# Groups:   insecticide [2]
# insecticide start_when  nObs    mn   sem
#   <chr>       <chr>      <int> <dbl> <dbl>
# 1 No          early          6  2.96 0.573
# 2 No          standard       5  8.47 4.60 
# 3 Yes         early          6  2.57 0.876
# 4 Yes         standard       5  1.68 0.841

write.csv(expt15,
          "./burks/data-intermediate/dmg_wndrw_interior_expt15_to_sas.csv",
          row.names = FALSE)
