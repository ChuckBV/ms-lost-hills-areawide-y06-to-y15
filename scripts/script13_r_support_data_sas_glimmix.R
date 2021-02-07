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
# [1]  345  344  346 3450 3440 3460

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
#             CMD     Conv       MD
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

### Report sample size
interior2 %>% 
  summarise(Mn = mean(tot_nuts),
            Min_nuts = min(tot_nuts),
            Max_nuts = max(tot_nuts))

interior2 %>% 
  arrange(tot_nuts)


write.csv(interior2,
          "./data/dmg_wndrw_interior_y06_to_y15_to_glimmix.csv",
          row.names = FALSE)


#-- 2. Output experiment 2006 and 2007 for GLIMMIX --------------------------

expt06_2yr <- interior %>% 
  mutate(Year = year(date)) %>% 
  filter(Year %in% c(2006,2007)) %>% 
  select(Year,Tier,treatment,Variety,dmg_now,tot_nuts)

expt06_2yr <- expt06_2yr %>% 
  group_by(Year,Tier,treatment) %>% 
  summarise(dmg_now = sum(dmg_now, na.rm = TRUE),
            tot_nuts = sum(tot_nuts, na.rm = TRUE)) %>% 
  mutate(pctNOW = 100*dmg_now/tot_nuts)
expt06_2yr

expt06_2yr %>% 
  group_by(treatment) %>% 
  summarise(nObs = n(),
            mn = mean(pctNOW),
            sem = se(pctNOW))

write.csv(expt06_2yr,
          "./data/expt06_2yr_windrow.csv",
          row.names = FALSE)

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
  mutate(Year = year(date)) %>% 
  filter(Year %in% c(2008,2009,2010,2011)) %>% 
  select(Year,Tier,treatment,Variety,dmg_now,tot_nuts)

unique(expt08_4yr$treatment)

### Remove conv to make this a straight factorial
expt08_4yr <- expt08_4yr %>%  # 335 obs
  filter(treatment != "Conv")

unique(expt08_4yr$treatment) 

expt08_4yr <- expt08_4yr %>% 
  group_by(Year,Tier,treatment) %>% 
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

expt08_4yr %>% 
  group_by(treatment) %>% 
  summarise(nObs = n(),
            mn = mean(pctNOW),
            sem = se(pctNOW))

expt08_4yr$treatment <- factor(expt08_4yr$treatment, levels = c("1MD","1CMD","2MD","2CMD"))

DescTools::DunnTest(pctNOW ~ treatment, data = expt08_4yr)

# Dunn's test of multiple comparisons using rank sums : holm  
# 
#           mean.rank.diff   pval    
# 1CMD-1MD          -4.375 1.0000    
# 2MD-1MD           -0.875 1.0000    
# 2CMD-1MD         -12.750 0.0394 *  
# 2MD-1CMD           3.500 1.0000    
# 2CMD-1CMD         -8.375 0.2967    
# 2CMD-2MD         -11.875 0.0567 .  
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

write.csv(expt08_4yr,
          "./data/dmg_wndrw_interior_expt08_4yr_to_sas.csv",
          row.names = FALSE)

expt08_4yr
# A tibble: 32 x 8
# Groups:   Year, Tier [32]
# Year  Tier treatment dmg_now tot_nuts disp_dens insecticide pctNOW
#   <dbl> <dbl> <fct>       <dbl>    <dbl>     <dbl> <chr>        <dbl>
# 1  2008     1 2MD            77     6797         2 No           1.13 
# 2  2008     2 2CMD           10     6742         2 Yes          0.148
# 3  2008     3 1MD           105    12228         1 No           0.859
# 4  2008     4 1CMD           57    12331         1 Yes          0.462
# 5  2008     5 1MD           303     5706         1 No           5.31 
# 6  2008     6 1CMD          170     8800         1 Yes          1.93 
# 7  2008     7 2MD           116     5338         2 No           2.17 
# 8  2008     8 2CMD           52     5352         2 Yes          0.972
# 9  2009     1 1CMD           15     4842         1 Yes          0.310
# 10  2009     2 1MD            44     5708         1 No           0.771

expt08_4yr %>% 
  group_by(insecticide,disp_dens) %>% 
  summarise(nObs = sum(!is.na(pctNOW)),
            mn = mean(pctNOW, na.rm = TRUE),
            sem = FSA::se(pctNOW))
# A tibble: 4 x 5
# Groups:   insecticide [2]
# insecticide disp_dens  nObs    mn    sem
#   <chr>           <dbl> <int> <dbl>  <dbl>
# 1 No                  1     8 1.67  0.637 
# 2 No                  2     8 0.932 0.217 
# 3 Yes                 1     8 0.644 0.197 
# 4 Yes                 2     8 0.330 0.0946

#-- 4. Output experiment 2012 to 2014 for GLIMMIX ---------------------------

expt12_3yr <- interior %>%  # 165 obs
  mutate(Year = year(date)) %>% 
  filter(Year %in% c(2013,2013,2014)) %>% 
  select(Year,Tier,treatment,Variety,dmg_now,tot_nuts)

unique(expt12_3yr$treatment)
# [1] "Conv"   "50%C"   "50%MD"  "100%C"  "100%MD"

expt12_3yr <- expt12_3yr %>%  # 130 obs
  filter(treatment != "Conv")

expt12_3yr <- expt12_3yr %>% 
  group_by(Year,Tier,treatment) %>% 
  summarise(dmg_now = sum(dmg_now, na.rm = TRUE),
            tot_nuts = sum(tot_nuts, na.rm = TRUE))

expt12_3yr <- expt12_3yr %>% 
  mutate(phero_conc = ifelse(treatment %in% c("50%MD","50%C"),"half","whole"),
         insecticide = ifelse(treatment %in% c("50%MD","100%MD"),"No","Yes"),
         pctNOW = 100*dmg_now/tot_nuts)

Desc(pctNOW ~ treatment, data = expt12_3yr)
# ----------------------------------------------------------------------------------------------------------- 
#   pctNOW ~ treatment (expt12_3yr)
# 
# Summary: 
#   n pairs: 16, valid: 16 (100.0%), missings: 0 (0.0%), groups: 4
# 
# 
# 100%C   100%MD     50%C    50%MD
# mean      0.720    1.110    0.948    1.666
# median    0.438    1.082    0.489    1.619
# sd        0.764    0.782    1.212    1.138
# IQR       0.727    1.320    1.092    1.169
# n             4        4        4        4
# np      25.000%  25.000%  25.000%  25.000%
#   NAs           0        0        0        0
# 0s            0        0        0        0
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 2.5368, df = 3, p-value = 0.4687

expt12_3yr
# A tibble: 16 x 8
# Groups:   Year, Tier [16]
# Year  Tier treatment dmg_now tot_nuts phero_conc insecticide pctNOW
#   <dbl> <dbl> <chr>       <dbl>    <dbl> <chr>      <chr>        <dbl>
# 1  2013     3 50%C           10     5124 half       Yes          0.195
# 2  2013     4 50%MD          20     5560 half       No           0.360
# 3  2013     5 100%C          11     4854 whole      Yes          0.227
# 4  2013     6 100%MD         22     5025 whole      No           0.438
# 5  2013     7 100%C          21     3233 whole      Yes          0.650
# 6  2013     8 100%MD         14     3250 whole      No           0.431
# 7  2013     9 50%C            4     3810 half       Yes          0.105
# 8  2013    10 50%MD          64     4957 half       No           1.29 
# 9  2014     1 100%MD         90     5214 whole      No           1.73 
# 10  2014     2 100%C           9     4916 whole      Yes          0.183
# 11  2014     3 50%MD          95     4878 half       No           1.95 
# 12  2014     4 50%C           38     4852 half       Yes          0.783
# 13  2014     5 100%MD         90     4879 whole      No           1.84 
# 14  2014     6 100%C          84     4610 whole      Yes          1.82 
# 15  2014     7 50%MD         100     3263 half       No           3.06 
# 16  2014     8 50%C           94     3469 half       Yes          2.71

expt12_3yr %>% 
  group_by(insecticide,phero_conc) %>% 
  summarise(nObs = sum(!is.na(pctNOW)),
            mn = mean(pctNOW, na.rm = TRUE),
            sem = FSA::se(pctNOW))
# A tibble: 4 x 5
# Groups:   insecticide [2]
# insecticide phero_conc  nObs    mn   sem
#   <chr>       <chr>      <int> <dbl> <dbl>
# 1 No          half           4 1.67  0.569
# 2 No          whole          4 1.11  0.391
# 3 Yes         half           4 0.948 0.606
# 4 Yes         whole          4 0.720 0.382

write.csv(expt12_3yr,
          "./data/dmg_wndrw_interior_expt12_3yr_to_sas.csv",
          row.names = FALSE)

#-- 5. Output experiment 2015 for GLIMMIX ---------------------------

expt15 <- interior %>%  # 82 obs
  mutate(Year = year(date)) %>% 
  filter(Year == 2015) %>% 
  select(Year,Tier,treatment,Variety,dmg_now,tot_nuts)

unique(expt15$treatment)
# [1] "EarlyC"  "EarlyMD" "StandC"  "StandMD" "Conv" 

expt15 <- expt15 %>%  # 66 obs
  filter(treatment != "Conv")

expt15 <- expt15 %>% 
  group_by(Year,Tier,treatment) %>% 
  summarise(dmg_now = sum(dmg_now, na.rm = TRUE),
            tot_nuts = sum(tot_nuts, na.rm = TRUE))

expt15 <- expt15 %>% 
  mutate(start_when = ifelse(treatment %in% c("EarlyC","EarlyMD"),"early","standard"),
         insecticide = ifelse(treatment %in% c("EarlyMD","StandMD"),"No","Yes"),
         pctNOW = 100*dmg_now/tot_nuts)

Desc(pctNOW ~ treatment, data = expt15)
# ----------------------------------------------------------------------------------------------------------- 
#   pctNOW ~ treatment (expt15)
# 
# Summary: 
#   n pairs: 8, valid: 8 (100.0%), missings: 0 (0.0%), groups: 4
# 
# 
# EarlyC  EarlyMD   StandC  StandMD
# mean      2.559    2.959    1.928   10.333
# median    2.559    2.959    1.928   10.333
# sd        0.625    1.309    1.852   13.223
# IQR       0.442    0.926    1.310    9.350
# n             2        2        2        2
# np      25.000%  25.000%  25.000%  25.000%
# NAs           0        0        0        0
# 0s            0        0        0        0
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 0.5, df = 3, p-value = 0.9189

expt15
# A tibble: 8 x 8
# Groups:   Year, Tier [8]
# Year  Tier treatment dmg_now tot_nuts start_when insecticide pctNOW
#   <dbl> <dbl> <chr>       <dbl>    <dbl> <chr>      <chr>        <dbl>
# 1  2015     1 EarlyC        137     4565 early      Yes          3.00 
# 2  2015     2 EarlyMD        93     4575 early      No           2.03 
# 3  2015     3 StandC         28     4532 standard   Yes          0.618
# 4  2015     4 StandMD        44     4476 standard   No           0.983
# 5  2015     5 EarlyC         99     4677 early      Yes          2.12 
# 6  2015     6 EarlyMD       176     4531 early      No           3.88 
# 7  2015     7 StandC         99     3058 standard   Yes          3.24 
# 8  2015     8 StandMD       608     3089 standard   No          19.7

expt15 %>% 
  group_by(insecticide,start_when) %>% 
  summarise(nObs = sum(!is.na(pctNOW)),
            mn = mean(pctNOW, na.rm = TRUE),
            sem = FSA::se(pctNOW))  
# A tibble: 4 x 5
# Groups:   insecticide [2]
# insecticide start_when  nObs    mn   sem
#   <chr>       <chr>      <int> <dbl> <dbl>
# 1 No          early          2  2.96 0.926
# 2 No          standard       2 10.3  9.35 
# 3 Yes         early          2  2.56 0.442
# 4 Yes         standard       2  1.93 1.31 

write.csv(expt15,
          "./data/dmg_wndrw_interior_expt15_to_sas.csv",
          row.names = FALSE)
