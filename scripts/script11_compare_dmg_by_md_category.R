#===========================================================================#
# script11_compare_dmg_by_md_category.R
#
# 1. Huller damage by treatment type
# 2. Edge damage by treatment type
#
#
# At level of non-parametric test, shows NOW_dmg is: md = insecticide > both
# NB as first written for non-parm, different varieties within the same level
# of year*rep are separate experimental units
#
# Demonstrated with huller data, not tried with windrow data
#
#===========================================================================#

### Load libraries
library(tidyverse)
library(lubridate)
library(DescTools) # for Desc, DunnTest
library(FSA) # for dunnTest and se
#library(rcompanion) # for cldList

# block_problems <- function(x){
#   y <- if_else(x == 11.4,11.3,
#                if_else(x == 25.2,25.1,x))
#   return(y)
# }

#-- 1. Huller damage by treatment type ------------------------------------

huller_dmg <- read_csv("./data/dmg_huller_y06_to_y15.csv")

### Treatments and categories
trts <- unique(huller_dmg$Treatment)
trts
# 
(chem <- trts[c(1,4)])
# [1] "C"    "Conv"
(md <- trts[c(3,5,7,9,11,14,15)])
# [1] "MD"      "1MD"     "2MD"     "50%MD"   "100%MD"  "EarlyMD" "StandMD"
(both <- trts[c(2,6,8,10,12,13,16)])
# [1] "CMD"    "1CMD"   "2CMD"   "50%C"   "100%C"  "EarlyC" "StandC"
# 
huller_dmg <- huller_dmg %>% 
  mutate(Trt_cat = case_when(
        Treatment %in% chem ~ "insecticide",
        Treatment %in% md ~ "mating_disruption",
        Treatment %in% both ~ "both",
        TRUE ~ "wtf"
   )) # filter finds no wtf
# 
huller_dmg %>% 
  filter(Trt_cat == "wtf")

### NB Two-part process. First char to numerical block codes, then block
### code to rep blocks
block_code_lookup <- read_csv("./data/huller_block_code_lookup.csv")

### Use info from x to fix Huller Damage file
huller_dmg <- right_join(block_code_lookup,huller_dmg)

huller_dmg %>% 
  filter(is.na(Block2)) %>% 
  group_by(Year) %>% 
  summarise(nObs = n()) # 473 records, all from 2008

### Make fake block number so treatment takes precedence
huller_dmg$Block2[is.na(huller_dmg$Block2)] <- 1.0

sort(unique(huller_dmg$Block2))

### Need to get rep data
Rep_blocks <- read_csv("./data/lost_hills_arrangement.csv") %>% 
  rename(Ranch2 = Ranch, 
         Block2 = Block)


huller_dmg <- left_join(huller_dmg,Rep_blocks)

### Examine cases of NA in Tier
x <- huller_dmg %>% 
  filter(is.na(Tier)) %>% 
  group_by(Year,Ranch2,Block2) %>% 
  summarise(nObs = n())
x
  # 437 cases from 2008, Block2 = 1 (weight tickets)

huller_dmg
# A tibble: 7,262 x 12
#   Block Block2 Ranch2 Variety Treatment pctNOW pctTwigborer  Year pctTotReject Trt_cat  Tier
#   <chr>  <dbl>  <dbl> <chr>   <chr>      <dbl>        <dbl> <dbl>        <dbl> <chr>   <dbl>
# 1 24-1    24.1   3440 FR      C          1.02        0       2006           NA insect~     7
# 2 24-1    24.1   3440 FR      C          2.28        0       2006           NA insect~     7

### Output huller_dmg to sas
write.csv(huller_dmg,
          "./data/huller_dmg_y06_to_y15_out_to_sas.csv", 
          row.names = FALSE)

hdmg2 <- huller_dmg %>% 
  group_by(Year,Trt_cat,Tier,Variety,) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE))
hdmg2

Desc(pctNOW ~ Trt_cat, data = hdmg2)
# --------------------------------------------------------------------------------------------------- 
#   pctNOW ~ Trt_cat (hdmg2)
# 
# Summary: 
#   n pairs: 301, valid: 301 (100.0%), missings: 0 (0.0%), groups: 3
# 
# 
# both        insecticide  mating_disruption
# mean                0.829              1.484              1.340
# median              0.303              0.560              0.601
# sd                  1.619              3.147              2.445
# IQR                 0.641              1.124              1.090
# n                     104                 89                108
# np                34.551%            29.568%            35.880%
# NAs                     0                  0                  0
# 0s                      1                  0                  0
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 19.104, df = 2, p-value = 7.107e-05

hdmg2$Trt_cat <- factor(hdmg2$Trt_cat,
                        levels = c("insecticide","mating_disruption","both"))

PT <- DescTools::DunnTest(pctNOW ~ Trt_cat, data = hdmg2, method = "bonferroni")
PT
# 
# Dunn's test of multiple comparisons using rank sums : bonferroni  
# 
#                               mean.rank.diff    pval    
# mating_disruption-insecticide      -6.436278 1.00000    
# both-insecticide                  -49.314661 0.00026 ***
# both-mating_disruption            -42.878383 0.00101 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#-- 2. Compare interior damage ------------------------------------------------

windrow_dmg <- read_csv("./data/dmg_wndrw_y06_to_y15.csv")
windrow_dmg
# A tibble: 1,862 x 12
#   date       type  plot     treatment variety ranch block position tot_nuts inf_now dmg_now sample_id
#   <date>     <chr> <chr>    <chr>     <chr>   <dbl> <dbl> <chr>       <dbl>   <dbl>   <dbl>     <dbl>
# 1 2006-08-12 MD    Interior MD        NP        345   2.2 A1            523       0       0        NA
# 2 2006-08-12 MD    Edge     MD        NP        345   2.2 E1            524       0       0        NA
# 3 2006-08-12 MD    Interior MD        NP        345   2.1 A2            598       4       4        NA

### Need a Year variable
windrow_dmg$Year <- year(windrow_dmg$date)

windrow_dmg <- windrow_dmg %>% 
  mutate(pctNOW = 100*dmg_now/tot_nuts) 

windrow_interior <- windrow_dmg %>%  # equivalent to y from script 10
  filter(plot == "Interior") %>%
  mutate(ranch = 10*ranch) %>% 
  group_by(Year,ranch,block,treatment,variety) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE))

windrow_interior
# A tibble: 794 x 6
# Groups:   Year, ranch, block, treatment [280]
# Year ranch block treatment variety   pctNOW
#   <dbl> <dbl> <dbl> <chr>     <chr>      <dbl>
# 1  2006  3440  24.1 C         Fritz      1.39 
# 2  2006  3440  24.1 C         Monterrey  1.26 

#windrow_edge$block <- block_problems(windrow_edge$block)
windrow_interior %>% # changes 3 instances of 11.4
  filter(block %in% c(11.4,25.2)) # comes up 0

### Very important--treatment categories
trts2 <- sort(unique(windrow_interior$treatment))
trts2
# [1] "100%C"   "100%MD"  "1CMD"    "1MD"     "2CMD"    "2MD"     "50%C"    "50%MD"   "C"       "CMD"     "Conv"    "EarlyC"  "EarlyMD"
# [14] "MD"      "StandC"  "StandMD"

setequal(trts,trts2)
# [1] TRUE
# Warning messages:
#   1: Unknown or uninitialised column: `Block2`. 
# 2: Unknown or uninitialised column: `Block2`. 
### So we can use code from huller_damage for categories (lines 43-56)

chem <- c("C","Conv")
md <- c("MD","1MD","2MD","50%MD","100%MD","EarlyMD","StandMD")
both <- c("CMD","1CMD","2CMD","50%C","100%C","EarlyC","StandC")

windrow_interior <- windrow_interior %>% 
  mutate(Trt_cat = case_when(
    treatment %in% chem ~ "insecticide",
    treatment %in% md ~ "mating_disruption",
    treatment %in% both ~ "both",
    TRUE ~ "wtf"
  )) # filter finds no wtf

### Clean up duplicate names for varieties
windrow_interior <- windrow_interior %>% 
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
  )) 

windrow_interior <- windrow_interior %>%
  filter(variety != "wtf") # removes 5 records w NAs in all vars that matter

windrow_interior
# A tibble: 789 x 8
# Groups:   Year, ranch, block, treatment [275]
#    Year ranch block treatment variety   pctNOW Trt_cat     Variety
#    <dbl> <dbl> <dbl> <chr>     <chr>      <dbl> <chr>       <chr>  
# 1  2006  3440  24.1 C         Fritz      1.39  insecticide FR     
# 2  2006  3440  24.1 C         Monterrey  1.26  insecticide MO  

### Compare variable names and make them compatible
Rep_blocks <- read_csv("./data/lost_hills_arrangement.csv")

Rep_blocks
# A tibble: 33 x 4
#    Ranch  Tier Block WestToEast
#    <dbl> <dbl> <dbl>      <dbl>
# 1  3450     1   2.2          1
# 2  3450     1   2.1          2

Rep_blocks <- Rep_blocks%>%
  rename(ranch = Ranch,
         block = Block)

windrow_interior <- left_join(windrow_interior,Rep_blocks)
windrow_interior
# A tibble: 789 x 10
# Groups:   Year, ranch, block, treatment [275]
# Year ranch block treatment variety   pctNOW Trt_cat     Variety  Tier WestToEast
#    <dbl> <dbl> <dbl> <chr>     <chr>      <dbl> <chr>       <chr>   <dbl>      <dbl>
# 1  2006  3440  24.1 C         Fritz      1.39  insecticide FR          7          2
# 2  2006  3440  24.1 C         Monterrey  1.26  insecticide MO          7          2


### Output windrow_edge to sas
write.csv(windrow_interior,
          "./data/windrow_interior_dmg_y06_to_y15_out_to_sas.csv", 
          row.names = FALSE)


interior2 <- windrow_interior %>% 
  group_by(Year,Trt_cat,Tier,Variety,) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE))
interior2

Desc(pctNOW ~ Trt_cat, data = interior2)
# --------------------------------------------------------------------------------------------------- 
#   pctNOW ~ Trt_cat (interior2)
# 
# Summary: 
#   n pairs: 295, valid: 292 (99.0%), missings: 3 (1.0%), groups: 3
# 
# 
# both        insecticide  mating_disruption
# mean                0.930              1.957              1.928
# median              0.374              0.730              0.907
# sd                  1.405              4.311              5.359
# IQR                 0.823              1.556              1.774
# n                     101                 85                106
# np                34.589%            29.110%            36.301%
# NAs                     1                  0                  2
# 0s                      9                  3                  7
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 15.739, df = 2, p-value = 0.0003823

interior2$Trt_cat <- factor(interior2$Trt_cat,
                        levels = c("mating_disruption","insecticide","both"))

PT <- DescTools::DunnTest(pctNOW ~ Trt_cat, data = interior2, method = "bonferroni")
PT
# 
# Dunn's test of multiple comparisons using rank sums : bonferroni  
# 
#                               mean.rank.diff   pval    
# insecticide-mating_disruption      -1.221865 1.0000    
# both-mating_disruption            -41.737997 0.0011 ** 
# both-insecticide                  -40.516133 0.0033 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#-- 3. Compare edge damage ------------------------------------------------

windrow_edge <- windrow_dmg %>%  # equivalent to y from script 10
  filter(plot == "Edge") %>%
  mutate(ranch = 10*ranch) %>% 
  group_by(Year,ranch,block,treatment,variety) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE))

windrow_edge
# A tibble: 642 x 6
# Groups:   Year, ranch, block, treatment [226]
# Year ranch block treatment variety   pctNOW
#    <dbl> <dbl> <dbl> <chr>     <chr>      <dbl>
# 1  2006  3440  24.1 C         Fritz      1.52 
# 2  2006  3440  24.1 C         Monterrey  3.23 

#windrow_edge$block <- block_problems(windrow_edge$block)
windrow_edge %>% # changes 3 instances of 11.4
  filter(block %in% c(11.4,25.2)) # Three instances in 2007

### Very important--treatment categories
trts2 <- sort(unique(windrow_edge$treatment))
trts2
# [1] "100%C"   "100%MD"  "1CMD"    "1MD"     "2CMD"    "2MD"     "50%C"    "50%MD"   "C"       "CMD"     "Conv"    "EarlyC"  "EarlyMD"
# [14] "MD"      "StandC"  "StandMD"

setequal(trts,trts2)
# [1] TRUE
# Warning messages:
#   1: Unknown or uninitialised column: `Block2`. 
# 2: Unknown or uninitialised column: `Block2`. 
### So we can use code from huller_damage for categories (lines 43-56)

chem <- c("C","Conv")
md <- c("MD","1MD","2MD","50%MD","100%MD","EarlyMD","StandMD")
both <- c("CMD","1CMD","2CMD","50%C","100%C","EarlyC","StandC")

windrow_edge <- windrow_edge %>% 
  mutate(Trt_cat = case_when(
    treatment %in% chem ~ "insecticide",
    treatment %in% md ~ "mating_disruption",
    treatment %in% both ~ "both",
    TRUE ~ "wtf"
  )) # filter finds no wtf

### Clean up duplicate names for varieties
windrow_edge <- windrow_edge %>% 
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
  )) 

windrow_edge <- windrow_edge %>%
  filter(variety != "wtf") # removes 5 records w NAs in all vars that matter

Rep_blocks <- read_csv("./data/lost_hills_arrangement.csv")

Rep_blocks
# A tibble: 33 x 4
#    Ranch  Tier Block WestToEast
#    <dbl> <dbl> <dbl>      <dbl>
# 1  3450     1   2.2          1
# 2  3450     1   2.1          2

Rep_blocks <- Rep_blocks%>%
  rename(ranch = Ranch,
         block = Block)

windrow_edge <- left_join(windrow_edge,Rep_blocks)
windrow_edge


### Output windrow_edge to sas
write.csv(windrow_edge,
          "./data/windrow_edge_dmg_y06_to_y15_out_to_sas.csv", 
          row.names = FALSE)


edge2 <- windrow_edge %>% 
  group_by(Year,Trt_cat,Tier,Variety,) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE))
edge2

Desc(pctNOW ~ Trt_cat, data = edge2)
# --------------------------------------------------------------------------------------------------- 
#   pctNOW ~ Trt_cat (edge2)
# 
# Summary: 
#   n pairs: 301, valid: 298 (99.0%), missings: 3 (1.0%), groups: 3
# 
# 
# both        insecticide  mating_disruption
# mean                2.247              2.886              3.644
# median              1.009              1.582              2.219
# sd                  3.204              5.462              5.287
# IQR                 2.634              2.580              2.781
# n                     105                 85                108
# np                35.235%            28.523%            36.242%
# NAs                     1                  0                  2
# 0s                      5                  2                  2
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 11.34, df = 2, p-value = 0.003448


edge2$Trt_cat <- factor(edge2$Trt_cat,
                        levels = c("mating_disruption","insecticide","both"))

PT <- DescTools::DunnTest(pctNOW ~ Trt_cat, data = edge2, method = "bonferroni")
PT
# Dunn's test of multiple comparisons using rank sums : bonferroni  
# 
#                               mean.rank.diff   pval    
# insecticide-mating_disruption      -17.55882 0.4797    
# both-mating_disruption             -39.70952 0.0023 ** 
# both-insecticide                   -22.15070 0.2343    
