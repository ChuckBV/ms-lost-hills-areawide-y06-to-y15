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
#library(FAS) # for dunnTest
#library(rcompanion) # for cldList

# block_problems <- function(x){
#   y <- if_else(x == 11.4,11.3,
#                if_else(x == 25.2,25.1,x))
#   return(y)
# }

#-- 1. Huller damage by treatment type ------------------------------------

huller_dmg <- read_csv("./data/dmg_huller_y06_to_y15.csv")
huller_dmg
# A tibble: 7,262 x 8
#   Ranch2 Block Variety Treatment pctNOW pctTwigborer  Year pctTotReject
#    <dbl> <chr> <chr>   <chr>      <dbl>        <dbl> <dbl>        <dbl>
# 1   3440 24-1  FR      C          1.02        0       2006           NA
# 2   3440 24-1  FR      C          2.28        0       2006           NA

### Treatments and categories
trts <- unique(huller_dmg$Treatment)
trts

(chem <- trts[c(1,4)])
# [1] "C"    "Conv"
(md <- trts[c(3,5,7,9,11,14,15)])
# [1] "MD"      "1MD"     "2MD"     "50%MD"   "100%MD"  "EarlyMD" "StandMD"
(both <- trts[c(2,6,8,10,12,13,16)])
# [1] "CMD"    "1CMD"   "2CMD"   "50%C"   "100%C"  "EarlyC" "StandC"

huller_dmg <- huller_dmg %>% 
  mutate(Trt_cat = case_when(
    Treatment %in% chem ~ "insecticide",
    Treatment %in% md ~ "mating_disruption",
    Treatment %in% both ~ "both",
    TRUE ~ "wtf"
  )) # filter finds no wtf

huller_dmg %>% 
  filter(Trt_cat == "wtf")

### NB Two-part process. First char to numerical block codes, then block
### code to rep blocks
block_code_lookup <- read_csv("./data/huller_block_code_lookup.csv")
block_code_lookup
# A tibble: 46 x 2
# Block Block2
#   <chr>  <dbl>
# 1 13-1    13.1
# 2 13-2    13.2
# 3 13-3    13.3

### Use info from x to fix Huller Damage file
huller_dmg <- right_join(block_code_lookup,huller_dmg)

huller_dmg %>% 
  filter(is.na(Block2)) %>% 
  group_by(Year) %>% 
  summarise(nObs = n()) # 473 records, all from 2008

### Make fake block number so treatment takes precedence
huller_dmg$Block2[is.na(huller_dmg$Block2)] <- 1.0

huller_dmg$Block2 <- block_problems(huller_dmg$Block2)

unique(huller_dmg$Block2)

### Need to get rep data

#-- STOPPED HERE ---------------------------------------##

### Output huller_dmg to sas
write.csv(huller_dmg,
          "./burks/data-intermediate/huller_dmg_y06_to_y15_out_to_sas.csv", 
          row.names = FALSE)

hdmg2 <- huller_dmg %>% 
  group_by(Year,Trt_cat,Ranch2,Block2,Variety,) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE))
hdmg2

Desc(pctNOW ~ Trt_cat, data = hdmg2)
# --------------------------------------------------------------------------------------------------------- 
#   pctNOW ~ Trt_cat (hdmg2)
# 
# Summary: 
#   n pairs: 748, valid: 748 (100.0%), missings: 0 (0.0%), groups: 3
# 
# 
#                      both        insecticide  mating_disruption
# mean                0.868              1.484              1.343
# median              0.310              0.628              0.534
# sd                  1.683              2.984              2.549
# IQR                 0.684              1.335              1.164
# n                     269                199                280
# np                35.963%            26.604%            37.433%
# NAs                     0                  0                  0
# 0s                      3                  0                  1
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 35.882, df = 2, p-value = 1.615e-08

hdmg2$Trt_cat <- factor(hdmg2$Trt_cat,
                        levels = c("insecticide","mating_disruption","both"))

PT <- DescTools::DunnTest(pctNOW ~ Trt_cat, data = hdmg2, method = "bonferroni")
PT
# 
# Dunn's test of multiple comparisons using rank sums : bonferroni  
# 
#                               mean.rank.diff    pval    
# mating_disruption-insecticide      -33.20424  0.2923    
# both-insecticide                  -114.17582 4.8e-08 ***
# both-mating_disruption             -80.97159 3.4e-05 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#-- 2. Compare interior damage ------------------------------------------------

windrow_dmg <- read_csv("./burks/data-intermediate/dmg_wndrw_y06_to_y15.csv")
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
  mutate(block = block_problems(block),
         ranch = 10*ranch) %>% 
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

### Output windrow_edge to sas
write.csv(windrow_interior,
          "./burks/data-intermediate/windrow_interior_dmg_y06_to_y15_out_to_sas.csv", 
          row.names = FALSE)


interior2 <- windrow_interior %>% 
  group_by(Year,Trt_cat,ranch,block,Variety,) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE))
interior2

Desc(pctNOW ~ Trt_cat, data = interior2)
# pctNOW ~ Trt_cat (edge2)
# 
# Summary: 
#   n pairs: 789, valid: 782 (99.1%), missings: 7 (0.9%), groups: 3
# 
# 
#                      both        insecticide  mating_disruption
# mean                0.916              1.932              1.845
# median              0.342              0.642              0.777
# sd                  1.761              4.338              4.962
# IQR                 0.878              1.509              1.581
# n                     285                200                297
# np                36.445%            25.575%            37.980%
# NAs                     3                  0                  4
# 0s                     76                 24                 47
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 35.34, df = 2, p-value = 2.118e-08

interior2$Trt_cat <- factor(interior2$Trt_cat,
                        levels = c("mating_disruption","insecticide","both"))

PT <- DescTools::DunnTest(pctNOW ~ Trt_cat, data = interior2, method = "bonferroni")
PT
# 
# Dunn's test of multiple comparisons using rank sums : bonferroni  
# 
#                               mean.rank.diff    pval    
# insecticide-mating_disruption       6.902593  1.0000    
# both-mating_disruption            -96.509162 7.0e-07 ***
# both-insecticide                 -103.411754 1.9e-06 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#-- 3. Compare edge damage ------------------------------------------------

windrow_edge <- windrow_dmg %>%  # equivalent to y from script 10
  filter(plot == "Edge") %>%
  mutate(block = block_problems(block),
         ranch = 10*ranch) %>% 
  group_by(Year,ranch,block,treatment,variety) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE))

windrow_edge
# A tibble: 794 x 6
# Groups:   Year, ranch, block, treatment [280]
# Year ranch block treatment variety   pctNOW
#   <dbl> <dbl> <dbl> <chr>     <chr>      <dbl>
# 1  2006  3440  24.1 C         Fritz      1.39 
# 2  2006  3440  24.1 C         Monterrey  1.26 

#windrow_edge$block <- block_problems(windrow_edge$block)
windrow_edge %>% # changes 3 instances of 11.4
  filter(block %in% c(11.4,25.2)) # comes up 0

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

### Output windrow_edge to sas
write.csv(windrow_edge,
          "./burks/data-intermediate/windrow_edge_dmg_y06_to_y15_out_to_sas.csv", 
          row.names = FALSE)


edge2 <- windrow_edge %>% 
  group_by(Year,Trt_cat,ranch,block,Variety,) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE))
edge2

Desc(pctNOW ~ Trt_cat, data = edge2)
# -------------------------------------------------------------------------------------------------------------------------------------- 
#   pctNOW ~ Trt_cat (edge2)
# 
# Summary: 
#   n pairs: 635, valid: 630 (99.2%), missings: 5 (0.8%), groups: 3
# 
# 
# both        insecticide  mating_disruption
# mean                2.292              2.971              3.576
# median              0.942              1.479              1.511
# sd                  4.275              6.264              6.307
# IQR                 2.064              2.585              3.593
# n                     223                177                230
# np                35.397%            28.095%            36.508%
# NAs                     2                  0                  3
# 0s                     30                 10                 22
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 12.837, df = 2, p-value = 0.001631


edge2$Trt_cat <- factor(edge2$Trt_cat,
                        levels = c("mating_disruption","insecticide","both"))

PT <- DescTools::DunnTest(pctNOW ~ Trt_cat, data = edge2, method = "bonferroni")
PT
# 
# Dunn's test of multiple comparisons using rank sums : bonferroni  
# 
#                               mean.rank.diff    pval    
# insecticide-mating_disruption       6.902593  1.0000    
# both-mating_disruption            -96.509162 7.0e-07 ***
# both-insecticide                 -103.411754 1.9e-06 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
