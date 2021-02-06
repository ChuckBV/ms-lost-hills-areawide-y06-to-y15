#===========================================================================#
# script10_compare_windrow_production_damage.R
#
# 1. Upload huller damage, windrow damage, and trapping files
# 2. Determine which variables are in which years: huller
# 3. Examine huller data by ranch, block and year
# 4. Examine windrow damage file
# 5. Get the huller, int windrow, ext windrow dat in same df & row
# 6. Run correlation and regression
#
# For number 5, compare these with a correlaton matrix
# Do linear regressior for huller, interior, and exterior
# 
# This script originally in "Y12-lost-hills-higbee-4yr", now in
# "ms-lost-hills-areawide-y06-to-y15"
#
#===========================================================================#

### Load libraries
library(tidyverse)
library(lubridate)
library(janitor) # for compare_df_col
library(psycho)

block_problems <- function(x){
  y <- if_else(x == 11.4,11.3,
               if_else(x == 25.2,25.1,x))
  return(x)
}

### NB. For comparing windrow and huller damage, quarter-section blocks are
### appropriate. For subseqent examination of treatments, proper designation
### of reps will be important.

#-- 1. Upload huller damage, windrow damage, and trapping files -------------

list.files("./data/")

huller_dmg <- read_csv("./data/dmg_huller_y06_to_y15.csv")
huller_dmg
# A tibble: 7,262 x 8
#   Ranch2 Block Variety Treatment pctNOW pctTwigborer  Year pctTotReject
#    <dbl> <chr> <chr>   <chr>      <dbl>        <dbl> <dbl>        <dbl>
# 1   3440 24-1  FR      C          1.02        0       2006           NA
# 2   3440 24-1  FR      C          2.28        0       2006           NA
# 3   3440 24-1  FR      C          1.62        0       2006           NA

windrow_dmg <- read_csv("./data/dmg_wndrw_y06_to_y15.csv")
windrow_dmg
# A tibble: 1,862 x 12
#   date       type  plot     treatment variety ranch block position tot_nuts inf_now dmg_now sample_id
#   <date>     <chr> <chr>    <chr>     <chr>   <dbl> <dbl> <chr>       <dbl>   <dbl>   <dbl>     <dbl>
# 1 2006-08-12 MD    Interior MD        NP        345   2.2 A1            523       0       0        NA
# 2 2006-08-12 MD    Edge     MD        NP        345   2.2 E1            524       0       0        NA
# 3 2006-08-12 MD    Interior MD        NP        345   2.1 A2            598       4       4        NA

#-- 2. Determine which variables are in which years: huller -----------------

#unique(huller_dmg$Treatment)
# [1] "C"       "CMD"     "MD"      "Conv"    "1MD"     "1CMD"    "2MD"     "2CMD"    "50%MD"   "50%C"   
# [11] "100%MD"  "100%C"   "EarlyC"  "EarlyMD" "StandMD" "StandC" 

# hull_lst <-split(huller_dmg, f = huller_dmg$Year)
# trts06 <- unique(hull_lst[[1]]$Treatment)
# trts07 <- unique(hull_lst[[2]]$Treatment)
# trts08 <- unique(hull_lst[[3]]$Treatment)
# trts09 <- unique(hull_lst[[4]]$Treatment)
# trts10 <- unique(hull_lst[[5]]$Treatment)
# trts11 <- unique(hull_lst[[6]]$Treatment)
# trts12 <- unique(hull_lst[[7]]$Treatment)
# trts13 <- unique(hull_lst[[8]]$Treatment)
# trts14 <- unique(hull_lst[[9]]$Treatment)
# trts15 <- unique(hull_lst[[10]]$Treatment)

### Treatments by year
# 2006,C,CMD,MD,, 
# 2007,Conv,MD,CMD,,
# 2008,1MD,1CMD,2MD,2CMD,Conv 
# 2009,1MD,1CMD,2MD,2CMD,Conv
# 2010,1MD,1CMD,2MD,2CMD,Conv
# 2011,Conv,1CMD,2CMD,1MD,2MD
# 2012,50%MD,50%C,Conv,100%MD,100%C
# 2013,100%C,50%C,50%MD,100%MD,Conv
# 2014,100%MD,Conv,100%C,50%C,50%MD
# 2015,EarlyC,Conv,EarlyMD,StandMD,StandC

### y06 and y07 are the same xpt. y08 to y11 is the same xpt
### y12 to y14 is the same xpt y15 is its own experiment

#-- 3. Examine huller data by ranch, block and year -------------------------

### Taking the opportunity to compare with block codes in the
### w

### Summary of huller codes
x <- huller_dmg %>% 
  group_by(Ranch2,Block,Year) %>% 
  summarise(nObs = n()) %>% 
  pivot_wider(names_from = Year, names_prefix = "yr", values_from = nObs)
x

### Need a Year variable
windrow_dmg$Year <- year(windrow_dmg$date)

### Drop 30 records for which did not take
windrow_dmg <- windrow_dmg %>% 
  filter(!is.na(Year))

### Verifying that the previous clean-up did not omit a year
windrow_dmg %>% 
  group_by(Year) %>% 
  summarise(nObs = n())

windrow_dmg <- windrow_dmg %>% 
  mutate(pctNOW = 100*dmg_now/tot_nuts) 

### Ranch represents as both 3- and 4-digit numeral
windrow_dmg <- windrow_dmg %>% 
  mutate(ranch = ifelse(ranch > 1000,ranch/10,ranch)) 


y <- windrow_dmg %>%  
  group_by(ranch,block,Year) %>% 
  summarise(nObs = sum(!is.na(pctNOW))) %>% 
  pivot_wider(names_from = Year, names_prefix = "yr", values_from = nObs)
y

block_codes <- unique(x$Block) # get all block codes
block_codes <- block_codes[str_detect(block_codes,"39",negate = TRUE)] # drop weigh tickets

block_codes <- data.frame(Block = block_codes, Block2 = block_codes)
  # Output 2 columns of the same thing so I can edit the second column by hand

#write.csv(block_codes,"./burks/data-intermediate/huller_block_codes.csv", row.names = FALSE)
# futzed with this by hand in Notepad++
rm(block_codes) # Now only caused confusion
block_code_lookup <- read_csv("./data/huller_block_code_lookup.csv")

### Use info from x to fix Huller Damage file
huller_dmg <- right_join(block_code_lookup,huller_dmg)

x2 <- huller_dmg %>% 
  group_by(Ranch2,Block,Block2,Year) %>% 
  summarise(nObs = n()) %>% 
  pivot_wider(names_from = Year, names_prefix = "yr", values_from = nObs)
x2

### Block2 now useful for huller_damage
a <-sort(unique(huller_dmg$Block2))
b <- sort(unique(windrow_dmg$block))

### Examine more closely how to pair huller and windrow data
b[!b %in% a] # 11.4 is in windrow but not huller

a[!a %in% b] # Everything in huller is in windrow 
# numeric(0)
length(a[a %in% b]) # confirms, note windrow is 1 element longer than huller

windrow_dmg <- windrow_dmg %>% 
  mutate(block = ifelse(block != 11.4,block,11.3))
  # Turn all instances of 11.4 into 11.3 for compatibility with huller data

### Going forward, need to accomodate equivalence of 11.3 with 11.4 and of
### 25.2 with 25.1 in any matching exercise

#-- 4. Examine windrow damage file -------------

rm(x,x2,y) # drop previous temporary files

windrow_dmg # reminder of file structure
# A tibble: 1,832 x 14
#   date       type  plot     treatment variety ranch block position tot_nuts inf_now dmg_now sample_id  Year pctNOW
#   <date>     <chr> <chr>    <chr>     <chr>   <dbl> <dbl> <chr>       <dbl>   <dbl>   <dbl>     <dbl> <dbl>  <dbl>
# 1 2006-08-12 MD    Interior MD        NP        345   2.2 A1            523       0       0        NA  2006  0    
# 2 2006-08-12 MD    Edge     MD        NP        345   2.2 E1            524       0       0        NA  2006  0    

x <- windrow_dmg %>% 
  group_by(Year,ranch,block,variety,plot) %>% 
  summarise(nObs = sum(!is.na(pctNOW)))

x %>% 
  group_by(nObs) %>% 
  summarise(n())
# A tibble: 4 x 2
#    nObs `n()`
#   <int> <int>
# 1     1  1063
# 2     2   302
# 3     3    35
# 4     4    15

x %>% 
  filter(nObs > 1) %>% 
  group_by(Year) %>% 
  summarise(nRecs = n())
# A tibble: 10 x 2
#    Year nRecs
#   <dbl> <int>
# 1  2006    72
# 2  2007    53
# 3  2008   133
# 4  2009    15
# 5  2010    17
# 6  2011    14
# 7  2012    12
# 8  2013    12
# 9  2014    12
# 10  2015    12

rm(x)

#-- 5. Get the huller, int windrow, ext windrow dat in same df & row  -------

### Now x is huller damage by Year-ranch-block-variety-plot, and
### y is windrow damage by Year-ranch-block-variety-plot for interior, and
### z is  windrow damage by Year-ranch-block-variety-plot for exterior

x <- huller_dmg %>% 
  group_by(Year,Ranch2,Block2,Variety) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE))

varieties_x <- sort(unique(x$Variety))
varieties_x
# [1] "BU" "CA" "FR" "MI" "MO" "NP" "PR" "RY" "WD"
x
# A tibble: 746 x 5
# Groups:   Year, Ranch2, Block2 [271]
#    Year Ranch2 Block2 Variety pctNOW
#    <dbl>  <dbl>  <dbl> <chr>    <dbl>
# 1  2006   3440   24.1 FR       1.70 
# 2  2006   3440   24.1 MO       2.22 
# 3  2006   3440   24.1 NP       0.394

### Modify INTERIOR windrow damage y to have the same file structure as
### the huller data x
y <- windrow_dmg %>% 
  filter(plot == "Interior") %>%
  mutate(ranch = 10*ranch) %>% 
  group_by(Year,ranch,block,variety) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE))

varieties_y <- sort(unique(y$variety))
varieties_y
# [1] "Bu"        "Ca"        "Carmel"    "Fr"        "Fritz"     "Mi"        "Mo"        "Monterey"  "Monterrey" "NP"        "Pr"       
# [12] "Price"     "Ru"        "Ruby"      "WC"        "Wood Col" 

y <- y %>% 
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

y %>% 
  filter(Variety == "wtf")
# A tibble: 0 x 5
# Groups:   Year, ranch, block [0]
# ... with 5 variables: Year <dbl>, ranch <dbl>, block <dbl>, variety <chr>, pctNOW <dbl>

y <- y %>%
  select(-variety)
y <- y[,c(1:3,5,4)]
colnames(y) <- colnames(x)
y
# A tibble: 782 x 5
# Groups:   Year, ranch, block [268]
#    Year Ranch2 Block2 Variety pctNOW
#    <dbl>  <dbl>  <dbl> <chr>    <dbl>
# 1  2006   3440   24.1 FR       1.39 
# 2  2006   3440   24.1 MO       1.26 
# 3  2006   3440   24.1 NP       1.17 

z <- windrow_dmg %>% 
  filter(plot == "Edge") %>%
  mutate(ranch = 10*ranch) %>% 
  group_by(Year,ranch,block,variety) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE))

z <- z %>% 
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

z %>% 
  filter(Variety == "wtf")
# A tibble: 0 x 6
# Groups:   Year, ranch, block [0]
# ... with 6 variables: Year <dbl>, ranch <dbl>, block <dbl>, variety <chr>, pctNOW <dbl>, Variety <chr>

z <- z %>%
  select(-variety)
z <- z[,c(1:3,5,4)]
colnames(z) <- colnames(x)


head(x,2)
head(y,2)
head(z,2)

x$source_file <- "huller"
y$source_file <- "windrow_interior"
z$source_file <- "windrow_edge"

### can these three be joined by row bind?
compare_df_cols(list(x,y,z), return = "all", bind_method = "bind_rows")

dmg_all <- bind_rows(list(x,y,z))

dmg_all <- dmg_all %>% 
  pivot_wider(names_from = source_file, values_from = pctNOW)
dmg_all_vars <- dmg_all[,5:7]
dmg_all_vars
# A tibble: 870 x 3
#   huller windrow_interior windrow_edge
#   <dbl>            <dbl>        <dbl>
# 1  1.70             1.39         1.52 
# 2  2.22             1.26         3.23 
# 3  0.394            1.17         1.50

dmg_all_vars <- dmg_all_vars[complete.cases(dmg_all_vars), ]
# loses valid interior blocks

#-- 6. Run correlation and regression ---------------------------------------

x <- cor(dmg_all_vars) 
x
#                     huller windrow_interior windrow_edge
# huller           1.0000000        0.8274589    0.7572187
# windrow_interior 0.8274589        1.0000000    0.7117741
# windrow_edge     0.7572187        0.7117741    1.0000000

### Seven easy graphs to visualize correlation matrices
### http://jamesmarquezportfolio.com/correlation_matrices_in_r.html
### NB--This is crude, should use logistic regression, but would
### have to use assumed denominator for huller sample
chart.Correlation(dmg_all[,5:7], histogram=TRUE, pch=19)

lreg_int_v_huller <- lm(windrow_interior ~ huller, data = dmg_all)
summary(lreg_int_v_huller)
# Call:
#   lm(formula = windrow_interior ~ huller, data = dmg_all)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -13.4282  -0.4199   0.0224   0.3885  25.7449 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.10085    0.10327  -0.977    0.329    
# huller       1.41958    0.03764  37.718   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.356 on 647 degrees of freedom
# (248 observations deleted due to missingness)
# Multiple R-squared:  0.6874,	Adjusted R-squared:  0.6869 
# F-statistic:  1423 on 1 and 647 DF,  p-value: < 2.2e-16

lreg_edge_v_huller <- lm(windrow_edge ~ huller, data = dmg_all)
summary(lreg_edge_v_huller)
# 
# Call:
#   lm(formula = windrow_edge ~ huller, data = dmg_all)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -24.1557  -1.0895  -0.5549   0.4212  28.0151 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.68752    0.18777   3.662 0.000276 ***
# huller       1.75499    0.06345  27.658  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.841 on 530 degrees of freedom
# (338 observations deleted due to missingness)
# Multiple R-squared:  0.5907,	Adjusted R-squared:   0.59 
# F-statistic:   765 on 1 and 530 DF,  p-value: < 2.2e-16
