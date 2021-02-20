#===========================================================================#
# script18_now_degree_days_10yr.R
#
# Exploratory script examining whether temperature or other factors could
# be identified as strongly influencing whether there was more damage in
# NP than MO or vice-versa. Degree-days did not seem to be a factor, and
# it did not appear that there were one or a few blocks that were 
# pre-disposed for damage in one variety compared to the other.
#
# 1. Upload NOW degree-day data for study period (line 20)
#     - Examine dd by year at certain Julian dates
# 2. Upload damage data for Nonpareil and Monterey (line 110)
#     - Examines whether there was a treatment x block pattern
#      in which variety gets greater damage
#     - t-test for Jun 15, reported in paper, is at line 203
# 3. Plot dd vs Julian for ead of the 10 years (line 239)
#
#===========================================================================#

library(tidyverse) # dplyr used to select and modify common/imp variables
library(lubridate) # for work with date format

#-- 1. Upload NOW degree-day data for study period -------------------------

ddf_lh_10yr <- read_csv("./data/now_deg_days_f_lost_hills_2005_to_2015.csv")

ddf_lh_10yr

### Select Jun 15
ddf_jun15 <- ddf_lh_10yr %>% 
  select(station,date,yr,julian,accumulated_dd) %>% 
  filter(julian == 167)

### Is there an observable warming trend over the decade?
plot(accumulated_dd ~ yr, data = ddf_jun15)
m1 <- lm(accumulated_dd ~ yr, data = ddf_jun15)
summary(m1) # P =  0.13, rsqr = 0.15
  # No

ddf <- ddf_jun15 %>% 
  select(yr,accumulated_dd)

ddf <-  ddf %>% 
  filter(yr > 2005) %>% 
  rename(Year = yr,
         now_ddf = accumulated_dd)
ddf
# A tibble: 10 x 2
# Year now_ddf
#   <dbl>   <dbl>
# 1  2006   1122.
# 2  2007   1368.
# 3  2008   1137.
# 4  2009   1285.
# 5  2010    974.
# 6  2011    879.
# 7  2012   1221.
# 8  2013   1412.
# 9  2014   1558.
# 10  2015   1475.

### Try again with August 1
ddf_aug1 <- ddf_lh_10yr %>% 
  select(station,date,yr,julian,accumulated_dd) %>% 
  filter(julian == 214)

ddf_aug1 <- ddf_aug1 %>% 
  filter(yr > 2005) %>% 
  select(yr,accumulated_dd) %>% 
  rename(Year = yr,
         ddf_aug01 = accumulated_dd)

ddf_aug1
# A tibble: 10 x 2
# Year ddf_aug01
# <dbl>     <dbl>
# 1  2006     2344.
# 2  2007     2451.
# 3  2008     2275.
# 4  2009     2405.
# 5  2010     2025.
# 6  2011     1905.
# 7  2012     2214.
# 8  2013     2520.
# 9  2014     2686.
# 10  2015     2585.

### Try again with September 1
ddf_sep1 <- ddf_lh_10yr %>% 
  select(station,date,yr,julian,accumulated_dd) %>% 
  filter(julian == 245)

ddf_sep1 <- ddf_sep1 %>% 
  filter(yr > 2005) %>% 
  select(yr,accumulated_dd) %>% 
  rename(Year = yr,
         ddf_sep01 = accumulated_dd)

ddf_sep1
# A tibble: 10 x 2
# Year ddf_sep01
#   <dbl>     <dbl>
# 1  2006     3021.
# 2  2007     3196.
# 3  2008     3039.
# 4  2009     3099.
# 5  2010     2680.
# 6  2011     2582.
# 7  2012     2975.
# 8  2013     3253.
# 9  2014     3412.
# 10  2015     3303.

#-- 2. Upload damage data for Nonpareil and Monterey ------------------------

windrow_interior <- read_csv("./data/windrow_interior_dmg_y06_to_y15_out_to_sas.csv") 
windrow_interior

### The following are from script12
var_by_yr3 <- windrow_interior %>% 
  filter(Variety %in% c("NP","MO") & Trt_cat == "insecticide") %>% 
  group_by(Year,block,Variety) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Variety, values_from = pctNOW) %>% 
  mutate(dom_var = ifelse(NP > MO,"NP","MO"))

var_by_yr3
# A tibble: 60 x 4
# Groups:   Year, block [60]
#    Year block    MO    NP
#   <dbl> <dbl> <dbl> <dbl>
# 1  2006  24.1 1.26  1.17 
# 2  2006  24.2 4.30  2.61 
# 3  2006  24.3 2.04  0.865

length(unique(var_by_yr3$block))
# [1] 18

### Create an index that is the ration of NP to total damage
var_by_yr3 <- var_by_yr3 %>% 
  mutate(np_index = NP/(NP+MO))

x <- var_by_yr3 %>% 
  ungroup(.) %>% 
  mutate(Year = factor(Year, levels = unique(Year)),
         block = factor(block, levels = unique(block)))

### Plot index by year (obs = block)
ggplot(x, aes(x = Year, y = np_index)) +
  geom_boxplot() 

### Plot index by block (obs = year)
ggplot(x, aes(x = block, y = np_index)) +
  geom_boxplot() 

  
m1 <- lm(np_index ~ Year + block, data = x) 
Anova(m1)
# Note: model has aliased coefficients
# sums of squares computed by model comparison
# Anova Table (Type II tests)
# 
# Response: np_index
#           Sum Sq Df F value    Pr(>F)    
# Year      3.2116  8  7.6983 1.239e-05 ***
# block     0.8171 16  0.9793    0.5004    
# Residuals 1.6166 31                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ 
    # Sloppy, but strongly suggests that this is not about box effects

var_by_yr3
# A tibble: 10 x 4
# Groups:   Year [10]
#     Year     NP     MO dom_var
#    <dbl>  <dbl>  <dbl> <chr>  
# 1   2006 1.09   2.02   Mo     
# 2   2007 0.481  0.628  Mo     
# 3   2008 2.27   0.647  NP     
# 4   2009 3.61   0.837  NP     
# 5   2010 0.350  0.0785 NP     
# 6   2011 0.0289 0.622  Mo     
# 7   2012 0.914  3.32   Mo     
# 8   2013 0.557  0.0933 NP     
# 9   2014 2.70   1.40   NP     
# 10  2015 2.35   0.434  NP

var_by_yr3b <- full_join(var_by_yr3,ddf)

var_by_yr3b %>% 
  arrange(now_ddf)

var_by_yr3b %>% 
  group_by(dom_var) %>% 
  summarise(nObs = n(),
            mn = mean(now_ddf),
            sem = FSA::se(now_ddf))
# A tibble: 2 x 4
#   dom_var  nObs    mn   sem
#   <chr>   <int> <dbl> <dbl>
# 1 Mo          4 1148. 103. 
# 2 NP          6 1307.  89.9

t.test(now_ddf ~ dom_var, data = var_by_yr3b, var.equal = FALSE)
# 
# Welch Two Sample t-test
# 
# data:  now_ddf by dom_var
# t = -1.1663, df = 6.915, p-value = 0.2821
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -483.0479  164.4745
# sample estimates:
#   mean in group Mo mean in group NP 
# 1147.685         1306.972 

var_by_yr3c <- full_join(var_by_yr3,ddf_aug1)
var_by_yr3c %>% 
  arrange(ddf_aug01)

t.test(ddf_aug01 ~ dom_var, data = var_by_yr3c, var.equal = FALSE)
# 
# Welch Two Sample t-test
# 
# data:  ddf_aug01 by dom_var
# t = -1.2216, df = 6.6265, p-value = 0.2635
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -553.8722  179.3889
# sample estimates:
#   mean in group Mo mean in group NP 
# 2228.725         2415.967 

var_by_yr3d <- full_join(var_by_yr3,ddf_sep1)
var_by_yr3d %>% 
  arrange(ddf_sep01)

t.test(ddf_sep01 ~ dom_var, data = var_by_yr3d, var.equal = FALSE)

#-- 3. Plot ddf vs Julian ---------------------------------------------------

ddf_lh_10yr
# A tibble: 4,017 x 10
#   station      date       air_min air_max degree_days accumulated_dd x     x1       yr julian
#   <chr>        <date>       <dbl>   <dbl>       <dbl>          <dbl> <chr> <chr> <dbl>  <dbl>
# 1 LOST_HILLS.A 2005-01-01      37      57        0.27           0.27 NA    NA     2005      1
# 2 LOST_HILLS.A 2005-01-02      44      54        0              0.27 NA    NA     2005      2

var_by_yr3b
# A tibble: 10 x 5
# Groups:   Year [10]
#    Year     NP     MO dom_var now_ddf
#    <dbl>  <dbl>  <dbl> <chr>     <dbl>
# 1  2006 1.09   2.02   Mo        1122.
# 2  2007 0.481  0.628  Mo        1368.

dom_var_x_yr <- var_by_yr3b %>% 
  select(Year,dom_var) %>% 
  rename(yr = Year) %>% 
  filter(yr > 2005)
dom_var_x_yr

ddf_lh_10yr2 <- right_join(ddf_lh_10yr,dom_var_x_yr)

ggplot(data = ddf_lh_10yr2, aes(x = julian, y = accumulated_dd, group = yr, colour = factor(yr))) +
  geom_line() +
  facet_grid(. ~ dom_var) +
  xlim(200,250)

