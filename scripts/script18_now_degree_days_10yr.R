#===========================================================================#
# script13_r_support_data_sas_glimmix.R
#
# 1. x
#
#===========================================================================#

library(tidyverse) # dplyr used to select and modify common/imp variables
library(lubridate) # for work with date formats
library(janitor)# clean_names and compare_df_cols
library(Hmisc) # for describe()
library(data.table)

#-- 1. Upload NOW degree-day data for study period --------------------------

### Find and list csv files for windrow damage
my_path <- "./burks/data-raw-2006-to-2011/"
(csv_files <-  list.files(my_path, pattern = ".csv")) # 34 files
(cimis_files <- csv_files[str_detect(csv_files,"ucipm")]) # 11 files
# [1] "ucipm-now-ddf-lost-hills-2005.csv" "ucipm-now-ddf-lost-hills-2006.csv"
# [3] "ucipm-now-ddf-lost-hills-2007.csv" "ucipm-now-ddf-lost-hills-2008.csv"
# [5] "ucipm-now-ddf-lost-hills-2009.csv" "ucipm-now-ddf-lost-hills-2010.csv"
# [7] "ucipm-now-ddf-lost-hills-2011.csv" "ucipm-now-ddf-lost-hills-2012.csv"
# [9] "ucipm-now-ddf-lost-hills-2013.csv" "ucipm-now-ddf-lost-hills-2014.csv"
# [11] "ucipm-now-ddf-lost-hills-2015.csv"

(cimis_names <- str_remove(cimis_files, ".csv"))

### Get relevant cimis files into a list of data frames
cimis_files %>%
  purrr::map(function(cimis_files){ # iterate through each file name
    assign(x = str_remove(cimis_files, ".csv"), # Remove file extension ".csv"
           value = read_csv(paste0(my_path, cimis_files)),
           envir = .GlobalEnv)
  }) -> df_list_read # Assign to a list

### Clean df names one by one
dd05 <- `ucipm-now-ddf-lost-hills-2005` # make df names more useable
dd06 <- `ucipm-now-ddf-lost-hills-2006`
dd07 <- `ucipm-now-ddf-lost-hills-2007`
dd08 <- `ucipm-now-ddf-lost-hills-2008`
dd09 <- `ucipm-now-ddf-lost-hills-2009`
dd10 <- `ucipm-now-ddf-lost-hills-2010`
dd11 <- `ucipm-now-ddf-lost-hills-2011`
dd12 <- `ucipm-now-ddf-lost-hills-2012`
dd13 <- `ucipm-now-ddf-lost-hills-2013`
dd14 <- `ucipm-now-ddf-lost-hills-2014`
dd15 <- `ucipm-now-ddf-lost-hills-2015`

rm(`ucipm-now-ddf-lost-hills-2005`,`ucipm-now-ddf-lost-hills-2006`,`ucipm-now-ddf-lost-hills-2007`,`ucipm-now-ddf-lost-hills-2008`,`ucipm-now-ddf-lost-hills-2009`,`ucipm-now-ddf-lost-hills-2010`,`ucipm-now-ddf-lost-hills-2011`,`ucipm-now-ddf-lost-hills-2012`,`ucipm-now-ddf-lost-hills-2013`,`ucipm-now-ddf-lost-hills-2014`,`ucipm-now-ddf-lost-hills-2015`)
  # take out trash
# A tibble: 365 x 8
#   station      date       air_min air_max degree_days accumulated_dd     x    x1
#   <chr>        <chr>        <dbl>   <dbl>       <dbl>          <dbl> <dbl> <dbl>
# 1 LOST_HILLS.A 01/01/2005      37      57        0.27           0.27    NA    NA
# 2 LOST_HILLS.A 01/02/2005      44      54        0              0.27    NA    NA
dd05 <- clean_names(dd05)
dd05

ddnames <- colnames(dd05)
  # get good column names w janitor::clean_names() and store in vector

### Impose column names on df for each year
colnames(dd06) <- ddnames
colnames(dd07) <- ddnames
colnames(dd08) <- ddnames
colnames(dd09) <- ddnames
colnames(dd10) <- ddnames
colnames(dd11) <- ddnames
colnames(dd12) <- ddnames
colnames(dd13) <- ddnames
colnames(dd14) <- ddnames
colnames(dd15) <- ddnames

dd05$x <- as.character(dd05$x)
dd06$x <- as.character(dd06$x)
dd07$x <- as.character(dd07$x)
dd08$x <- as.character(dd08$x)
dd09$x <- as.character(dd09$x)
dd10$x <- as.character(dd10$x)
dd11$x <- as.character(dd11$x)
dd12$x <- as.character(dd12$x)
dd13$x <- as.character(dd13$x)
dd14$x <- as.character(dd14$x)
dd15$x <- as.character(dd15$x)

dd05$x1 <- as.character(dd05$x1)
dd06$x1 <- as.character(dd06$x1)
dd07$x1 <- as.character(dd07$x1)
dd08$x1 <- as.character(dd08$x1)
dd09$x1 <- as.character(dd09$x1)
dd10$x1 <- as.character(dd10$x1)
dd11$x1 <- as.character(dd11$x1)
dd12$x1 <- as.character(dd12$x1)
dd13$x1 <- as.character(dd13$x1)
dd14$x1 <- as.character(dd14$x1)
dd15$x1 <- as.character(dd15$x1)


ddf_lh_10yr <- bind_rows(list(dd05,dd06,dd07,dd08,dd09,dd10,dd11,dd12,dd13,dd14,dd15))
ddf_lh_10yr
# A tibble: 4,017 x 8
#   station      date       air_min air_max degree_days accumulated_dd x     x1   
#   <chr>        <chr>        <dbl>   <dbl>       <dbl>          <dbl> <chr> <chr>
# 1 LOST_HILLS.A 01/01/2005      37      57        0.27           0.27 NA    NA   
# 2 LOST_HILLS.A 01/02/2005      44      54        0              0.27 NA    NA

rm(df_list_read,dd05,dd06,dd07,dd08,dd09,dd10,dd11,dd12,dd13,dd14,dd15)

ddf_lh_10yr$date <- as.Date(mdy(ddf_lh_10yr$date))
ddf_lh_10yr$yr <- year(ddf_lh_10yr$date)
ddf_lh_10yr$julian <- yday(ddf_lh_10yr$date)

write.csv(ddf_lh_10yr,
          "./burks/data-intermediate/now_deg_days_f_lost_hills_2005_to_2015.csv",
          row.names = FALSE)

ddf_jun15 <- ddf_lh_10yr %>% 
  select(station,date,yr,julian,accumulated_dd) %>% 
  filter(julian == 167)

plot(accumulated_dd ~ yr, data = ddf_jun15)
m1 <- lm(accumulated_dd ~ yr, data = ddf_jun15)
summary(m1) # P =  0.13, rsqr = 0.15

ddf <- ddf_jun15 %>% 
  select(yr,accumulated_dd)

ddf <-  ddf %>% 
  filter(yr > 2005) %>% 
  rename(Year = yr,
         now_ddf = accumulated_dd)

ddf_aug1 <- ddf_lh_10yr %>% 
  select(station,date,yr,julian,accumulated_dd) %>% 
  filter(julian == 214)

ddf_aug1 <- ddf_aug1 %>% 
  filter(yr > 2005) %>% 
  select(yr,accumulated_dd) %>% 
  rename(Year = yr,
         ddf_aug01 = accumulated_dd)

ddf_sep1 <- ddf_lh_10yr %>% 
  select(station,date,yr,julian,accumulated_dd) %>% 
  filter(julian == 245)

ddf_sep1 <- ddf_sep1 %>% 
  filter(yr > 2005) %>% 
  select(yr,accumulated_dd) %>% 
  rename(Year = yr,
         ddf_sep01 = accumulated_dd)

ddf_sep1

#-- 2. Upload damage data for Nonpareil and Monterey ------------------------

windrow_interior <- read_csv("./burks/data-intermediate/windrow_interior_dmg_y06_to_y15_out_to_sas.csv") 

### The following are from script12
var_by_yr3 <- windrow_interior %>% 
  filter(Variety %in% c("NP","MO") & Trt_cat == "insecticide") %>% 
  group_by(Year,Variety) %>% 
  summarise(pctNOW = mean(pctNOW, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Variety, values_from = pctNOW) %>% 
  select(Year,NP,MO) %>% 
  mutate(dom_var = ifelse(NP > MO,"NP","Mo"))


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

