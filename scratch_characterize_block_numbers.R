# scratch_characterize_block_numbers.R

library(tidyverse)
library(lubridate)
library(DescTools)
library(FSA) # for their se() function
library(Hmisc) # for describe()

Rep_blocks <- read_csv("./data/lost_hills_arrangement.csv")

Rep_blocks
# A tibble: 33 x 4
#    Ranch  Tier Block WestToEast
#    <dbl> <dbl> <dbl>      <dbl>
# 1  3450     1   2.2          1
# 2  3450     1   2.1          2


nrow(Rep_blocks)
# [1] 33

length(unique(Rep_blocks$Block))
# [1] 32 

### Find the one duplicate
Rep_blocks %>% 
  group_by(Block) %>% 
  summarise(nObs = n()) %>% 
  filter(nObs > 1)
# 1  12.1     2
# Ranch  Tier Block WestToEast
#   <dbl> <dbl> <dbl>      <dbl>
# 1  3450     3  12.1          4
# 2  4390     3  12.1          5
  # filter pistachios (4000 series) out before merge

### Blocks are unique w/i Lost Hills, do not have to include ranch in merge

Rep_blocks %>% 
  filter(Block == 12.1)

Rep_blocks <- Rep_blocks%>%
  rename(ranch = Ranch,
         block = Block)
Rep_blocks