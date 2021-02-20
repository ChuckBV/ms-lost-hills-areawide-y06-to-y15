# Impact of mating disruption and insecticide treatments on navel orangeworm damage

Repository:  ms-lost-hills-areawide-y06-to-y15

## Overview

Files compare damage between plots treated with mating disruption, 
insecticide, or both. Similar experiments were run at the Paramount Farming
(Wonderful Orchards) Lost Hills location from 2006 to 2015. Plots compared
mating disruption, alone, insecticide alone, or both together. The mating
disruption treatments were further subdivided by mating disruption intensity.

Experiments included:
 - Insecticide only vs. mating disruption vs. both (2006 and 2007)
 - Insecticide, and 1 or 2 dispensers per acre with or without insecide,
 (five treatments) (2008-2011)
 - Insecticide, and 2 dispensers per acre with 50% or 100% label rate
 with or without insecticide (five treatments) (2012-2014)
 - Insecticide, and mating disruption with early or standard start time
 with or without insectide (five treatments) (2015 only)

Initial data wrangling is done in gitub.com/chuckbv/Y12-lost-hills-higbee-4yr

Data files in this repository include huller damage, damage assessments from
windrow samples collected by the research department, and degree-day \
information for 2005 to 2015.

## Data Sets

### Input data
 - Huller damage: dmg_huller_y06_to_y15.csv
 - Windrow damage: dmg_wndrw_y06_to_y15.csv
 - NOW deg days f frm jan 1: now_deg_days_f_lost_hills_2005_to_2015.csv

### Meta data
 - Ranch and section lookup: huller_block_code_lookup.csv
 - Tier rep block arrangement: lost_hills_arrangement.cv

### Intermediate files for statistical input

## Scripts
 - script10_compare_windrow_production_damage.R
 - script11_compare_dmg_by_md_category.R
 - script12_graph_dmg_across_years.R
 - script13_r_support_data_sas_glimmix.R
 - script14_glmm_bin_windrow_interior_10yr.sas
 - script14b_glmm_bin_windrow_interior_2yr.sas
 - script15_glmm_bin_windrow_interior_y08_4yr.sas
 - script16_glmm_bin_windrow_interior_y12_3yr.sas
 - script17_glmm_bin_windrow_interior_y15.sas
 - script18_now_degree_days_10yr.R
 