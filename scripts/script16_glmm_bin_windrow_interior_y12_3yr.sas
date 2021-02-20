/*****************************************************************************
/ script16_glmm_bin_windrow_interior_y12_3yr.sas
/
/
/****************************************************************************/

proc import out=y12_3yr
  datafile="dmg_wndrw_interior_expt12_3yr_to_sas.csv"
  dbms=csv;
run;

proc print data =y12_3yr(obs = 6);
run;

proc glimmix data = y12_3yr;
  class Year Tier phero_conc insecticide;
  model dmg_now/tot_nuts = phero_conc|insecticide / dist=bin ddfm = kr;
  random Year|Tier;
run;
quit;

