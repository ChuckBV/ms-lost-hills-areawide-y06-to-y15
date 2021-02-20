/*****************************************************************************
/ script17_glmm_bin_windrow_interior_y15.sas
/
/
/****************************************************************************/

proc import out=y15
  datafile="dmg_wndrw_interior_expt15_to_sas.csv"
  dbms=csv replace;
run;

proc print data =y15(obs = 6);
run;

proc glimmix data = y15;
  class Tier start_when insecticide;
  model dmg_now/tot_nuts = start_when|insecticide / dist=bin ddfm = kr;
  random Tier;
run;
quit;
