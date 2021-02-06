/*****************************************************************************
/ script15_glmm_bin_windrow_interior_y08_4yr.sas
/
/
/****************************************************************************/

proc import out=y08_4yr
  datafile="dmg_wndrw_interior_expt08_4yr_to_sas.csv"
  dbms=csv;
run;

proc print data =y08_4yr(obs = 6);
run;

proc glimmix data = y08_4yr;
  class Year Tier disp_dens insecticide;
  model dmg_now/tot_nuts = disp_dens|insecticide / dist=bin ddfm = kr;
  random Year|Tier;
run;
quit;
