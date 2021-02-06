/*****************************************************************************
/ script14_glmm_bin_windrow_interior_10yr.sas
/
/
/****************************************************************************/

proc import out=y06_2yr
  datafile="expt06_2yr_windrow.csv"
  dbms=csv;
run;

proc print data =y06_2yr(obs = 6);
run;

proc glimmix data = y06_2yr;
  class Year Tier treatment;
  model dmg_now/tot_nuts = treatment / dist=bin ddfm = kr;
  random Year|Tier;
  lsmeans treatment / adjust=tukey lines ADJDFE=ROW;
run;
quit;
/* F = 21.35, df = 2,249.9; P < 0.0001 */
/* means sep a, a, b                   */
