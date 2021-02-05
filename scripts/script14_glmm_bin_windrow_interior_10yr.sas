/*****************************************************************************
/ script14_glmm_bin_windrow_interior_10yr.sas
/
/
/****************************************************************************/

proc import out=interior2
  datafile="dmg_wndrw_interior_y06_to_y15_to_glimmix.csv"
  dbms=csv;
run;

data interior2;
  set interior2;
  if Year = . then delete;
run;

proc print data = interior2 (obs = 6);
run;

proc glimmix data = interior2;
  class Year Tier type;
  model dmg_now/tot_nuts = type / dist=bin ddfm = kr;
  random Year|Tier;
  lsmeans type / adjust=tukey lines  ADJDFE=ROW;
run;
quit;
/* F = 14.06, df = 2,91.8; P < 0.0001 */
/* means sep a, a, b                   */
