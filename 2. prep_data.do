
use cps17_21_Working_hh.dta, clear;
drop if statefip==11;
rename statefip FIPS;
merge m:1 FIPS using "FIPS.dta", nogen;

merge m:1 year using "cpi_2017.dta", nogen;


replace Y=(Y/1000)*cpi2017;
replace totalTransfers=(totalTransfers/1000)*cpi2017;

save cps17_21_Working_hh_new.dta, replace;

