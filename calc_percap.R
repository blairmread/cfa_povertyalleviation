
rm(list=ls())
library(tidyverse)
library(readstata13)
library(MASS)

cps <- readstata13::read.dta13("/Users/blair@codeforamerica.org/Documents/GitHub/cfa_povertyalleviation/cps17_21_Working_hh_new.dta")

sn21 <- c(644691797, 612218916, 866700965, 702034598)
sn22 <- c(795256578,733723478,804579750,640787400)
t21 <- c(159480730,	167714728, 71377614,	388795141)
t22 <- c(61071188,102018494,79321840,128368530)

sn_p_21 <- c(702951,	708329,	957985,	865140)
sn_p_22 <- c(994746,	946750,	1107350,	804398)
t_p_21 <- c(206323,	130738,	94147,	158878)
t_p_22 <- c(72301, 73023,	41028, 91273)
