set.seed(2024)
load_packages <- c("tidyverse", "reshape2", "rlist", "pipeR", "readxl")
lapply(load_packages, library, character.only = TRUE)

setwd("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model")
source("code/func_rds_extract.R")
# imm_scenarios = read_xlsx('parameter_exploration/immunity/parexp_values.xlsx', sheet = 'imm_scenarios')

##############################
# Immunity-varying analysis  #
# (a) SI model               #
# (b) QI model               #
##############################

## (a) IV: SI model
filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/immunity/cp/ts_1/rds")
imm_ts1.list = setup(filedir, stringNum = 4, immVtrans = 0, secVquart = 0, age = 0)
imm_ts1.res = propCalc.sec(imm_ts1.list, n = 70, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/immunity/cp/ts_2/rds")
imm_ts2.list = setup(filedir, stringNum = 4, immVtrans = 0, secVquart = 0, age = 0)
imm_ts2.res = propCalc.sec(imm_ts2.list, n = 70, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/immunity/cp/ts_3/rds")
imm_ts3.list = setup(filedir, stringNum = 4, immVtrans = 0, secVquart = 0, age = 0)
imm_ts3.res = propCalc.sec(imm_ts3.list, n = 70, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/immunity/cp/ts_4/rds")
imm_ts4.list = setup(filedir, stringNum = 4, immVtrans = 0, secVquart = 0, age = 0)
imm_ts4.res = propCalc.sec(imm_ts4.list, n = 70, age = 0)

## (b) IV: QI model
filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/immunity/quaternary/imm_quart_1/rds")
imm_ts1.list = setup(filedir, stringNum = 5, immVtrans = 0, secVquart = 1, age = 0)
imm_ts1_quat.res = propCalc.quat(imm_ts1.list, n = 70, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/immunity/quaternary/imm_quart_2/rds")
imm_ts2.list = setup(filedir, stringNum = 5, immVtrans = 0, secVquart = 1, age = 0)
imm_ts2_quat.res = propCalc.quat(imm_ts2.list, n = 70, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/immunity/quaternary/imm_quart_3/rds")
imm_ts3.list = setup(filedir, stringNum = 5, immVtrans = 0, secVquart = 1, age = 0)
imm_ts3_quat.res = propCalc.quat(imm_ts3.list, n = 70, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/immunity/quaternary/imm_quart_4/rds")
imm_ts4.list = setup(filedir, stringNum = 5, immVtrans = 0, secVquart = 1, age = 0)
imm_ts4_quat.res = propCalc.quat(imm_ts4.list, n = 70, age = 0)

rm(c(imm_ts1.list, imm_ts2.list, imm_ts3.list, imm_ts4.list))

##################################
# Transmission-varying analysis  #
# (c) SI model                   #
# (d) QI model                   #
##################################

## (c) TV: SI model
filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/is_1/rds")
trans_is1.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 0, age = 0)
trans_is1.res = propCalc.sec(trans_is1.list, n = 256, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/is_2/rds")
trans_is2.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 0, age = 0)
trans_is2.res = propCalc.sec(trans_is2.list, n = 256, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/is_3/rds")
trans_is3.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 0, age = 0)
trans_is3.res = propCalc.sec(trans_is3.list, n = 256, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/is_4/rds")
trans_is4.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 0, age = 0)
trans_is4.res = propCalc.sec(trans_is4.list, n = 256, age = 0)

## (d) TV: QI model
filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/quaternary/quartDetect_1/rds")
trans_is1.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 1, age = 0)
trans_is1_quat.res = propCalc.quat(trans_is1.list, n = 256, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/quaternary/quartDetect_2/rds")
trans_is2.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 1, age = 0)
trans_is2_quat.res = propCalc.quat(trans_is2.list, n = 256, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/quaternary/quartDetect_3/rds")
trans_is3.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 1, age = 0)
trans_is3_quat.res = propCalc.quat(trans_is3.list, n = 256, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/quaternary/quartDetect_4/rds")
trans_is4.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 1, age = 0)
trans_is4_quat.res = propCalc.quat(trans_is4.list, n = 256, age = 0)

rm(c(trans_is1.list, trans_is2.list, trans_is3.list, trans_is4.list))

#########################################
# Singapore-based analysis              #
# (e) SI model: two = protection        #
# (f) SI model: two = cross-protection  #
# (g) QI model                          #
#########################################

## (e) SG: >=2 seroresponses == past secondary
filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/data_cal/sgdata_run/rds")
DI1.list = setup(filedir, stringNum = 3, immVtrans = 1, secVquart = 0, age = 1)
DI1.res = propCalc.sec(DI1.list, n = 256, age = 1)

## (f) SG: 2 seroresponses == cross-reactivity
filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/data_cal/sensitivity/crossreact/rds")
DI2.list = setup(filedir, stringNum = 3, immVtrans = 1, secVquart = 0, age = 1)
DI2.res = propCalc.sec(DI2.list, n = 256, age = 1)

## (g) SG: Take seroresponse as it is
filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/data_cal/sensitivity/quarternary/rds")
DI3.list = setup(filedir, stringNum = 3, immVtrans = 1, secVquart = 1, age = 1)
DI3.res = propCalc.quat(DI3.list, n = 256, age = 1)

rm(c(DI1.res, DI2.res, DI3.res))

#########################################################
# Sensitivity analysis: TV                              #
# (h) all immunity settings but 25% population immunity #
#     (1) SI model                                      #
#     (2) QI model                                      #
# (i) all immunity settings but 75% population immunity #
#     (1) SI model                                      #
#     (2) QI model                                      #
#########################################################

## (h) SA-TV: 25% population immunity
###   (1) SI model
filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/sensitivity/pop_imm_25/secondary/SA_tv_sec_2/rds")
trans_is2.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 0, age = 0)
si25_is2.res = propCalc.sec(trans_is2.list, n = 256, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/sensitivity/pop_imm_25/secondary/SA_tv_sec_3/rds")
trans_is3.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 0, age = 0)
si25_is3.res = propCalc.sec(trans_is3.list, n = 256, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/sensitivity/pop_imm_25/secondary/SA_tv_sec_4/rds")
trans_is4.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 0, age = 0)
si25_is4.res = propCalc.sec(trans_is4.list, n = 256, age = 0)

###   (2) QI model
filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/sensitivity/pop_imm_25/quaternary/SA_tv_quat_2/rds")
trans_is2.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 1, age = 0)
qi25_is2.res = propCalc.quat(trans_is2.list, n = 256, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/sensitivity/pop_imm_25/quaternary/SA_tv_quat_3/rds")
trans_is3.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 1, age = 0)
qi25_is3.res = propCalc.quat(trans_is3.list, n = 256, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/sensitivity/pop_imm_25/quaternary/SA_tv_quat_4/rds")
trans_is4.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 1, age = 0)
qi25_is4.res = propCalc.quat(trans_is4.list, n = 256, age = 0)

## (i) SA-TV: 75% population immunity
###   (1) SI model
filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/sensitivity/pop_imm_75/secondary/SA_tv_sec_2/rds")
trans_is2.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 0, age = 0)
si75_is2.res = propCalc.sec(trans_is2.list, n = 256, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/sensitivity/pop_imm_75/secondary/SA_tv_sec_3/rds")
trans_is3.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 0, age = 0)
si75_is3.res = propCalc.sec(trans_is3.list, n = 256, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/sensitivity/pop_imm_75/secondary/SA_tv_sec_4/rds")
trans_is4.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 0, age = 0)
si75_is4.res = propCalc.sec(trans_is4.list, n = 256, age = 0)

###   (2) QI model
filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/sensitivity/pop_imm_75/quaternary/SA_tv_quat_2/rds")
trans_is2.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 1, age = 0)
qi75_is2.res = propCalc.quat(trans_is2.list, n = 256, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/sensitivity/pop_imm_75/quaternary/SA_tv_quat_3/rds")
trans_is3.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 1, age = 0)
qi75_is3.res = propCalc.quat(trans_is3.list, n = 256, age = 0)

filedir = c("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/parameter_exploration/transmission/sensitivity/pop_imm_75/quaternary/SA_tv_quat_4/rds")
trans_is4.list = setup(filedir, stringNum = 4, immVtrans = 1, secVquart = 1, age = 0)
qi75_is4.res = propCalc.quat(trans_is4.list, n = 256, age = 0)

## SAVE RESULTS
setwd("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model/outcomes")
save(imm_ts1.res, imm_ts2.res, imm_ts3.res, imm_ts4.res, file = "IV_si_res.RData")
save(imm_ts1_quat.res, imm_ts2_quat.res, imm_ts3_quat.res, imm_ts4_quat.res, file = "IV_qi_res.RData")

save(trans_is1.res, trans_ts2.res, trans_ts3.res, trans_ts4.res, file = "TV_si_res.RData")
save(trans_is1_quat.res, trans_ts2_quat.res, trans_ts3_quat.res, trans_ts4_quat.res, file = "TV_qi_res.RData")

save(DI1.res, DI2.res, DI3.res, file = "SG_res.RData")

save(si25_is2.res, si25_is3.res, si25_is4.res, file = "SATV_25si_res.RData")
save(qi25_is2.res, qi25_is3.res, qi25_is4.res, file = "SATV_25qi_res.RData")

save(si75_is2.res, si75_is3.res, si75_is4.res, file = "SATV_75si_res.RData")
save(qi75_is2.res, qi75_is3.res, qi75_is4.res, file = "SATV_75qi_res.RData")
