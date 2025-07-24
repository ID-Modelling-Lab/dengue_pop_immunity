# ======================= #
# Immunity scenarios      #
# Supplementary figures   #
# ======================= #
set.seed(2024)
load_packages <- c("tidyverse", "reshape2", "rlist", "pipeR", "readxl", "gridExtra", "ggnewscale")
lapply(load_packages, library, character.only = TRUE)

setwd("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model")
source("code/func_results_plot.R")
imm_scenarios = read_xlsx('parameter_exploration/parexp_values.xlsx', sheet = 'imm_scenarios')
load("outcomes/IV_si_res.RData")
load("outcomes/IV_qi_res.RData")
setting_names <- as_labeller(
  c(`1` = "R0 = 0.7",
    `2` = "R0 = 1.4",
    `3` = "R0 = 2.1",
    `4` = "R0 = 2.8") )
id.vars = c("Scenario", "DENVa_imm", "DENVb_imm", "DENVc_imm", "DENVd_imm")

# =========================================== #
# Supplementary Figure 1 - IV, SI model       #
# All transmission settings and outcomes      #
# =========================================== #
col.to.factor <- c("Scenario", "Serotype", "Outcome")

iv.si.1 <- melt_immunity(res1 = imm_ts1.res$scenarios.serowins.summary,
                         res2 = imm_ts1.res$scenarios.takeoff.summary,
                         setting = 1, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # R0 = 0.7 (TS1)
iv.si.2 <- melt_immunity(res1 = imm_ts2.res$scenarios.serowins.summary,
                         res2 = imm_ts2.res$scenarios.takeoff.summary,
                         setting = 2, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # R0 = 1.4 (TS2)
iv.si.3 <- melt_immunity(res1 = imm_ts3.res$scenarios.serowins.summary,
                         res2 = imm_ts3.res$scenarios.takeoff.summary,
                         setting = 3, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # R0 = 2.1 (TS3)
iv.si.4 <- melt_immunity(res1 = imm_ts4.res$scenarios.serowins.summary,
                         res2 = imm_ts4.res$scenarios.takeoff.summary,
                         setting = 4, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # R0 = 2.8 (TS4)

iv.si.all <- rbind(iv.si.1, iv.si.2) %>% rbind(iv.si.3) %>% rbind(iv.si.4)
iv.si.all$Setting <- factor(iv.si.all$Setting, levels = c(1,2,3,4))

imm_scen.labeller <- scenario.labeller(iv.si.4$Scenario, imm_scenarios, 1)

res.plot <- plotter(res = iv.si.all, outcome = c("Prop. of sim." , "Immunity scenario"), scale.lim = c(0,0.75), scenario.label = imm_scen.labeller, label.title = c("Pop. prop.")) +
  facet_grid(rows = vars(Outcome), cols = vars(Setting), 
             switch = "y", labeller = labeller(Setting = setting_names))
res.plot

ggsave(filename = "figures/SuppFig_1.pdf",
       res.plot, 
       width = 8500, height = 15000, 
       units = "px", dpi = 800)
 
# =========================================== #
# Supplementary Figure 2 - IV, QI model       #
# All transmission settings and outcomes      #
# All cases                                   #
# =========================================== #
col.to.factor <- c("Scenario", "Serotype", "Outcome")

iv.qi.1 <- melt_immunity(res1 = imm_ts1_quat.res$scenarios.serowins.summary.all,
                         res2 = imm_ts1_quat.res$scenarios.takeoff.summary.all,
                         setting = 1, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # R0 = 0.7 (TS1)
iv.qi.2 <- melt_immunity(res1 = imm_ts2_quat.res$scenarios.serowins.summary.all,
                         res2 = imm_ts2_quat.res$scenarios.takeoff.summary.all,
                         setting = 2, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # R0 = 1.4 (TS2)
iv.qi.3 <- melt_immunity(res1 = imm_ts3_quat.res$scenarios.serowins.summary.all,
                         res2 = imm_ts3_quat.res$scenarios.takeoff.summary.all,
                         setting = 3, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # R0 = 2.1 (TS3)
iv.qi.4 <- melt_immunity(res1 = imm_ts4_quat.res$scenarios.serowins.summary.all,
                         res2 = imm_ts4_quat.res$scenarios.takeoff.summary.all,
                         setting = 4, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # R0 = 2.8 (TS4)

iv.qi.all <- rbind(iv.qi.1, iv.qi.2) %>% rbind(iv.qi.3) %>% rbind(iv.qi.4)
iv.qi.all$Setting <- factor(iv.qi.all$Setting, levels = c(1,2,3,4))

imm_scen.labeller <- scenario.labeller(iv.qi.4$Scenario, imm_scenarios, 1)

res.plot <- plotter(res = iv.qi.all, outcome = c("Prop. of sim." , "Immunity scenario"), scale.lim = c(0,0.75), scenario.label = imm_scen.labeller, label.title = c("Pop. prop.")) +
  facet_grid(rows = vars(Outcome), cols = vars(Setting), 
             switch = "y", labeller = labeller(Setting = setting_names))
res.plot

ggsave(filename = "figures/SuppFig_2.pdf",
       res.plot, 
       width = 8500, height = 15000, 
       units = "px", dpi = 800)

# =========================================== #
# Supplementary Figure 3 - IV                 #
# All transmission settings, wins only        #
# SI model vs QI model (All infections)       #
# =========================================== #
col.to.factor <- c("Scenario", "Serotype", "Model")

iv.sq.1 <- melt_immunity(res1 = imm_ts1.res$scenarios.serowins.summary,
                         res2 = imm_ts1_quat.res$scenarios.serowins.summary.all,
                         setting = 1, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 3) # R0 = 0.7 (TS1)
iv.sq.2 <- melt_immunity(res1 = imm_ts2.res$scenarios.serowins.summary,
                         res2 = imm_ts2_quat.res$scenarios.serowins.summary.all,
                         setting = 2, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 3) # R0 = 1.4 (TS2)
iv.sq.3 <- melt_immunity(res1 = imm_ts3.res$scenarios.serowins.summary,
                         res2 = imm_ts3_quat.res$scenarios.serowins.summary.all,
                         setting = 3, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 3) # R0 = 2.1 (TS3)
iv.sq.4 <- melt_immunity(res1 = imm_ts4.res$scenarios.serowins.summary,
                         res2 = imm_ts4_quat.res$scenarios.serowins.summary.all,
                         setting = 4, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 3) # R0 = 2.8 (TS4)

iv.sq.all <- rbind(iv.sq.1, iv.sq.2) %>% rbind(iv.sq.3) %>% rbind(iv.sq.4)
iv.sq.all$Setting <- factor(iv.sq.all$Setting, levels = c(1,2,3,4))
iv.sq.all$Model <- factor(iv.sq.all$Model, levels = c("Secondary", "Quaternary"))

imm_scen.labeller <- scenario.labeller(iv.sq.4$Scenario, imm_scenarios, 1)

res.plot <- plotter(res = iv.sq.all, outcome = c("Prop. of sim." , "Immunity scenario"), scale.lim = c(0,0.75), scenario.label = imm_scen.labeller, label.title = c("Pop. prop.")) +
  facet_grid(rows = vars(Model), cols = vars(Setting), 
             switch = "y", labeller = labeller(Setting = setting_names))
res.plot

ggsave(filename = "figures/SuppFig_3.pdf",
       res.plot, 
       width = 8500, height = 15000, 
       units = "px", dpi = 800)

# =========================================== #
# Supplementary Figure 4 - IV, QI model       #
# All transmission settings, wins only        #
# Detections vs All infections               #
# =========================================== #
col.to.factor <- c("Scenario", "Serotype", "Detection")

iv.qd.1 <- melt_immunity(res1 = imm_ts1_quat.res$scenarios.serowins.summary.det,
                         res2 = imm_ts1_quat.res$scenarios.serowins.summary.all,
                         setting = 1, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 2) # R0 = 0.7 (TS1)
iv.qd.2 <- melt_immunity(res1 = imm_ts2_quat.res$scenarios.serowins.summary.det,
                         res2 = imm_ts2_quat.res$scenarios.serowins.summary.all,
                         setting = 2, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 2) # R0 = 1.4 (TS2)
iv.qd.3 <- melt_immunity(res1 = imm_ts3_quat.res$scenarios.serowins.summary.det,
                         res2 = imm_ts3_quat.res$scenarios.serowins.summary.all,
                         setting = 3, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 2) # R0 = 2.1 (TS3)
iv.qd.4 <- melt_immunity(res1 = imm_ts4_quat.res$scenarios.serowins.summary.det,
                         res2 = imm_ts4_quat.res$scenarios.serowins.summary.all,
                         setting = 4, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 2) # R0 = 2.8 (TS4)

iv.qd.all <- rbind(iv.qd.1, iv.qd.2) %>% rbind(iv.qd.3) %>% rbind(iv.qd.4)
iv.qd.all$Setting <- factor(iv.qd.all$Setting, levels = c(1,2,3,4))

imm_scen.labeller <- scenario.labeller(iv.qd.4$Scenario, imm_scenarios, 1)

res.plot <- plotter(res = iv.qd.all, outcome = c("Prop. of sim." , "Immunity scenario"), scale.lim = c(0,0.75), scenario.label = imm_scen.labeller, label.title = c("Pop. prop.")) +
  facet_grid(rows = vars(Detection), cols = vars(Setting), 
             switch = "y", labeller = labeller(Setting = setting_names))
res.plot

ggsave(filename = "figures/SuppFig_4.pdf",
       res.plot, 
       width = 8500, height = 15000, 
       units = "px", dpi = 800)







