####################################
# Main text figures                #
# 1. Immunity-varying analysis     #
# 2. Tranmission-varying analysis  #
# 3. Singapore-based analysis      #
####################################

set.seed(2024)
load_packages <- c("tidyverse", "reshape2", "rlist", "pipeR", "readxl", "gridExtra", "ggnewscale")
lapply(load_packages, library, character.only = TRUE)

setwd("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model")
source("code/func_results_plot.R")

# ============================================== 
# Immunity-varying analysis              
# Fig 3: SI: R0 = 1.4, both outcomes 
# Fig 4: SI: All settings, serotype predominance
# Fig 5: QI: All settings, serotype predominance

imm_scenarios = read_xlsx('parameter_exploration/parexp_values.xlsx', sheet = 'imm_scenarios')
load("outcomes/IV_si_res.RData")
load("outcomes/IV_qi_res.RData")
setting_names <- as_labeller(
  c(`1` = "R0 = 0.7",
    `2` = "R0 = 1.4",
    `3` = "R0 = 2.1",
    `4` = "R0 = 2.8") )
id.vars = c("Scenario", "DENVa_imm", "DENVb_imm", "DENVc_imm", "DENVd_imm")

col.to.factor <- c("Scenario", "Serotype", "Outcome")

# Fig 3: SI: R0 = 1.4, both outcomes 
iv.si.1 <- melt_outcomes(res1 = imm_ts1.res$scenarios.serowins.summary,
                         res2 = imm_ts1.res$scenarios.takeoff.summary,
                         setting = 1, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # R0 = 0.7 (TS1)
iv.si.2 <- melt_outcomes(res1 = imm_ts2.res$scenarios.serowins.summary,
                         res2 = imm_ts2.res$scenarios.takeoff.summary,
                         setting = 2, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # R0 = 1.4 (TS2)
iv.si.3 <- melt_outcomes(res1 = imm_ts3.res$scenarios.serowins.summary,
                         res2 = imm_ts3.res$scenarios.takeoff.summary,
                         setting = 3, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # R0 = 2.1 (TS3)
iv.si.4 <- melt_outcomes(res1 = imm_ts4.res$scenarios.serowins.summary,
                         res2 = imm_ts4.res$scenarios.takeoff.summary,
                         setting = 4, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # R0 = 2.8 (TS4)

imm_scen.labeller <- scenario.labeller(iv.si.4$Scenario, imm_scenarios, analysis = 1)

res.plot <- plotter(res = iv.si.2, outcome = c("Prop. of sim." , "Immunity scenario"), scale.lim = c(0,0.75), scenario.label = imm_scen.labeller, label.title = c("Pop. prop.")) +
  facet_wrap(~ Outcome, nrow = 1) + ggtitle("R0 = 1.4")
res.plot

ggsave(filename = "figures/Fig_3.pdf",
       res.plot, 
       width = 7500, height = 8000, 
       units = "px", dpi = 800)

# Fig 4: SI: All settings, serotype predominance
iv.si.all <- rbind(iv.si.1, iv.si.2) %>% rbind(iv.si.3) %>% rbind(iv.si.4)
iv.si.all$Setting <- factor(iv.si.all$Setting, levels = c(1,2,3,4))

res.plot <- plotter(res = filter(iv.si.all, Outcome == "Wins"), outcome = c("Prop. of sim." , "Immunity scenario"), scale.lim = c(0,0.75), scenario.label = imm_scen.labeller, label.title = c("Pop. prop.")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/Fig_4.pdf",
       res.plot, 
       width = 17000, height = 9800, 
       units = "px", dpi = 800)

# Fig 5: QI: All settings, serotype predominance
col.to.factor <- c("Scenario", "Serotype", "Detection")

iv.qi.1 <- melt_outcomes(res1 = imm_ts1_quat.res$scenarios.serowins.summary.det,
                         res2 = imm_ts1_quat.res$scenarios.serowins.summary.all,
                         setting = 1, imm_scenarios, col.to.factor, id.vars,
                         outcome.lab = 2) # R0 = 0.7 (TS1)
iv.qi.2 <- melt_outcomes(res1 = imm_ts2_quat.res$scenarios.serowins.summary.det,
                         res2 = imm_ts2_quat.res$scenarios.serowins.summary.all,
                         setting = 2, imm_scenarios, col.to.factor, id.vars,
                         outcome.lab = 2) # R0 = 1.4 (TS2)
iv.qi.3 <- melt_outcomes(res1 = imm_ts3_quat.res$scenarios.serowins.summary.det,
                         res2 = imm_ts3_quat.res$scenarios.serowins.summary.all,
                         setting = 3, imm_scenarios, col.to.factor, id.vars,
                         outcome.lab = 2) # R0 = 2.1 (TS3)
iv.qi.4 <- melt_outcomes(res1 = imm_ts4_quat.res$scenarios.serowins.summary.det,
                         res2 = imm_ts4_quat.res$scenarios.serowins.summary.all,
                         setting = 4, imm_scenarios, col.to.factor, id.vars,
                         outcome.lab = 2) # R0 = 2.8 (TS4)

iv.qi.all <- rbind(iv.qi.1, iv.qi.2) %>% rbind(iv.qi.3) %>% rbind(iv.qi.4)
iv.qi.all$Setting <- factor(iv.qi.all$Setting, levels = c(1,2,3,4))

imm_scen.labeller <- scenario.labeller(iv.qi.4$Scenario, imm_scenarios, analysis = 1)

res.plot <- plotter(res = filter(iv.qi.all, Detection == "All"), outcome = c("Prop. of sim." , "Immunity scenario"), scale.lim = c(0,0.75), scenario.label = imm_scen.labeller, label.title = c("Pop. prop.")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/Fig_5.pdf",
       res.plot, 
       width = 17000, height = 9800, 
       units = "px", dpi = 800)

# ================================================= 
# Transmission-varying analysis              
# Fig 6: SI: All settings, outbreak establishment
# Fig 7: QI: All settings, outbreak establishment

trans_scenarios = read_xlsx('parameter_exploration/parexp_values.xlsx', sheet = 'trans_scenarios')
load("outcomes/TV_si_res.RData")
load("outcomes/TV_qi_res.RData")
setting_names <- as_labeller(
  c(`1` = "No immunity",
    `2` = "DENV-A",
    `3` = "DENV-A & -B",
    `4` = "DENV-A, -B & -C") )
id.vars = c("Scenario", "DENVa_trans", "DENVb_trans", "DENVc_trans", "DENVd_trans")

col.to.factor <- c("Scenario", "Serotype", "Outcome")

# Fig 6: SI: All settings, outbreak establishment
tv.si.1 <- melt_outcomes(res1 = trans_is1.res$scenarios.serowins.summary,
                         res2 = trans_is1.res$scenarios.takeoff.summary,
                         setting = 1, trans_scenarios, col.to.factor, id.vars,
                         outcome.lab = 1) # No population immunity (IS1)
tv.si.2 <- melt_outcomes(res1 = trans_is2.res$scenarios.serowins.summary,
                         res2 = trans_is2.res$scenarios.takeoff.summary,
                         setting = 2, trans_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # 50% immunity to DENV-A (IS2)
tv.si.3 <- melt_outcomes(res1 = trans_is3.res$scenarios.serowins.summary,
                         res2 = trans_is3.res$scenarios.takeoff.summary,
                         setting = 3, trans_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # 50% immunity to DENV-A & -B (IS3)
tv.si.4 <- melt_outcomes(res1 = trans_is4.res$scenarios.serowins.summary,
                         res2 = trans_is4.res$scenarios.takeoff.summary,
                         setting = 4, trans_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # 50% immunity to DENV-A, -B & -C (IS4)

temp = trans_scenarios * 14
trans_scen.labeller <- scenario.labeller(tv.si.4$Scenario, temp, analysis = 2)

tv.si.all <- rbind(tv.si.1, tv.si.2) %>% rbind(tv.si.3) %>% rbind(tv.si.4)
tv.si.all$Setting <- factor(tv.si.all$Setting, levels = c(1,2,3,4))

res.plot <- plotter(res = filter(tv.si.all, Outcome == "Take offs"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/Fig_6.pdf",
       res.plot, 
       width = 18000, height = 27500, 
       units = "px", dpi = 800)

# Fig 7: QI: All settings, outbreak establishment
col.to.factor <- c("Scenario", "Serotype", "Detection")

tv.qi.1 <- melt_outcomes(res1 = trans_is1_quat.res$scenarios.takeoff.summary.det,
                         res2 = trans_is1_quat.res$scenarios.takeoff.summary.all,
                         setting = 1, trans_scenarios, col.to.factor, id.vars,
                         outcome.lab = 2) # No population immunity (IS1)
tv.qi.2 <- melt_outcomes(res1 = trans_is2_quat.res$scenarios.takeoff.summary.det,
                         res2 = trans_is2_quat.res$scenarios.takeoff.summary.all,
                         setting = 2, trans_scenarios, col.to.factor, id.vars,
                         outcome.lab = 2) # 50% immunity to DENV-A (IS2)
tv.qi.3 <- melt_outcomes(res1 = trans_is3_quat.res$scenarios.takeoff.summary.det,
                         res2 = trans_is3_quat.res$scenarios.takeoff.summary.all,
                         setting = 3, trans_scenarios, col.to.factor, id.vars,
                         outcome.lab = 2) # 50% immunity to DENV-A & -B (IS3)
tv.qi.4 <- melt_outcomes(res1 = trans_is4_quat.res$scenarios.takeoff.summary.det,
                         res2 = trans_is4_quat.res$scenarios.takeoff.summary.all,
                         setting = 4, trans_scenarios, col.to.factor, id.vars,
                         outcome.lab = 2) # 50% immunity to DENV-A, -B & -C (IS4)

tv.qi.all <- rbind(tv.qi.1, tv.qi.2) %>% rbind(tv.qi.3) %>% rbind(tv.qi.4)
tv.qi.all$Setting <- factor(tv.qi.all$Setting, levels = c(1,2,3,4))

res.plot <- plotter(res = filter(tv.qi.all, Detection == "All"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/Fig_7.pdf",
       res.plot, 
       width = 18000, height = 27500, 
       units = "px", dpi = 800)

# ================================================= 
# Singapore-based analysis              
# Fig 8: SI - data interpretation 1 (>=2 == recovered class)
# Fig 9: SI - data interpretation 2 (2 == cross-reactivity)
# Fig 10: QI: data interpretation 3 (take seroresponse as is)
# Fig 11: Outbreak establishment for all DI

sg_scenarios = read_xlsx('data_cal/singapore_2014_datafm.xlsx', sheet = 'trans_scenarios')
load("outcomes/SG_res.RData")
interpretation_names <- as_labeller(
  c(`1` = "Data interpretation 1",
    `2` = "Data interpretation 2",
    `3` = "Data interpretation 3")
)
id.vars = c("Scenario", "DENV1_trans", "DENV2_trans", "DENV3_trans", "DENV4_trans")
col.to.factor <- c("Scenario", "Serotype", "Outcome")

# Fig 8: SI - data interpretation 1 (>=2 == recovered class)
sg.di.1 <- melt_sg(res1 = DI1.res$scenarios.serowins.summary,
                   res2 = DI1.res$scenarios.takeoff.summary,
                   scenario_vals = sg_scenarios, col.to.factor, id.vars,
                   interpretation = 1)
sg.di.2 <- melt_sg(res1 = DI2.res$scenarios.serowins.summary,
                   res2 = DI2.res$scenarios.takeoff.summary,
                   scenario_vals = sg_scenarios, col.to.factor, id.vars,
                   interpretation = 2)
sg.di.3 <- melt_sg(res1 = DI3.res$scenarios.serowins.summary.all,
                   res2 = DI3.res$scenarios.takeoff.summary.all,
                   scenario_vals = sg_scenarios, col.to.factor, id.vars,
                   interpretation = 3)

temp = sg_scenarios * 14
sg_scen.labeller <- scenario.labeller(sg.di.1$Scenario, temp, analysis = 3)

res.plot <- plotter(res = sg.di.1, outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = sg_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Outcome))
res.plot

ggsave(filename = "figures/Fig_8.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)

# Fig 9: SI - data interpretation 2 (2 == cross-reactivity)
res.plot <- plotter(res = sg.di.2, outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = sg_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Outcome))
res.plot

ggsave(filename = "figures/Fig_9.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)

# Fig 10: QI: data interpretation 3 (take seroresponse as is)
res.plot <- plotter(res = sg.di.3, outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = sg_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Outcome))
res.plot

ggsave(filename = "figures/Fig_10.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)

# Fig 11: Outbreak establishment for all DI
sg.di.all <- rbind(sg.di.1, sg.di.2) %>% rbind(sg.di.3)
sg.di.all$Interpretation <- as.factor(sg.di.all$Interpretation)
sg.di.all$Interpretation <- factor(sg.di.all$Interpretation, levels = c(1,2,3))

res.plot <- plotter(res = filter(sg.di.all, Outcome == "Take offs"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.4), scenario.label = sg_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Interpretation), labeller = interpretation_names)
res.plot

ggsave(filename = "figures/Fig_11.pdf",
       res.plot, 
       width = 18000, height = 27500, 
       units = "px", dpi = 800)

