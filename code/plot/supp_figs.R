####################################
# Supplementary figures            #
# 1. Immunity-varying analysis     #
# 2. Tranmission-varying analysis  #
# 3. Singapore-based analysis      #
####################################

set.seed(2024)
load_packages <- c("tidyverse", "reshape2", "rlist", "pipeR", "readxl", "gridExtra", "ggnewscale")
lapply(load_packages, library, character.only = TRUE)

setwd("~/NUS Dropbox/Maxine W Tan/projects/Dengue population model")
source("code/func_results_plot.R")

# ================================================================================================== # 
# Immunity-varying analysis              
# SF1: SI: All settings, both outcomes
# SF2: QI: All settings, both outcomes
# SF3: Both models, all settings, serotype predominance
# SF4: QI: All settings, serotype pred, 1+2 only vs all infections
# ================================================================================================== # 

imm_scenarios = read_xlsx('parameter_exploration/parexp_values.xlsx', sheet = 'imm_scenarios')
load("outcomes/IV_si_res.RData")
load("outcomes/IV_qi_res.RData")
setting_names <- as_labeller(
  c(`1` = "R0 = 0.7",
    `2` = "R0 = 1.4",
    `3` = "R0 = 2.1",
    `4` = "R0 = 2.8") )
id.vars = c("Scenario", "DENVa_imm", "DENVb_imm", "DENVc_imm", "DENVd_imm")

# SF1: SI: All settings, both outcomes
col.to.factor <- c("Scenario", "Serotype", "Outcome")

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

iv.si.all <- rbind(iv.si.1, iv.si.2) %>% rbind(iv.si.3) %>% rbind(iv.si.4)
iv.si.all$Setting <- factor(iv.si.all$Setting, levels = c(1,2,3,4))

imm_scen.labeller <- scenario.labeller(iv.si.4$Scenario, imm_scenarios, 1)

res.plot <- plotter(res = iv.si.all, outcome = c("Prop. of sim." , "Immunity scenario"), scale.lim = c(0,0.75), scenario.label = imm_scen.labeller, label.title = c("Pop. prop.")) +
  facet_grid(rows = vars(Outcome), cols = vars(Setting), 
             switch = "y", labeller = labeller(Setting = setting_names))
res.plot

ggsave(filename = "figures/SFig_1.pdf",
       res.plot, 
       width = 8500, height = 15000, 
       units = "px", dpi = 800)

# SF2: QI: All settings, both outcomes
iv.qi.1 <- melt_outcomes(res1 = imm_ts1_quat.res$scenarios.serowins.summary.all,
                         res2 = imm_ts1_quat.res$scenarios.takeoff.summary.all,
                         setting = 1, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # R0 = 0.7 (TS1)
iv.qi.2 <- melt_outcomes(res1 = imm_ts2_quat.res$scenarios.serowins.summary.all,
                         res2 = imm_ts2_quat.res$scenarios.takeoff.summary.all,
                         setting = 2, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # R0 = 1.4 (TS2)
iv.qi.3 <- melt_outcomes(res1 = imm_ts3_quat.res$scenarios.serowins.summary.all,
                         res2 = imm_ts3_quat.res$scenarios.takeoff.summary.all,
                         setting = 3, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 1) # R0 = 2.1 (TS3)
iv.qi.4 <- melt_outcomes(res1 = imm_ts4_quat.res$scenarios.serowins.summary.all,
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

ggsave(filename = "figures/SFig_2.pdf",
       res.plot, 
       width = 8500, height = 15000, 
       units = "px", dpi = 800)

# SF3: Both models, all settings, serotype pred
col.to.factor <- c("Scenario", "Serotype", "Model")

iv.sq.1 <- melt_outcomes(res1 = imm_ts1.res$scenarios.serowins.summary,
                         res2 = imm_ts1_quat.res$scenarios.serowins.summary.all,
                         setting = 1, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 3) # R0 = 0.7 (TS1)
iv.sq.2 <- melt_outcomes(res1 = imm_ts2.res$scenarios.serowins.summary,
                         res2 = imm_ts2_quat.res$scenarios.serowins.summary.all,
                         setting = 2, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 3) # R0 = 1.4 (TS2)
iv.sq.3 <- melt_outcomes(res1 = imm_ts3.res$scenarios.serowins.summary,
                         res2 = imm_ts3_quat.res$scenarios.serowins.summary.all,
                         setting = 3, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 3) # R0 = 2.1 (TS3)
iv.sq.4 <- melt_outcomes(res1 = imm_ts4.res$scenarios.serowins.summary,
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

ggsave(filename = "figures/SFig_3.pdf",
       res.plot, 
       width = 8500, height = 15000, 
       units = "px", dpi = 800)

# SF4: QI: All settings, serotype pred, all infections vs 1+2 only
col.to.factor <- c("Scenario", "Serotype", "Detection")

iv.qd.1 <- melt_outcomes(res1 = imm_ts1_quat.res$scenarios.serowins.summary.det,
                         res2 = imm_ts1_quat.res$scenarios.serowins.summary.all,
                         setting = 1, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 2) # R0 = 0.7 (TS1)
iv.qd.2 <- melt_outcomes(res1 = imm_ts2_quat.res$scenarios.serowins.summary.det,
                         res2 = imm_ts2_quat.res$scenarios.serowins.summary.all,
                         setting = 2, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 2) # R0 = 1.4 (TS2)
iv.qd.3 <- melt_outcomes(res1 = imm_ts3_quat.res$scenarios.serowins.summary.det,
                         res2 = imm_ts3_quat.res$scenarios.serowins.summary.all,
                         setting = 3, imm_scenarios, col.to.factor, id.vars, 
                         outcome.lab = 2) # R0 = 2.1 (TS3)
iv.qd.4 <- melt_outcomes(res1 = imm_ts4_quat.res$scenarios.serowins.summary.det,
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

ggsave(filename = "figures/SFig_4.pdf",
       res.plot, 
       width = 8500, height = 15000, 
       units = "px", dpi = 800)

# ================================================================================================== # 
# Transmission-varying analysis    
# SF5: SI: All settings, serotype predominance
# SF6: QI: All settings, serotype predominance
# SF7: QI: All settings, outbreak establishment, 1+2 only
# ================================================================================================== # 
trans_scenarios = read_xlsx('parameter_exploration/parexp_values.xlsx', sheet = 'trans_scenarios')
load("outcomes/TV_si_res.RData")
load("outcomes/TV_qi_res.RData")
setting_names <- as_labeller(
  c(`1` = "No immunity",
    `2` = "DENV-A",
    `3` = "DENV-A & -B",
    `4` = "DENV-A, -B & -C") )
id.vars = c("Scenario", "DENVa_trans", "DENVb_trans", "DENVc_trans", "DENVd_trans")

# SF5: SI: All settings, serotype predominance
col.to.factor <- c("Scenario", "Serotype", "Outcome")
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

res.plot <- plotter(res = filter(tv.si.all, Outcome == "Wins"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_5.pdf",
       res.plot, 
       width = 18000, height = 27500, 
       units = "px", dpi = 800)

# SF6: QI: All settings, serotype predominance
col.to.factor <- c("Scenario", "Serotype", "Detection")

tv.qi.1 <- melt_outcomes(res1 = trans_is1_quat.res$scenarios.serowins.summary.det,
                         res2 = trans_is1_quat.res$scenarios.serowins.summary.all,
                         setting = 1, trans_scenarios, col.to.factor, id.vars,
                         outcome.lab = 2) # No population immunity (IS1)
tv.qi.2 <- melt_outcomes(res1 = trans_is2_quat.res$scenarios.serowins.summary.det,
                         res2 = trans_is2_quat.res$scenarios.serowins.summary.all,
                         setting = 2, trans_scenarios, col.to.factor, id.vars,
                         outcome.lab = 2) # 50% immunity to DENV-A (IS2)
tv.qi.3 <- melt_outcomes(res1 = trans_is3_quat.res$scenarios.serowins.summary.det,
                         res2 = trans_is3_quat.res$scenarios.serowins.summary.all,
                         setting = 3, trans_scenarios, col.to.factor, id.vars,
                         outcome.lab = 2) # 50% immunity to DENV-A & -B (IS3)
tv.qi.4 <- melt_outcomes(res1 = trans_is4_quat.res$scenarios.serowins.summary.det,
                         res2 = trans_is4_quat.res$scenarios.serowins.summary.all,
                         setting = 4, trans_scenarios, col.to.factor, id.vars,
                         outcome.lab = 2) # 50% immunity to DENV-A, -B & -C (IS4)

tv.qi.all <- rbind(tv.qi.1, tv.qi.2) %>% rbind(tv.qi.3) %>% rbind(tv.qi.4)
tv.qi.all$Setting <- factor(tv.qi.all$Setting, levels = c(1,2,3,4))

res.plot <- plotter(res = filter(tv.qi.all, Detection == "All"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_6.pdf",
       res.plot, 
       width = 18000, height = 27500, 
       units = "px", dpi = 800)

# SF7: QI: All settings, serotype predominance, 1+2 only
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

res.plot <- plotter(res = filter(tv.qi.all, Detection == "Detected"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_7.pdf",
       res.plot, 
       width = 18000, height = 27500, 
       units = "px", dpi = 800)

# ================================================================================================== # 
# SF8: Outbreak establishment comparison for SI vs QI (1+2) vs QI (all)
## (A) IS3; (B) IS4
# SF9: PI 25% - Outbreak establishment comparison for SI vs QI (1+2) vs QI (all)
## (A) IS3; (B) IS4
# SF10: PI 75% - Outbreak establishment comparison for SI vs QI (1+2) vs QI (all)
## (A) IS3; (B) IS4
# ================================================================================================== # 
load("outcomes/SATV_25si_res.RData")
load("outcomes/SATV_25qi_res.RData")

# SF8: Outbreak establishment comparison for SI vs QI (1+2) vs QI (all)
setting_names <- as_labeller(
  c(`1` = "SI model",
    `2` = "QI model (pri & sec cases)",
    `3` = "QI model (all cases)") )
col.to.factor <- c("Scenario", "Serotype", "Model")
id.vars = c("Scenario", "DENVa_trans", "DENVb_trans", "DENVc_trans", "DENVd_trans")

# (A) IS3
tv.si.3 <- melt_one(res = trans_is3.res$scenarios.takeoff.summary,
                    outcome.lab = 1, trans_scenarios, col.to.factor, id.vars,
                    takeoff = 1)
tv.qi.det.3 <- melt_one(res = trans_is3_quat.res$scenarios.takeoff.summary.det,
                    outcome.lab = 2, trans_scenarios, col.to.factor, id.vars,
                    takeoff = 1)
tv.qi.all.3 <- melt_one(res = trans_is3_quat.res$scenarios.takeoff.summary.all,
                    outcome.lab = 3, trans_scenarios, col.to.factor, id.vars,
                    takeoff = 1)

compare.all.3 <- rbind(tv.si.3, tv.qi.det.3) %>% rbind(tv.qi.all.3)
compare.all.3$Model <- factor(compare.all.3$Model, levels = c(1,2,3))

res.plot <- plotter(res = compare.all.3, outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.8), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Model), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_8a.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)

# (B) IS4
tv.si.4 <- melt_one(res = trans_is4.res$scenarios.takeoff.summary,
                    outcome.lab = 1, trans_scenarios, col.to.factor, id.vars,
                    takeoff = 1)
tv.qi.det.4 <- melt_one(res = trans_is4_quat.res$scenarios.takeoff.summary.det,
                        outcome.lab = 2, trans_scenarios, col.to.factor, id.vars,
                        takeoff = 1)
tv.qi.all.4 <- melt_one(res = trans_is4_quat.res$scenarios.takeoff.summary.all,
                        outcome.lab = 3, trans_scenarios, col.to.factor, id.vars,
                        takeoff = 1)

compare.all.4 <- rbind(tv.si.4, tv.qi.det.4) %>% rbind(tv.qi.all.4)
compare.all.4$Model <- factor(compare.all.4$Model, levels = c(1,2,3))

res.plot <- plotter(res = compare.all.4, outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.8), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Model), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_8b.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)


# SF9: PI 25% - Outbreak establishment comparison for SI vs QI (1+2) vs QI (all)
setting_names <- as_labeller(
  c(`1` = "SI model",
    `2` = "QI model (pri & sec cases)",
    `3` = "QI model (all cases)") )
col.to.factor <- c("Scenario", "Serotype", "Model")
id.vars = c("Scenario", "DENVa_trans", "DENVb_trans", "DENVc_trans", "DENVd_trans")

# (A) IS3
sa.25s.3 <- melt_one(res = si25_is3.res$scenarios.takeoff.summary,
                     outcome.lab = 1, trans_scenarios, col.to.factor, id.vars,
                     takeoff = 1)
sa.25q.3.det <- melt_one(res = qi25_is3.res$scenarios.takeoff.summary.det,
                         outcome.lab = 2, trans_scenarios, col.to.factor, id.vars,
                         takeoff = 1)
sa.25q.3.all <- melt_one(res = qi25_is3.res$scenarios.takeoff.summary.all,
                         outcome.lab = 3, trans_scenarios, col.to.factor, id.vars,
                         takeoff = 1)

compare.all.3 <- rbind(sa.25s.3, sa.25q.3.det) %>% rbind(sa.25q.3.all)
compare.all.3$Model <- factor(compare.all.3$Model, levels = c(1,2,3))

res.plot <- plotter(res = compare.all.3, outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.8), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Model), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_9a.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)

# (B) IS4
col.to.factor <- c("Scenario", "Serotype", "Model")
id.vars = c("Scenario", "DENVa_trans", "DENVb_trans", "DENVc_trans", "DENVd_trans")

sa.25s.4 <- melt_one(res = si25_is4.res$scenarios.takeoff.summary,
                     outcome.lab = 1, trans_scenarios, col.to.factor, id.vars,
                     takeoff = 1)
sa.25q.4.det <- melt_one(res = qi25_is4.res$scenarios.takeoff.summary.det,
                         outcome.lab = 2, trans_scenarios, col.to.factor, id.vars,
                         takeoff = 1)
sa.25q.4.all <- melt_one(res = qi25_is4.res$scenarios.takeoff.summary.all,
                         outcome.lab = 3, trans_scenarios, col.to.factor, id.vars,
                         takeoff = 1)

compare.all.4 <- rbind(sa.25s.4, sa.25q.4.det) %>% rbind(sa.25q.4.all)
compare.all.4$Model <- factor(compare.all.4$Model, levels = c(1,2,3))

res.plot <- plotter(res = compare.all.4, outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.8), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Model), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_9b.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)

# SF10: PI 75% - Outbreak establishment comparison for SI vs QI (1+2) vs QI (all)
load("outcomes/SATV_75si_res.RData")
load("outcomes/SATV_75qi_res.RData")

setting_names <- as_labeller(
  c(`1` = "SI model",
    `2` = "QI model (pri & sec cases)",
    `3` = "QI model (all cases)") )
col.to.factor <- c("Scenario", "Serotype", "Model")
id.vars = c("Scenario", "DENVa_trans", "DENVb_trans", "DENVc_trans", "DENVd_trans")

# (A) IS3
sa.75s.3 <- melt_one(res = si75_is3.res$scenarios.takeoff.summary,
                     outcome.lab = 1, trans_scenarios, col.to.factor, id.vars,
                     takeoff = 1)
sa.75q.3.det <- melt_one(res = qi75_is3.res$scenarios.takeoff.summary.det,
                         outcome.lab = 2, trans_scenarios, col.to.factor, id.vars,
                         takeoff = 1)
sa.75q.3.all <- melt_one(res = qi75_is3.res$scenarios.takeoff.summary.all,
                         outcome.lab = 3, trans_scenarios, col.to.factor, id.vars,
                         takeoff = 1)

compare.all.3 <- rbind(sa.75s.3, sa.75q.3.det) %>% rbind(sa.75q.3.all)
compare.all.3$Model <- factor(compare.all.3$Model, levels = c(1,2,3))

res.plot <- plotter(res = compare.all.3, outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.8), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Model), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_10a.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)

# (B) IS4
sa.75s.4 <- melt_one(res = si75_is4.res$scenarios.takeoff.summary,
                     outcome.lab = 1, trans_scenarios, col.to.factor, id.vars,
                     takeoff = 1)
sa.75q.4.det <- melt_one(res = qi75_is4.res$scenarios.takeoff.summary.det,
                         outcome.lab = 2, trans_scenarios, col.to.factor, id.vars,
                         takeoff = 1)
sa.75q.4.all <- melt_one(res = qi75_is4.res$scenarios.takeoff.summary.all,
                         outcome.lab = 3, trans_scenarios, col.to.factor, id.vars,
                         takeoff = 1)

compare.all.4 <- rbind(sa.75s.4, sa.75q.4.det) %>% rbind(sa.75q.4.all)
compare.all.4$Model <- factor(compare.all.4$Model, levels = c(1,2,3))

res.plot <- plotter(res = compare.all.4, outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.8), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Model), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_10b.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)


# # SF9: Serotype predominance comparison for SI vs QI (1+2) vs QI (all)
# # (A) IS3
# col.to.factor <- c("Scenario", "Serotype", "Model")
# id.vars = c("Scenario", "DENVa_trans", "DENVb_trans", "DENVc_trans", "DENVd_trans")
# 
# tv.si.3 <- melt_one(res = trans_is3.res$scenarios.serowins.summary,
#                     outcome.lab = 1, trans_scenarios, col.to.factor, id.vars,
#                     takeoff = 0)
# tv.qi.det.3 <- melt_one(res = trans_is3_quat.res$scenarios.serowins.summary.det,
#                         outcome.lab = 2, trans_scenarios, col.to.factor, id.vars,
#                         takeoff = 0)
# tv.qi.all.3 <- melt_one(res = trans_is3_quat.res$scenarios.serowins.summary.all,
#                         outcome.lab = 3, trans_scenarios, col.to.factor, id.vars,
#                         takeoff = 0)
# 
# compare.all.3 <- rbind(tv.si.3, tv.qi.det.3) %>% rbind(tv.qi.all.3)
# compare.all.3$Model <- factor(compare.all.3$Model, levels = c(1,2,3))
# 
# res.plot <- plotter(res = compare.all.3, outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.8), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
#   facet_grid(cols = vars(Model), labeller = setting_names)
# res.plot
# 
# ggsave(filename = "figures/SFig_9a.pdf",
#        res.plot, 
#        width = 15000, height = 27500, 
#        units = "px", dpi = 800)
# 
# # (B) IS4
# tv.si.4 <- melt_one(res = trans_is4.res$scenarios.serowins.summary,
#                     outcome.lab = 1, trans_scenarios, col.to.factor, id.vars,
#                     takeoff = 0)
# tv.qi.det.4 <- melt_one(res = trans_is4_quat.res$scenarios.serowins.summary.det,
#                         outcome.lab = 2, trans_scenarios, col.to.factor, id.vars,
#                         takeoff = 0)
# tv.qi.all.4 <- melt_one(res = trans_is4_quat.res$scenarios.serowins.summary.all,
#                         outcome.lab = 3, trans_scenarios, col.to.factor, id.vars,
#                         takeoff = 0)
# 
# compare.all.4 <- rbind(tv.si.4, tv.qi.det.4) %>% rbind(tv.qi.all.4)
# compare.all.4$Model <- factor(compare.all.4$Model, levels = c(1,2,3))
# 
# res.plot <- plotter(res = compare.all.4, outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.8), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
#   facet_grid(cols = vars(Model), labeller = setting_names)
# res.plot
# 
# ggsave(filename = "figures/SFig_9b.pdf",
#        res.plot, 
#        width = 15000, height = 27500, 
#        units = "px", dpi = 800)



# ================================================================================================== # 
# SF11: PI 25% - SI model, All settings, both outcomes
## (A) outbreak establishment; (B) serotype predominance
# SF12: PI 25% - QI model, All settings, both outcomes
## (A) outbreak establishment; (B) serotype predominance
# SF13: PI 25% - QI model, All settings, both outcomes, 1+2 only
## (A) outbreak establishment; (B) serotype predominance

# SF14: PI 75% - SI model, All settings, both outcomes
## (A) outbreak establishment; (B) serotype predominance
# SF15: PI 75% - QI model, All settings, both outcomes
## (A) outbreak establishment; (B) serotype predominance
# SF16: PI 75% - QI model, All settings, both outcomes, 1+2 only
## (A) outbreak establishment; (B) serotype predominance
# ================================================================================================== # 

# SF11: PI 25% - SI model, All settings, both outcomes
trans_scenarios = read_xlsx('parameter_exploration/parexp_values.xlsx', sheet = 'trans_scenarios')
load("outcomes/SATV_25si_res.RData")
load("outcomes/SATV_25qi_res.RData")
setting_names <- as_labeller(
  c(`2` = "DENV-A",
    `3` = "DENV-A & -B",
    `4` = "DENV-A, -B & -C") )
id.vars = c("Scenario", "DENVa_trans", "DENVb_trans", "DENVc_trans", "DENVd_trans")

col.to.factor <- c("Scenario", "Serotype", "Outcome")

# SF10: PI 25% - SI model, All settings, both outcomes
## (A) outbreak establishment
sa.25s.2 <- melt_outcomes(res1 = si25_is2.res$scenarios.serowins.summary,
                          res2 = si25_is2.res$scenarios.takeoff.summary,
                          setting = 2, trans_scenarios, col.to.factor, id.vars, 
                          outcome.lab = 1) # 50% immunity to DENV-A (IS2)
sa.25s.3 <- melt_outcomes(res1 = si25_is3.res$scenarios.serowins.summary,
                          res2 = si25_is3.res$scenarios.takeoff.summary,
                          setting = 3, trans_scenarios, col.to.factor, id.vars, 
                          outcome.lab = 1) # 50% immunity to DENV-A & -B (IS3)
sa.25s.4 <- melt_outcomes(res1 = si25_is4.res$scenarios.serowins.summary,
                          res2 = si25_is4.res$scenarios.takeoff.summary,
                          setting = 4, trans_scenarios, col.to.factor, id.vars, 
                          outcome.lab = 1) # 50% immunity to DENV-A, -B & -C (IS4)

sa.25s.all <- rbind(sa.25s.2, sa.25s.3) %>% rbind(sa.25s.4)
sa.25s.all$Setting <- factor(sa.25s.all$Setting, levels = c(2,3,4))
temp = trans_scenarios * 14
trans_scen.labeller <- scenario.labeller(sa.25s.2$Scenario, temp, analysis = 2)

res.plot <- plotter(res = filter(sa.25s.all, Outcome == "Take offs"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_11a.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)

## (B) serotype predominance
res.plot <- plotter(res = filter(sa.25s.all, Outcome == "Wins"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_11b.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)

# SF12: PI 25% - QI model, All settings, both outcomes
## (A) outbreak establishment; (B) serotype predominance
sa.25q.2 <- melt_outcomes(res1 = qi25_is2.res$scenarios.serowins.summary.all,
                          res2 = qi25_is2.res$scenarios.takeoff.summary.all,
                          setting = 2, trans_scenarios, col.to.factor, id.vars, 
                          outcome.lab = 1) # 50% immunity to DENV-A (IS2)
sa.25q.3 <- melt_outcomes(res1 = qi25_is3.res$scenarios.serowins.summary.all,
                          res2 = qi25_is3.res$scenarios.takeoff.summary.all,
                          setting = 3, trans_scenarios, col.to.factor, id.vars, 
                          outcome.lab = 1) # 50% immunity to DENV-A & -B (IS3)
sa.25q.4 <- melt_outcomes(res1 = qi25_is4.res$scenarios.serowins.summary.all,
                          res2 = qi25_is4.res$scenarios.takeoff.summary.all,
                          setting = 4, trans_scenarios, col.to.factor, id.vars, 
                          outcome.lab = 1) # 50% immunity to DENV-A, -B & -C (IS4)

sa.25q.all <- rbind(sa.25q.2, sa.25q.3) %>% rbind(sa.25q.4)
sa.25q.all$Setting <- factor(sa.25q.all$Setting, levels = c(2,3,4))
temp = trans_scenarios * 14
trans_scen.labeller <- scenario.labeller(sa.25q.2$Scenario, temp, analysis = 2)

res.plot <- plotter(res = filter(sa.25q.all, Outcome == "Take offs"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_12a.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)

## (B) serotype predominance
res.plot <- plotter(res = filter(sa.25q.all, Outcome == "Wins"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_12b.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)

# SF13: PI 25% - QI model, All settings, both outcomes, 1+2 only
## (A) serotype predominance; (B) outbreak establishment
col.to.factor <- c("Scenario", "Serotype", "Detection")

sa.25q.2 <- melt_outcomes(res1 = qi25_is2.res$scenarios.serowins.summary.det,
                          res2 = qi25_is2.res$scenarios.serowins.summary.all,
                          setting = 2, trans_scenarios, col.to.factor, id.vars,
                          outcome.lab = 2) # 50% immunity to DENV-A (IS2)
sa.25q.3 <- melt_outcomes(res1 = qi25_is3.res$scenarios.serowins.summary.det,
                          res2 = qi25_is3.res$scenarios.serowins.summary.all,
                          setting = 3, trans_scenarios, col.to.factor, id.vars,
                          outcome.lab = 2) # 50% immunity to DENV-A & -B (IS3)
sa.25q.4 <- melt_outcomes(res1 = qi25_is4.res$scenarios.serowins.summary.det,
                          res2 = qi25_is4.res$scenarios.serowins.summary.all,
                          setting = 4, trans_scenarios, col.to.factor, id.vars,
                          outcome.lab = 2) # 50% immunity to DENV-A, -B & -C (IS4)

sa.25q.all <- rbind(sa.25q.2, sa.25q.3) %>% rbind(sa.25q.4)
sa.25q.all$Setting <- factor(sa.25q.all$Setting, levels = c(2,3,4))

res.plot <- plotter(res = filter(sa.25q.all, Detection == "Detected"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_13a.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)

# (B) outbreak establishmentt, 1+2 only
sa.25q.2 <- melt_outcomes(res1 = qi25_is2.res$scenarios.takeoff.summary.det,
                          res2 = qi25_is2.res$scenarios.takeoff.summary.all,
                          setting = 2, trans_scenarios, col.to.factor, id.vars,
                          outcome.lab = 2) # 50% immunity to DENV-A (IS2)
sa.25q.3 <- melt_outcomes(res1 = qi25_is3.res$scenarios.takeoff.summary.det,
                          res2 = qi25_is3.res$scenarios.takeoff.summary.all,
                          setting = 3, trans_scenarios, col.to.factor, id.vars,
                          outcome.lab = 2) # 50% immunity to DENV-A & -B (IS3)
sa.25q.4 <- melt_outcomes(res1 = qi25_is4.res$scenarios.takeoff.summary.det,
                          res2 = qi25_is4.res$scenarios.takeoff.summary.all,
                          setting = 4, trans_scenarios, col.to.factor, id.vars,
                          outcome.lab = 2) # 50% immunity to DENV-A, -B & -C (IS4)

sa.25q.all <- rbind(sa.25q.2, sa.25q.3) %>% rbind(sa.25q.4)
sa.25q.all$Setting <- factor(sa.25q.all$Setting, levels = c(2,3,4))

res.plot <- plotter(res = filter(sa.25q.all, Detection == "Detected"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_13b.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)


# SF14: PI 75% - SI model, All settings, both outcomes
trans_scenarios = read_xlsx('parameter_exploration/parexp_values.xlsx', sheet = 'trans_scenarios')
load("outcomes/SATV_75si_res.RData")
setting_names <- as_labeller(
  c(`2` = "DENV-A",
    `3` = "DENV-A & -B",
    `4` = "DENV-A, -B & -C") )
id.vars = c("Scenario", "DENVa_trans", "DENVb_trans", "DENVc_trans", "DENVd_trans")

col.to.factor <- c("Scenario", "Serotype", "Outcome")

# SF14: PI 75% - SI model, All settings, both outcomes
## (A) outbreak establishment
sa.75s.2 <- melt_outcomes(res1 = si75_is2.res$scenarios.serowins.summary,
                          res2 = si75_is2.res$scenarios.takeoff.summary,
                          setting = 2, trans_scenarios, col.to.factor, id.vars, 
                          outcome.lab = 1) # 50% immunity to DENV-A (IS2)
sa.75s.3 <- melt_outcomes(res1 = si75_is3.res$scenarios.serowins.summary,
                          res2 = si75_is3.res$scenarios.takeoff.summary,
                          setting = 3, trans_scenarios, col.to.factor, id.vars, 
                          outcome.lab = 1) # 50% immunity to DENV-A & -B (IS3)
sa.75s.4 <- melt_outcomes(res1 = si75_is4.res$scenarios.serowins.summary,
                          res2 = si75_is4.res$scenarios.takeoff.summary,
                          setting = 4, trans_scenarios, col.to.factor, id.vars, 
                          outcome.lab = 1) # 50% immunity to DENV-A, -B & -C (IS4)

sa.75s.all <- rbind(sa.75s.2, sa.75s.3) %>% rbind(sa.75s.4)
sa.75s.all$Setting <- factor(sa.75s.all$Setting, levels = c(2,3,4))
temp = trans_scenarios * 14
trans_scen.labeller <- scenario.labeller(sa.75s.2$Scenario, temp, analysis = 2)

res.plot <- plotter(res = filter(sa.75s.all, Outcome == "Take offs"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_14a.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)

## (B) serotype predominance
res.plot <- plotter(res = filter(sa.75s.all, Outcome == "Wins"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_14b.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)


# SF15: PI 75% - QI model, All settings, both outcomes
## (A) outbreak establishment; (B) serotype predominance
sa.75q.2 <- melt_outcomes(res1 = qi75_is2.res$scenarios.serowins.summary.all,
                          res2 = qi75_is2.res$scenarios.takeoff.summary.all,
                          setting = 2, trans_scenarios, col.to.factor, id.vars, 
                          outcome.lab = 1) # 50% immunity to DENV-A (IS2)
sa.75q.3 <- melt_outcomes(res1 = qi75_is3.res$scenarios.serowins.summary.all,
                          res2 = qi75_is3.res$scenarios.takeoff.summary.all,
                          setting = 3, trans_scenarios, col.to.factor, id.vars, 
                          outcome.lab = 1) # 50% immunity to DENV-A & -B (IS3)
sa.75q.4 <- melt_outcomes(res1 = qi75_is4.res$scenarios.serowins.summary.all,
                          res2 = qi75_is4.res$scenarios.takeoff.summary.all,
                          setting = 4, trans_scenarios, col.to.factor, id.vars, 
                          outcome.lab = 1) # 50% immunity to DENV-A, -B & -C (IS4)

sa.75q.all <- rbind(sa.75q.2, sa.75q.3) %>% rbind(sa.75q.4)
sa.75q.all$Setting <- factor(sa.75q.all$Setting, levels = c(2,3,4))
temp = trans_scenarios * 14
trans_scen.labeller <- scenario.labeller(sa.75q.2$Scenario, temp, analysis = 2)

res.plot <- plotter(res = filter(sa.75q.all, Outcome == "Take offs"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_15a.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)

## (B) serotype predominance
res.plot <- plotter(res = filter(sa.75q.all, Outcome == "Wins"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_15b.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)

# SF16: PI 75% - QI model, All settings, both outcomes, 1+2 only
## (A) outbreak establishment; (B) serotype predominance
col.to.factor <- c("Scenario", "Serotype", "Detection")


sa.75q.2 <- melt_outcomes(res1 = qi75_is2.res$scenarios.serowins.summary.det,
                          res2 = qi75_is2.res$scenarios.serowins.summary.all,
                          setting = 2, trans_scenarios, col.to.factor, id.vars,
                          outcome.lab = 2) # 50% immunity to DENV-A (IS2)
sa.75q.3 <- melt_outcomes(res1 = qi75_is3.res$scenarios.serowins.summary.det,
                          res2 = qi75_is3.res$scenarios.serowins.summary.all,
                          setting = 3, trans_scenarios, col.to.factor, id.vars,
                          outcome.lab = 2) # 50% immunity to DENV-A & -B (IS3)
sa.75q.4 <- melt_outcomes(res1 = qi75_is4.res$scenarios.serowins.summary.det,
                          res2 = qi75_is4.res$scenarios.serowins.summary.all,
                          setting = 4, trans_scenarios, col.to.factor, id.vars,
                          outcome.lab = 2) # 50% immunity to DENV-A, -B & -C (IS4)

sa.75q.all <- rbind(sa.75q.2, sa.75q.3) %>% rbind(sa.75q.4)
sa.75q.all$Setting <- factor(sa.75q.all$Setting, levels = c(2,3,4))

res.plot <- plotter(res = filter(sa.75q.all, Detection == "Detected"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_16a.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)

# SF13: PI 25% - QI model, All settings, both outcomes, 1+2 only
## (A) serotype predominance; (B) outbreak establishment
sa.75q.2 <- melt_outcomes(res1 = qi75_is2.res$scenarios.takeoff.summary.det,
                          res2 = qi75_is2.res$scenarios.takeoff.summary.all,
                          setting = 2, trans_scenarios, col.to.factor, id.vars,
                          outcome.lab = 2) # 50% immunity to DENV-A (IS2)
sa.75q.3 <- melt_outcomes(res1 = qi75_is3.res$scenarios.takeoff.summary.det,
                          res2 = qi75_is3.res$scenarios.takeoff.summary.all,
                          setting = 3, trans_scenarios, col.to.factor, id.vars,
                          outcome.lab = 2) # 50% immunity to DENV-A & -B (IS3)
sa.75q.4 <- melt_outcomes(res1 = qi75_is4.res$scenarios.takeoff.summary.det,
                          res2 = qi75_is4.res$scenarios.takeoff.summary.all,
                          setting = 4, trans_scenarios, col.to.factor, id.vars,
                          outcome.lab = 2) # 50% immunity to DENV-A, -B & -C (IS4)

sa.75q.all <- rbind(sa.75q.2, sa.75q.3) %>% rbind(sa.75q.4)
sa.75q.all$Setting <- factor(sa.75q.all$Setting, levels = c(2,3,4))

res.plot <- plotter(res = filter(sa.75q.all, Detection == "Detected"), outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = trans_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Setting), labeller = setting_names)
res.plot

ggsave(filename = "figures/SFig_16b.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)




# ============================================== 
# Singapore-based analysis              
# SF17: DI3: Both outcomes, 1+2 only

sg_scenarios = read_xlsx('data_cal/singapore_2014_datafm.xlsx', sheet = 'trans_scenarios')
load("outcomes/SG_res.RData")
interpretation_names <- as_labeller(
  c(`1` = "Data interpretation 1",
    `2` = "Data interpretation 2",
    `3` = "Data interpretation 3")
)
id.vars = c("Scenario", "DENV1_trans", "DENV2_trans", "DENV3_trans", "DENV4_trans")
col.to.factor <- c("Scenario", "Serotype", "Outcome")

sg.di.3 <- melt_sg(res1 = DI3.res$scenarios.serowins.summary.det,
                   res2 = DI3.res$scenarios.takeoff.summary.det,
                   scenario_vals = sg_scenarios, col.to.factor, id.vars,
                   interpretation = 3)

temp = sg_scenarios * 14
sg_scen.labeller <- scenario.labeller(sg.di.3$Scenario, temp, analysis = 3)

res.plot <- plotter(res = sg.di.3, outcome = c("Prop. of sim." , "Transmission scenario"), scale.lim = c(0,0.85), scenario.label = sg_scen.labeller, label.title = c("R_0")) +
  facet_grid(cols = vars(Outcome))
res.plot

ggsave(filename = "figures/SFig_18.pdf",
       res.plot, 
       width = 15000, height = 27500, 
       units = "px", dpi = 800)








