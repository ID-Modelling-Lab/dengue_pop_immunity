melt_one <- function(res, outcome.lab, scenario_vals, col.fac, id.vars, takeoff) {
  outcome <- as.data.frame(res)
  
  if (outcome.lab == 1) {
    if (takeoff == 1) {
      res.cols <- c("Scenario", "A_Take offs", "B_Take offs", "C_Take offs", "D_Take offs")
      model.label <- c("Take offs")
    } else {
      res.cols <- c("Scenario", "A_Wins", "B_Wins", "C_Wins", "D_Wins")
      model.label <- c("Wins")
      }
    
  }
  if (outcome.lab == 2) {
    res.cols <- c("Scenario", "A_Detected", "B_Detected", "C_Detected", "D_Detected")
    model.label <- c("Detected")
  }
  if (outcome.lab == 3) {
    res.cols <- c("Scenario", "A_All", "B_All", "C_All", "D_All")
    model.label <- c("All")
  }
  colnames(outcome) <- res.cols
  
  outcome$Scenario <- as.numeric(outcome$Scenario)
  outcome <- outcome %>% arrange(Scenario)
  
  outcome.melt <- melt(cbind(outcome, scenario_vals), id.vars = id.vars)
  outcome.melt <- separate(outcome.melt, col = variable, into = c("Serotype", "Model"), sep = "_")
  outcome.melt$Model[outcome.melt$Model == model.label] <- outcome.lab
  outcome.melt[col.fac] <- lapply(outcome.melt[col.fac], factor)
  
  return(outcome.melt)
}


melt_outcomes <- function(res1, res2, setting, scenario_vals, col.fac, id.vars, outcome.lab) {
  all_outcomes <- cbind(as.data.frame(res1), as.data.frame(res2))
  all_outcomes <- all_outcomes[-c(6)]
  
  if (outcome.lab == 1) { # outcome measures
    colnames(all_outcomes) <- c("Scenario", "A_Wins", "B_Wins", "C_Wins", "D_Wins", "A_Take offs", "B_Take offs", "C_Take offs", "D_Take offs")
    var.split <- c("Serotype", "Outcome")
  }
  
  if (outcome.lab == 2) { # detection
    colnames(all_outcomes) <- c("Scenario", "A_Detected", "B_Detected", "C_Detected", "D_Detected", "A_All", "B_All", "C_All", "D_All")
    var.split <- c("Serotype", "Detection")
  }
  
  if (outcome.lab == 3) { # model
    colnames(all_outcomes) <- c("Scenario", "A_Secondary", "B_Secondary", "C_Secondary", "D_Secondary", "A_Quaternary", "B_Quaternary", "C_Quaternary", "D_Quaternary")
    var.split <- c("Serotype", "Model")
  }
  all_outcomes$Scenario <- as.numeric(all_outcomes$Scenario)
  all_outcomes <- all_outcomes %>% arrange(Scenario)
  
  all_outcomes.melt <- melt(cbind(all_outcomes, scenario_vals), id.vars = id.vars)
  all_outcomes.melt <- separate(all_outcomes.melt, col = variable, into = var.split, sep = "_")
  all_outcomes.melt[col.fac] <- lapply(all_outcomes.melt[col.fac], factor)
  all_outcomes.melt$Setting <- setting
  
  return(all_outcomes.melt)
}

melt_sg <- function(res1, res2, scenario_vals, col.fac, id.vars, interpretation) {
  outcomes <- cbind(as.data.frame(res1), as.data.frame(res2))
  outcomes <- outcomes[-c(6)]
  colnames(outcomes) <- c("Scenario", "DENV1_Wins", "DENV2_Wins", "DENV3_Wins", "DENV4_Wins", "DENV1_Take offs", "DENV2_Take offs", "DENV3_Take offs", "DENV4_Take offs")
  var.split <- c("Serotype", "Outcome")
  
  outcomes$Scenario <- as.numeric(outcomes$Scenario)
  outcomes <- outcomes %>% arrange(Scenario)
  
  outcomes.melt <- melt(cbind(outcomes, scenario_vals), id.vars = id.vars)
  outcomes.melt <- separate(outcomes.melt, col = variable, into = var.split, sep = "_")
  outcomes.melt[col.fac] <- lapply(outcomes.melt[col.fac], factor)
  outcomes.melt$Interpretation <- interpretation
  
  return(outcomes.melt)
}

scenario.labeller <- function(scenarios, scenario_vals, analysis) { # analysis - 1: IV; 2: TV; 3: SG
  labeller <- cbind(scenarios, scenario_vals)
  if (analysis == 3) {
    colnames(labeller) <- c("Scenario", "DENV1", "DENV2", "DENV3", "DENV4")
  } else {
    colnames(labeller) <- c("Scenario", "A", "B", "C", "D")
  }

  labeller <- melt(labeller, id.vars = c("Scenario"))
  if (analysis == 1) { labeller$value <- factor(labeller$value, levels = c(0.1, 0.2, 0.3, 0.4, 0.5)) }
  
  if (analysis == 2) { labeller$value <- factor(labeller$value, levels = c(0.7, 1.4, 2.1, 2.8)) }
  
  if (analysis == 3) { labeller$value <- factor(labeller$value, levels = c(0.75, 1, 1.25, 1.5)) }
  
  return(labeller)
}

theme_cust <- theme(plot.title = element_text(hjust=0.5,face="bold",size=13,colour = "blue"),
                    legend.position = "right",
                    legend.text = element_text(size = 10),
                    legend.title = element_text(size = 10),
                    axis.title.y = element_text(size = 13),
                    axis.title.x = element_text(size = 13),
                    axis.ticks.x = element_blank(),
                    axis.text.y = element_text(face = "bold", angle = 0, hjust = 0.5, size = 12),
                    axis.text.x = element_text(face = "bold", angle = 0, hjust = 0.5, size = 12),
                    strip.text.x = element_text(size = 13, face = "bold"),
                    strip.text.y = element_text(size = 13, face = "bold"))

plotter <- function(res, outcome, scale.lim, scenario.label, label.title) {
  res.plot <- ggplot() +
    geom_tile(data = res, aes(x = Serotype, y = Scenario, fill = value), colour = "grey") +
    scale_fill_viridis_c(direction = -1, limits = scale.lim) +
    theme_cust +
    labs(fill = outcome[1], 
         x = "Serotype",
         y = outcome[2]) +
    new_scale_fill() +
    geom_tile(data = scenario.label, aes(x = variable, y = Scenario, fill = value, width = 0.1), position = position_nudge(x=-0.445)) + 
    scale_color_brewer(palette = 'Oranges', aesthetics = 'fill', name = label.title)
}