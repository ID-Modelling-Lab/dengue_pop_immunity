###############################################################################
# Functions to save and calculate outcome measures for each analysis
# Split into secondary and quaternary models extractions

# setting up empty list to save outcome results
setup = function(filedir, stringNum, immVtrans, secVquart, age) {
  setwd(filedir)
  fileNames = list.files(pattern = ".RDS")
  scenarioName = unname(sapply(fileNames, function(x) {
    str_split_fixed(str_split_fixed(x, "_", stringNum)[stringNum], "\\.", 2)[1] } )  )
  
  # 0: Immunity-varying analysis; 1: Transmission-varying analysis
  if(immVtrans == 0) { n = 70 } else { n = 256 }
  
  scenarios.serowins.summary = data.frame(matrix(0, nrow = n, ncol = 5))
  scenarios.serowins.summary[,1] = scenarioName
  scenarios.takeoff.summary = data.frame(matrix(0, nrow = n, ncol = 5))
  scenarios.takeoff.summary[,1] = scenarioName
  
  if (age == 0) { # 0: no age (IV & TV analyses); 1: age is included (aka SG analysis)
    new.cases= sapply(c(1:4), function(x) { paste0("new.I", x) })
    detected.cases= sapply(c(1:4), function(x) { paste0("detected.I", x) })
  } else {
    new.cases = sapply(c(1:4), function(x) {
      sapply(c(1:11), function(y) { paste0("new_age_I", x, ".", y) }) })
    detected.cases = sapply(c(1:4), function(x) {
      sapply(c(1:11), function(y) {
        paste0("detected.I", x, ".", y) }) })
  }
  
  # create list items based on selected model and analysis
  if (secVquart == 0) { # 0: SI model; 1: QI model
    return(list(fileNames = fileNames,
                scenarioName = scenarioName,
                new.cases = new.cases,
                scenarios.serowins.summary = scenarios.serowins.summary,
                scenarios.takeoff.summary = scenarios.takeoff.summary ))
  } else {
    return(list(fileNames = fileNames,
                scenarioName = scenarioName,
                new.cases = new.cases,
                detected.cases = detected.cases,
                scenarios.serowins.summary.det = scenarios.serowins.summary,
                scenarios.serowins.summary.all = scenarios.serowins.summary,
                scenarios.takeoff.summary.det = scenarios.takeoff.summary,
                scenarios.takeoff.summary.all = scenarios.takeoff.summary ))
    }
}

##############################
# Secondary infections model #
##############################

# tabulate proportion of wins and proportion of take offs per scenario for each serotype
propCalc.sec = function(settingResults, n, age) {
  for (i in 1:n) {
    scenario = readRDS(settingResults$fileNames[i])
    
    ## collecting each run, runs = 500
    infecteds.1 = data.frame(matrix(0, nrow = 366, ncol = 500))
    infecteds.2 = data.frame(matrix(0, nrow = 366, ncol = 500))
    infecteds.3 = data.frame(matrix(0, nrow = 366, ncol = 500))
    infecteds.4 = data.frame(matrix(0, nrow = 366, ncol = 500))
    
    consec.1 = data.frame(matrix(0, nrow = 366, ncol = 500))
    consec.2 = data.frame(matrix(0, nrow = 366, ncol = 500))
    consec.3 = data.frame(matrix(0, nrow = 366, ncol = 500))
    consec.4 = data.frame(matrix(0, nrow = 366, ncol = 500))
    print(i)
    
    for (j in 1:500) {
      if (age == 0) { # no age
        temp1 = scenario[[j]][["res"]][[settingResults$new.cases[1]]] # incident cases for each time step in 1 sim
        temp2 = scenario[[j]][["res"]][[settingResults$new.cases[2]]] # incident cases for each time step in 1 sim
        temp3 = scenario[[j]][["res"]][[settingResults$new.cases[3]]] # incident cases for each time step in 1 sim
        temp4 = scenario[[j]][["res"]][[settingResults$new.cases[4]]] # incident cases for each time step in 1 sim
      } else { # age included
        i1.l = lapply(scenario[[j]], `[`, settingResults$new.cases[,1])
        temp1 = Reduce('+', i1.l$res) # number of incident cases for each time step in 1 simulation
        
        i2.l = lapply(scenario[[j]], `[`, settingResults$new.cases[,2])
        temp2 = Reduce('+', i2.l$res) # number of incident cases for each time step in 1 simulation
        
        i3.l = lapply(scenario[[j]], `[`, settingResults$new.cases[,3])
        temp3 = Reduce('+', i3.l$res) # number of incident cases for each time step in 1 simulation
        
        i4.l = lapply(scenario[[j]], `[`, settingResults$new.cases[,4])
        temp4 = Reduce('+', i4.l$res) # number of incident cases for each time step in 1 simulation
      }

      infecteds.1[,j] <- temp1
      infecteds.2[,j] <- temp2
      infecteds.3[,j] <- temp3
      infecteds.4[,j] <- temp4
      
      temp1 <- temp1 > 0
      temp2 <- temp2 > 0
      temp3 <- temp3 > 0
      temp4 <- temp4 > 0
      
      consec.1[,j] <- with(rle(temp1), rep(lengths * values, lengths))
      consec.2[,j] <- with(rle(temp2), rep(lengths * values, lengths))
      consec.3[,j] <- with(rle(temp3), rep(lengths * values, lengths))
      consec.4[,j] <- with(rle(temp4), rep(lengths * values, lengths))
    }
    
    # maximum incident cases in a day for each run
    infected1.Max = apply(infecteds.1, 2, max)
    infected2.Max = apply(infecteds.2, 2, max)
    infected3.Max = apply(infecteds.3, 2, max)
    infected4.Max = apply(infecteds.4, 2, max)
    
    # infectedX.Max contains the maximum # of new infections in a single day for each run
    # we need to compare each run to see which was the 'winning' serotype
    infecteds.bysero.Max = data.frame(infected1.Max, infected2.Max, infected3.Max, infected4.Max)
    infecteds.bysero.Max$winningSerotype = as.factor(max.col(infecteds.bysero.Max))
    
    # temp = unname(summary(infecteds.bysero.Max$winningSerotype))
    temp = c(length(which(infecteds.bysero.Max$winningSerotype==1)),
             length(which(infecteds.bysero.Max$winningSerotype==2)),
             length(which(infecteds.bysero.Max$winningSerotype==3)),
             length(which(infecteds.bysero.Max$winningSerotype==4)))
    settingResults$scenarios.serowins.summary[i,2:5] <- temp/500
    
    # maximum number of consecutive days with incident cases detected
    consec1.Max = apply(consec.1, 2, max)
    consec2.Max = apply(consec.2, 2, max)
    consec3.Max = apply(consec.3, 2, max)
    consec4.Max = apply(consec.4, 2, max)
    
    temp1 = consec1.Max >= 7
    temp2 = consec2.Max >= 7
    temp3 = consec3.Max >= 7
    temp4 = consec4.Max >= 7
    
    # assign take off score
    consec.bysero.Max = data.frame(sum(temp1), sum(temp2), sum(temp3), sum(temp4))
    settingResults$scenarios.takeoff.summary[i, 2:5] <- consec.bysero.Max/500
    
    print("done")
    
    rm(scenario)
  }
  return(settingResults)
}


###############################
# Quaternary infections model #
###############################
propCalc.quat = function(settingResults, n, age) {
  for (i in 1:n) {
    scenario = readRDS(settingResults$fileNames[i])
    
    ## setting up dataframes to collect each run, runs = 500
    infecteds.1 = data.frame(matrix(0, nrow = 366, ncol = 500))
    infecteds.2 = data.frame(matrix(0, nrow = 366, ncol = 500))
    infecteds.3 = data.frame(matrix(0, nrow = 366, ncol = 500))
    infecteds.4 = data.frame(matrix(0, nrow = 366, ncol = 500))
    
    detected.1 = data.frame(matrix(0, nrow = 366, ncol = 500))
    detected.2 = data.frame(matrix(0, nrow = 366, ncol = 500))
    detected.3 = data.frame(matrix(0, nrow = 366, ncol = 500))
    detected.4 = data.frame(matrix(0, nrow = 366, ncol = 500))
    
    consec.1 = data.frame(matrix(0, nrow = 366, ncol = 500))
    consec.2 = data.frame(matrix(0, nrow = 366, ncol = 500))
    consec.3 = data.frame(matrix(0, nrow = 366, ncol = 500))
    consec.4 = data.frame(matrix(0, nrow = 366, ncol = 500))
    
    detconsec.1 = data.frame(matrix(0, nrow = 366, ncol = 500))    
    detconsec.2 = data.frame(matrix(0, nrow = 366, ncol = 500))    
    detconsec.3 = data.frame(matrix(0, nrow = 366, ncol = 500))    
    detconsec.4 = data.frame(matrix(0, nrow = 366, ncol = 500))   
    
    for (j in 1:500) {
      if (age == 0) { # no age
        # all infections
        temp1 = scenario[[j]][["res"]][[settingResults$new.cases[1]]] # incident cases for each time step in 1 sim
        temp2 = scenario[[j]][["res"]][[settingResults$new.cases[2]]] # incident cases for each time step in 1 sim
        temp3 = scenario[[j]][["res"]][[settingResults$new.cases[3]]] # incident cases for each time step in 1 sim
        temp4 = scenario[[j]][["res"]][[settingResults$new.cases[4]]] # incident cases for each time step in 1 sim
        
        # pri and sec infections
        temp5 = scenario[[j]][["res"]][[settingResults$detected.cases[1]]] # detected cases for each time step in 1 sim
        temp6 = scenario[[j]][["res"]][[settingResults$detected.cases[2]]] # detected cases for each time step in 1 sim
        temp7 = scenario[[j]][["res"]][[settingResults$detected.cases[3]]] # detected cases for each time step in 1 sim
        temp8 = scenario[[j]][["res"]][[settingResults$detected.cases[4]]] # detected cases for each time step in 1 sim
      } else { # age included
        # all infections
        i1.l = lapply(scenario[[j]], `[`, settingResults$new.cases[,1])
        temp1 = Reduce('+', i1.l$res) # number of incident cases for each time step in 1 simulation
        
        i2.l = lapply(scenario[[j]], `[`, settingResults$new.cases[,2])
        temp2 = Reduce('+', i2.l$res) # number of incident cases for each time step in 1 simulation
        
        i3.l = lapply(scenario[[j]], `[`, settingResults$new.cases[,3])
        temp3 = Reduce('+', i3.l$res) # number of incident cases for each time step in 1 simulation
        
        i4.l = lapply(scenario[[j]], `[`, settingResults$new.cases[,4])
        temp4 = Reduce('+', i4.l$res) # number of incident cases for each time step in 1 simulation
        
        # pri and sec infections
        i1.l = lapply(scenario[[j]], `[`, settingResults$detected.cases[,1]) # detected cases for each time step in 1 sim
        temp5 = Reduce('+', i1.l$res) # number of incident cases for each time step in 1 simulation
        
        i2.l = lapply(scenario[[j]], `[`, settingResults$detected.cases[,2]) # detected cases for each time step in 1 sim
        temp6 = Reduce('+', i2.l$res) # number of incident cases for each time step in 1 simulation
        
        i3.l = lapply(scenario[[j]], `[`, settingResults$detected.cases[,3]) # detected cases for each time step in 1 sim
        temp7 = Reduce('+', i3.l$res) # number of incident cases for each time step in 1 simulation
        
        i4.l = lapply(scenario[[j]], `[`, settingResults$detected.cases[,4]) # detected cases for each time step in 1 sim
        temp8 = Reduce('+', i4.l$res) # number of incident cases for each time step in 1 simulation
      }
      
      # all infections
      infecteds.1[,j] <- temp1
      infecteds.2[,j] <- temp2
      infecteds.3[,j] <- temp3
      infecteds.4[,j] <- temp4
      
      temp1 <- temp1 > 0
      temp2 <- temp2 > 0
      temp3 <- temp3 > 0
      temp4 <- temp4 > 0
      
      consec.1[,j] <- with(rle(temp1), rep(lengths * values, lengths))
      consec.2[,j] <- with(rle(temp2), rep(lengths * values, lengths))
      consec.3[,j] <- with(rle(temp3), rep(lengths * values, lengths))
      consec.4[,j] <- with(rle(temp4), rep(lengths * values, lengths))
      
      # pri and sec infections
      detected.1[,j] <- temp5
      detected.2[,j] <- temp6
      detected.3[,j] <- temp7
      detected.4[,j] <- temp8
      
      temp5 <- temp5 > 0
      temp6 <- temp6 > 0
      temp7 <- temp7 > 0
      temp8 <- temp8 > 0
      
      detconsec.1[,j] <- with(rle(temp5), rep(lengths * values, lengths))
      detconsec.2[,j] <- with(rle(temp6), rep(lengths * values, lengths))
      detconsec.3[,j] <- with(rle(temp7), rep(lengths * values, lengths))
      detconsec.4[,j] <- with(rle(temp8), rep(lengths * values, lengths))
    }
    # ALL CASES
    # maximum incident cases in a day for each run
    infected1.Max = apply(infecteds.1, 2, max)
    infected2.Max = apply(infecteds.2, 2, max)
    infected3.Max = apply(infecteds.3, 2, max)
    infected4.Max = apply(infecteds.4, 2, max)
    
    # infectedX.Max contains the maximum # of new infections in a single day for each run
    # we need to compare each run to see which was the 'winning' serotype
    infecteds.bysero.Max = data.frame(infected1.Max, infected2.Max, infected3.Max, infected4.Max)
    infecteds.bysero.Max$winningSerotype = as.factor(max.col(infecteds.bysero.Max))
    
    # temp = unname(summary(infecteds.bysero.Max$winningSerotype))
    temp = c(length(which(infecteds.bysero.Max$winningSerotype==1)),
             length(which(infecteds.bysero.Max$winningSerotype==2)),
             length(which(infecteds.bysero.Max$winningSerotype==3)),
             length(which(infecteds.bysero.Max$winningSerotype==4)))
    # settingResults$scenarios.serowins.summary[i,2:5] <- temp/500
    settingResults$scenarios.serowins.summary.all[i,2:5] <- temp
    settingResults$scenarios.serowins.summary.all[i,2:5] <- settingResults$scenarios.serowins.summary.all[i,2:5]/500
    
    # maximum number of consecutive days with incident cases detected
    consec1.Max = apply(consec.1, 2, max)
    consec2.Max = apply(consec.2, 2, max)
    consec3.Max = apply(consec.3, 2, max)
    consec4.Max = apply(consec.4, 2, max)
    
    temp1 = consec1.Max >= 7
    temp2 = consec2.Max >= 7
    temp3 = consec3.Max >= 7
    temp4 = consec4.Max >= 7
    
    # assign take off score
    consec.bysero.Max = data.frame(sum(temp1), sum(temp2), sum(temp3), sum(temp4))
    settingResults$scenarios.takeoff.summary.all[i, 2:5] <- consec.bysero.Max/500
    
    print("done incident")
    # DETECTED CASES
    # maximum detected cases in a day for each run
    detected1.Max = apply(detected.1, 2, max)
    detected2.Max = apply(detected.2, 2, max)
    detected3.Max = apply(detected.3, 2, max)
    detected4.Max = apply(detected.4, 2, max)
    
    # infectedX.Max contains the maximum # of new infections in a single day for each run
    # we need to compare each run to see which was the 'winning' serotype
    infecteds.bysero.Max = data.frame(detected1.Max, detected2.Max, detected3.Max, detected4.Max)
    infecteds.bysero.Max$winningSerotype = as.factor(max.col(infecteds.bysero.Max))
    
    # temp = unname(summary(infecteds.bysero.Max$winningSerotype))
    temp = c(length(which(infecteds.bysero.Max$winningSerotype==1)),
             length(which(infecteds.bysero.Max$winningSerotype==2)),
             length(which(infecteds.bysero.Max$winningSerotype==3)),
             length(which(infecteds.bysero.Max$winningSerotype==4)))
    # settingResults$scenarios.serowins.summary[i,2:5] <- temp/500
    settingResults$scenarios.serowins.summary.det[i,2:5] <- temp
    settingResults$scenarios.serowins.summary.det[i,2:5] <- settingResults$scenarios.serowins.summary.det[i,2:5]/500
    
    # maximum number of consecutive days with incident cases detected
    detconsec1.Max = apply(detconsec.1, 2, max)
    detconsec2.Max = apply(detconsec.2, 2, max)
    detconsec3.Max = apply(detconsec.3, 2, max)
    detconsec4.Max = apply(detconsec.4, 2, max)
    
    temp1 = detconsec1.Max >= 7
    temp2 = detconsec2.Max >= 7
    temp3 = detconsec3.Max >= 7
    temp4 = detconsec4.Max >= 7
    
    # assign take off score
    consec.bysero.Max = data.frame(sum(temp1), sum(temp2), sum(temp3), sum(temp4))
    settingResults$scenarios.takeoff.summary.det[i, 2:5] <- consec.bysero.Max/500
    
    print("done detected")
    rm(scenario)
  }
  return(settingResults)
}
