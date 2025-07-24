# load packages
library(tidyverse)
library(rlist)
library(readxl)
library(doParallel)
numCores = detectCores()
registerDoParallel(numCores)

# load data
source("dengue_parexp_quart.R")
imm_settings = read_xlsx('parexp_values.xlsx',
                         sheet = 'imm_settings')
trans_scenarios = read_xlsx('parexp_values.xlsx',
                            sheet = 'trans_scenarios')

# Setting and scenarios to run
immSetting = 2
scenarioNum = 1

# immunity setting
imm_vals = imm_settings[immSetting,] # population immune to X serotype

# population set up
repeats = 500
parameters = list()

N = 100000
birthrate = 0
deathrate = 0
parameters$births = birthrate
parameters$deaths = deathrate

parameters$time_steps = 365

parameters$n_contacts = 2
parameters$pr_recovery = rep(1 - exp(-(1/7)), 4)
parameters$pr_protect = 1 - exp(-(1/180))
parameters$pr_det = 1
parameters$ade_detect = 1

parameters$immuneList = c(imm_vals$Var1, imm_vals$Var2, imm_vals$Var3, imm_vals$Var4)
pr_transmission = c(trans_scenarios[scenarioNum,1],
                    trans_scenarios[scenarioNum,2],
                    trans_scenarios[scenarioNum,3],
                    trans_scenarios[scenarioNum,4])
parameters$pr_transmission = pr_transmission

# characteristic of each person
popProperties = lapply(1:N, function(n) {
  list(
    "id" = n,
    "status" = 0,
    "serotype1" = 0,
    "serotype2" = 0,
    "serotype3" = 0,
    "serotype4" = 0,
    "status_seroresponse" = 1,
    "sero1transmission" = unname(pr_transmission[[1]]),
    "sero2transmission" = unname(pr_transmission[[2]]),
    "sero3transmission" = unname(pr_transmission[[3]]),
    "sero4transmission" = unname(pr_transmission[[4]])
  )
})

# updating parameters list
parameters$N_parameters = popProperties

# run simulations
print(paste0("scenario number: ", scenarioNum, " start"))

results = foreach (n = 1:repeats, .packages = 'rlist') %dopar% {
  test = simulate_model(parameters)
  return(test)
}

print(paste0("scenario number: ", scenarioNum, " end"))

saveRDS(results, file = paste0("dp_quartDetect_", immSetting, "_", scenarioNum, ".RDS"))
