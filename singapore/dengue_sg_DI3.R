# load packages
library(tidyverse)
library(rlist)
library(readxl)
library(doParallel)
numCores = detectCores()
registerDoParallel(numCores)

# load data
source("dengue_sg_quart.R")
pop_char = read_xlsx('sg_pop_scenarios.xlsx',
                     sheet = 'pop_char')
imm_setting = read_xlsx('dengue_sg_immunity.xlsx',
                         sheet = 'DI3')
trans_scenarios = read_xlsx('sg_pop_scenarios.xlsx',
                            sheet = 'trans_scenarios')

# transmission scenario to run
scenarioNum = 1

# population set up
repeats = 500
parameters = list()

N = 100000
pop = pop_char$pop
age = pop_char$age
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

parameters$immuneList = as.data.frame(imm_setting[,5:15])

pr_transmission = c(trans_scenarios[scenarioNum,1],
                    trans_scenarios[scenarioNum,2],
                    trans_scenarios[scenarioNum,3],
                    trans_scenarios[scenarioNum,4])
parameters$pr_transmission = pr_transmission

# characteristic of each person
popProperties = lapply(1:N, function(n) {
  list(
    "age" = age[n],
    "ageGroup" = pop[n],
    "status" = 0,
    "status_check" = 0,
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

saveRDS(results, file = paste0("dpsg_quart_", scenarioNum, ".RDS"))
