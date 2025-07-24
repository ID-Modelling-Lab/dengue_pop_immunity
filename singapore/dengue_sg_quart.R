library(doParallel)
library(foreach)
library(tictoc)
library(rlist)

# storage for tictoc checks
a=0

simulate_model = function(parameters){
  
  # Unpacking the parameters input
  births = parameters$births # birth rate
  deaths = parameters$deaths # death rate
  time_steps = parameters$time_steps # no. of days for simulation
  n_contacts = parameters$n_contacts # no. of contacts per day each person has
  pr_recovery =parameters$pr_recovery # each value corresponds to probability of recovery during sero 1-4 infection
  pr_protect = parameters$pr_protect # probability of becoming susceptible after period of cross-protection
  pr_det = parameters$pr_det # baseline probability of detection of a case
  ade_detect = parameters$ade_detect # relative probability of detection for secondary cases
  pr_transmission = parameters$pr_transmission # each value corresponds to trans probability of sero 1-4
  immuneList = parameters$immuneList # each value corresponds to pop prop with immunity to sero 1-4
  N_parameters = parameters$N_parameters # list containing details of each individual in the population of size N
  N = length(N_parameters) # population size
  
  # Creating vectors from N_parameters input
  age = unlist(list.map(N_parameters, age), use.names=FALSE)
  ageGroup = unlist(list.map(N_parameters, ageGroup), use.names=FALSE)
  status = as.numeric(unlist(list.map(N_parameters, status), use.names=FALSE))
  status_check = as.numeric(unlist(list.map(N_parameters, status_check), use.names=FALSE))
  sero1transmission = unlist(list.map(N_parameters, sero1transmission), use.names=FALSE)
  sero2transmission = unlist(list.map(N_parameters, sero2transmission), use.names=FALSE)
  sero3transmission = unlist(list.map(N_parameters, sero3transmission), use.names=FALSE)
  sero4transmission = unlist(list.map(N_parameters, sero4transmission), use.names=FALSE)
  
  ## Setting up population immunity
  lapply(1:11, function(ageGrp) {
    age_status_index = which(ageGroup == ageGrp & status == 0)
    
    # assigning primary
    selected_immune_1 = sample(age_status_index, floor(length(age_status_index) * immuneList[ageGrp,1]))
    selected_immune_2 = sample(age_status_index, floor(length(age_status_index) * immuneList[ageGrp,2]))
    selected_immune_3 = sample(age_status_index, floor(length(age_status_index) * immuneList[ageGrp,3]))
    selected_immune_4 = sample(age_status_index, floor(length(age_status_index) * immuneList[ageGrp,4]))
    
    status[selected_immune_1] <<- 9
    status[selected_immune_2] <<- 10
    status[selected_immune_3] <<- 11
    status[selected_immune_4] <<- 12
    
    # assigning secondary
    selected_immune_12 = sample(age_status_index, floor(length(age_status_index) * immuneList[ageGrp,5]))
    immune_alloc = sample(c(37,40), length(selected_immune_12), replace = TRUE)
    status[selected_immune_12] <<- immune_alloc
    
    selected_immune_13 = sample(age_status_index, floor(length(age_status_index) * immuneList[ageGrp,6]))
    immune_alloc = sample(c(38,43), length(selected_immune_13), replace = TRUE)
    status[selected_immune_13] <<- immune_alloc
    
    selected_immune_14 = sample(age_status_index, floor(length(age_status_index) * immuneList[ageGrp,7]))
    immune_alloc = sample(c(39,46), length(selected_immune_14), replace = TRUE)
    status[selected_immune_14] <<- immune_alloc
    
    selected_immune_24 = sample(age_status_index, floor(length(age_status_index) * immuneList[ageGrp,8]))
    immune_alloc = sample(c(42,47), length(selected_immune_24), replace = TRUE)
    status[selected_immune_24] <<- immune_alloc

    # assigning tertiary
    selected_immune_123 = sample(age_status_index, floor(length(age_status_index) * immuneList[ageGrp,9]))
    immune_alloc = sample(c(97,99,103,105,109,111), length(selected_immune_123), replace = TRUE)
    status[selected_immune_123] <<- immune_alloc
    
    selected_immune_124 = sample(age_status_index, floor(length(age_status_index) * immuneList[ageGrp,10]))
    immune_alloc = sample(c(98,101,104,107,115,117), length(selected_immune_124), replace = TRUE)
    status[selected_immune_124] <<- immune_alloc
    
    # assigning quaternary
    selected_immune_1234 = sample(age_status_index, floor(length(age_status_index) * immuneList[ageGrp,11]))
    status[selected_immune_1234] <<- 145

  })
  
  # checking allocation
  status_check <- status
  
  ## Seeding index cases for each serotype -- we'll pick 4 individuals to infect, 1 with each serotype
  # sample from totally susceptible individuals
  strain1_patientzero = sample(which(status == 0), 1)
  status[strain1_patientzero] = 1
  
  strain2_patientzero = sample(which(status == 0), 1)
  status[strain2_patientzero] = 2
  
  strain3_patientzero = sample(which(status == 0), 1)
  status[strain3_patientzero] = 3

  strain4_patientzero = sample(which(status == 0), 1)
  status[strain4_patientzero] = 4
  
  ### Initialising some states that we want to track
  S_t = numeric(time_steps+1) # Susceptible
  R_t = numeric(time_steps+1) # infected twice
  deaths_t = numeric(time_steps+1)
  
  sum_I1 = numeric(time_steps+1) # total serotype 1 infections
  sum_I2 = numeric(time_steps+1) # total serotype 2 infections
  sum_I3 = numeric(time_steps+1) # total serotype 3 infections
  sum_I4 = numeric(time_steps+1) # total serotype 4 infections
  
  I1 = list(
    "1" = numeric(time_steps+1),
    "2" = numeric(time_steps+1),
    "3" = numeric(time_steps+1),
    "4" = numeric(time_steps+1),
    "5" = numeric(time_steps+1),
    "6" = numeric(time_steps+1),
    "7" = numeric(time_steps+1),
    "8" = numeric(time_steps+1),
    "9" = numeric(time_steps+1),
    "10" = numeric(time_steps+1),
    "11" = numeric(time_steps+1)
  )
  
  I2 = list(
    "1" = numeric(time_steps+1),
    "2" = numeric(time_steps+1),
    "3" = numeric(time_steps+1),
    "4" = numeric(time_steps+1),
    "5" = numeric(time_steps+1),
    "6" = numeric(time_steps+1),
    "7" = numeric(time_steps+1),
    "8" = numeric(time_steps+1),
    "9" = numeric(time_steps+1),
    "10" = numeric(time_steps+1),
    "11" = numeric(time_steps+1)
  )
  
  I3 = list(
    "1" = numeric(time_steps+1),
    "2" = numeric(time_steps+1),
    "3" = numeric(time_steps+1),
    "4" = numeric(time_steps+1),
    "5" = numeric(time_steps+1),
    "6" = numeric(time_steps+1),
    "7" = numeric(time_steps+1),
    "8" = numeric(time_steps+1),
    "9" = numeric(time_steps+1),
    "10" = numeric(time_steps+1),
    "11" = numeric(time_steps+1)
  )
  
  I4 = list(
    "1" = numeric(time_steps+1),
    "2" = numeric(time_steps+1),
    "3" = numeric(time_steps+1),
    "4" = numeric(time_steps+1),
    "5" = numeric(time_steps+1),
    "6" = numeric(time_steps+1),
    "7" = numeric(time_steps+1),
    "8" = numeric(time_steps+1),
    "9" = numeric(time_steps+1),
    "10" = numeric(time_steps+1),
    "11" = numeric(time_steps+1)
  )
  
  newI1 = list(
    "1" = numeric(time_steps+1),
    "2" = numeric(time_steps+1),
    "3" = numeric(time_steps+1),
    "4" = numeric(time_steps+1),
    "5" = numeric(time_steps+1),
    "6" = numeric(time_steps+1),
    "7" = numeric(time_steps+1),
    "8" = numeric(time_steps+1),
    "9" = numeric(time_steps+1),
    "10" = numeric(time_steps+1),
    "11" = numeric(time_steps+1)
  )
  
  newI2 = list(
    "1" = numeric(time_steps+1),
    "2" = numeric(time_steps+1),
    "3" = numeric(time_steps+1),
    "4" = numeric(time_steps+1),
    "5" = numeric(time_steps+1),
    "6" = numeric(time_steps+1),
    "7" = numeric(time_steps+1),
    "8" = numeric(time_steps+1),
    "9" = numeric(time_steps+1),
    "10" = numeric(time_steps+1),
    "11" = numeric(time_steps+1)
  )
  
  newI3 = list(
    "1" = numeric(time_steps+1),
    "2" = numeric(time_steps+1),
    "3" = numeric(time_steps+1),
    "4" = numeric(time_steps+1),
    "5" = numeric(time_steps+1),
    "6" = numeric(time_steps+1),
    "7" = numeric(time_steps+1),
    "8" = numeric(time_steps+1),
    "9" = numeric(time_steps+1),
    "10" = numeric(time_steps+1),
    "11" = numeric(time_steps+1)
  )
  
  newI4 = list(
    "1" = numeric(time_steps+1),
    "2" = numeric(time_steps+1),
    "3" = numeric(time_steps+1),
    "4" = numeric(time_steps+1),
    "5" = numeric(time_steps+1),
    "6" = numeric(time_steps+1),
    "7" = numeric(time_steps+1),
    "8" = numeric(time_steps+1),
    "9" = numeric(time_steps+1),
    "10" = numeric(time_steps+1),
    "11" = numeric(time_steps+1)
  )
  
  detectedI1 = list(
    "1" = numeric(time_steps+1),
    "2" = numeric(time_steps+1),
    "3" = numeric(time_steps+1),
    "4" = numeric(time_steps+1),
    "5" = numeric(time_steps+1),
    "6" = numeric(time_steps+1),
    "7" = numeric(time_steps+1),
    "8" = numeric(time_steps+1),
    "9" = numeric(time_steps+1),
    "10" = numeric(time_steps+1),
    "11" = numeric(time_steps+1)
  )
  
  detectedI2 = list(
    "1" = numeric(time_steps+1),
    "2" = numeric(time_steps+1),
    "3" = numeric(time_steps+1),
    "4" = numeric(time_steps+1),
    "5" = numeric(time_steps+1),
    "6" = numeric(time_steps+1),
    "7" = numeric(time_steps+1),
    "8" = numeric(time_steps+1),
    "9" = numeric(time_steps+1),
    "10" = numeric(time_steps+1),
    "11" = numeric(time_steps+1)
  )
  
  detectedI3 = list(
    "1" = numeric(time_steps+1),
    "2" = numeric(time_steps+1),
    "3" = numeric(time_steps+1),
    "4" = numeric(time_steps+1),
    "5" = numeric(time_steps+1),
    "6" = numeric(time_steps+1),
    "7" = numeric(time_steps+1),
    "8" = numeric(time_steps+1),
    "9" = numeric(time_steps+1),
    "10" = numeric(time_steps+1),
    "11" = numeric(time_steps+1)
  )
  
  detectedI4 = list(
    "1" = numeric(time_steps+1),
    "2" = numeric(time_steps+1),
    "3" = numeric(time_steps+1),
    "4" = numeric(time_steps+1),
    "5" = numeric(time_steps+1),
    "6" = numeric(time_steps+1),
    "7" = numeric(time_steps+1),
    "8" = numeric(time_steps+1),
    "9" = numeric(time_steps+1),
    "10" = numeric(time_steps+1),
    "11" = numeric(time_steps+1)
  )

  tic("1 simulation run time")
  # starting iterations here
  out = tryCatch({
    for(tt in 1:(time_steps+1)) {
      
      ## Storing values here
      selected = sample(which(status != 146), deaths)
      status[selected] = 146
      deaths_t[tt] = deaths
      
      N = N + births
      
      ## adding new births
      if(births > 0) {
        # adding all the new births into our N_parameters' parameters
        age = c(age, rep(0, births)) 
        ageGroup = c(ageGroup, rep(1, births))
        status = c(status, rep(0, births))
        sero1transmission = c(sero1transmission, rep(pr_transmission[1], births))
        sero2transmission = c(sero2transmission, rep(pr_transmission[2], births))
        sero3transmission = c(sero3transmission, rep(pr_transmission[3], births))
        sero4transmission = c(sero4transmission, rep(pr_transmission[4], births))
      }
      # status = c(status, rep(0, births))
      S_t[tt] = sum(status == 0)
      
      
      total1status = status == 1 | status == 16 | status == 19 | status == 22 |
        status == 57 | status == 59 | status == 63 | status == 65 | status == 69 | status == 71 |
        status == 130 | status == 132 | status == 136 | status == 138 | status == 142 | status == 144
      
      total2status = status == 2 | status == 13 | status == 20 | status == 23 |
        status == 51 | status == 53 | status == 61 | status == 66 | status == 67 | status == 72 |
        status == 124 | status == 126 | status == 134 | status == 137 | status == 140 | status == 143
      
      total3status = status == 3 | status == 14 | status == 17 | status == 24 |
        status == 49 | status == 54 | status == 55 | status == 60 | status == 68 | status == 70 |
        status == 122 | status == 125 | status == 128 | status == 131 | status == 139 | status == 141
      
      total4status = status == 4 | status == 15 | status == 18 | status == 21 |
        status == 50 | status == 52 | status == 56 | status == 58 | status == 62 | status == 64 |
        status == 121 | status == 123 | status == 127 | status == 129 | status == 133 | status == 135
      
      lapply(1:11, function(ageGrp) {
        I1[[ageGrp]][tt] <<- sum(total1status & ageGroup == ageGrp)
        I2[[ageGrp]][tt] <<- sum(total2status & ageGroup == ageGrp)
        I3[[ageGrp]][tt] <<- sum(total3status & ageGroup == ageGrp)
        I4[[ageGrp]][tt] <<- sum(total4status & ageGroup == ageGrp)
      })
      
      sum_I1[tt] = sum(total1status)
      sum_I2[tt] = sum(total2status)
      sum_I3[tt] = sum(total3status)
      sum_I4[tt] = sum(total4status)
      
      # N_r = N - deaths 
      
      ## We'll do all the stochastic processes here and resolve the status changes later
  
      ## (1) Successful contact
      # Each individual contacts 'n_contacts' other individuals in the population
      # We can sample from the multinomial to get the no. of contacts they make with an infected person
      
      contact_success = rmultinom(N, n_contacts, c(sum_I1[tt]/N, 
                                                   sum_I2[tt]/N, 
                                                   sum_I3[tt]/N, 
                                                   sum_I4[tt]/N, 
                                                   (N - sum_I1[tt] - sum_I2[tt] - sum_I3[tt] - sum_I4[tt])/N))
      
      # First row indicates the number of successful contacts with sero 1
      # Second row indicates the number of successful contacts with sero 2
      # Third row indicates the number of successful contacts with sero 3
      # Fourth row indicates the number of successful contacts with sero 4
      # Fifth row indicates the number of successful contacts with non-infecteds
      
      ## (2) Successful transmission
      # Each successful contact with an infected person has a certain prob of infecting the susceptible
      # We can sample from a binomial to simulate this. 
      # No. of trials equals the number of successful contacts from earlier. 
      # So for sero 1, we'll take the first row of contact_success. For sero 2, we'll take the second row.
      transmission1 = rbinom(N, contact_success[1,], sero1transmission) # primary sero 1
      transmission2 = rbinom(N, contact_success[2,], sero2transmission) # primary sero 2
      transmission3 = rbinom(N, contact_success[3,], sero3transmission) # primary sero 3
      transmission4 = rbinom(N, contact_success[4,], sero4transmission) # primary sero 4
      
      ## (3) Successful recoveries
      # Similar to infections, we can sample from the binomial
      recover1 = rbinom(N, 1, pr_recovery[1]) # second value refers to number of trials - only 1 in this case
      recover2 = rbinom(N, 1, pr_recovery[2])
      recover3 = rbinom(N, 1, pr_recovery[3])
      recover4 = rbinom(N, 1, pr_recovery[4])
    
      ## (4) Successful move from cross-protected to susceptible after 1st infection
      no_protection = rbinom(N, 1, pr_protect)
      
      ## We'll resolve the status changes here. We'll check for recoveries before infections. 
      # This prevents people from recovering from an infection they just received.
      # We resolve cross-protection before infections
      
      ## (5) Resolving recoveries
      status = case_when(
        status == 1 & recover1 == 1 ~ 5, # Recovery from sero 1 (primary)
        status == 2 & recover2 == 1 ~ 6, # Recovery from sero 2 (primary)
        status == 3 & recover3 == 1 ~ 7, # Recovery from sero 3 (primary)
        status == 4 & recover4 == 1 ~ 8, # Recovery from sero 4 (primary)
        status == 13 & recover2 == 1 ~ 25, # Recovery from 1 -> 2 (secondary)
        status == 14 & recover3 == 1 ~ 26, # Recovery from 1 -> 3 (secondary)
        status == 15 & recover4 == 1 ~ 27, # Recovery from 1 -> 4 (secondary)
        status == 16 & recover1 == 1 ~ 28, # Recovery from 2 -> 1 (secondary)
        status == 17 & recover3 == 1 ~ 29, # Recovery from 2 -> 3 (secondary)
        status == 18 & recover4 == 1 ~ 30, # Recovery from 2 -> 4 (secondary)
        status == 19 & recover1 == 1 ~ 31, # Recovery from 3 -> 1 (secondary)
        status == 20 & recover2 == 1 ~ 32, # Recovery from 3 -> 2 (secondary)
        status == 21 & recover4 == 1 ~ 33, # Recovery from 3 -> 4 (secondary)
        status == 22 & recover1 == 1 ~ 34, # Recovery from 4 -> 1 (secondary)
        status == 23 & recover2 == 1 ~ 35, # Recovery from 4 -> 2 (secondary)
        status == 24 & recover3 == 1 ~ 36, # Recovery from 4 -> 3 (secondary)
        status == 49 & recover3 == 1 ~ 73,
        status == 50 & recover4 == 1 ~ 74,
        status == 51 & recover2 == 1 ~ 75,
        status == 52 & recover4 == 1 ~ 76,
        status == 53 & recover2 == 1 ~ 77,
        status == 54 & recover3 == 1 ~ 78,
        status == 55 & recover3 == 1 ~ 79,
        status == 56 & recover4 == 1 ~ 80,
        status == 57 & recover1 == 1 ~ 81,
        status == 58 & recover4 == 1 ~ 82,
        status == 59 & recover1 == 1 ~ 83,
        status == 60 & recover3 == 1 ~ 84,
        status == 61 & recover2 == 1 ~ 85,
        status == 62 & recover4 == 1 ~ 86,
        status == 63 & recover1 == 1 ~ 87,
        status == 64 & recover4 == 1 ~ 88,
        status == 65 & recover1 == 1 ~ 89,
        status == 66 & recover2 == 1 ~ 90,
        status == 67 & recover2 == 1 ~ 91,
        status == 68 & recover3 == 1 ~ 92,
        status == 69 & recover1 == 1 ~ 93,
        status == 70 & recover3 == 1 ~ 94,
        status == 71 & recover1 == 1 ~ 95,
        status == 72 & recover2 == 1 ~ 96,
        status == 121 & recover4 == 1 ~ 145,
        status == 122 & recover3 == 1 ~ 145,
        status == 123 & recover4 == 1 ~ 145,
        status == 124 & recover2 == 1 ~ 145,
        status == 125 & recover3 == 1 ~ 145,
        status == 126 & recover2 == 1 ~ 145,
        status == 127 & recover4 == 1 ~ 145,
        status == 128 & recover3 == 1 ~ 145,
        status == 129 & recover4 == 1 ~ 145,
        status == 130 & recover1 == 1 ~ 145,
        status == 131 & recover3 == 1 ~ 145,
        status == 132 & recover1 == 1 ~ 145,
        status == 133 & recover4 == 1 ~ 145,
        status == 134 & recover2 == 1 ~ 145,
        status == 135 & recover4 == 1 ~ 145,
        status == 136 & recover1 == 1 ~ 145,
        status == 137 & recover2 == 1 ~ 145,
        status == 138 & recover1 == 1 ~ 145,
        status == 139 & recover3 == 1 ~ 145,
        status == 140 & recover2 == 1 ~ 145,
        status == 141 & recover3 == 1 ~ 145,
        status == 142 & recover1 == 1 ~ 145,
        status == 143 & recover2 == 1 ~ 145,
        status == 144 & recover1 == 1 ~ 145,
        .default = status
      )
      
      ## (6) Resolving cross-protections
      status = case_when(
        status == 5 & no_protection == 1 ~ 9, # Recovery from sero 1 (primary)
        status == 6 & no_protection == 1 ~ 10, # Recovery from sero 2 (primary)
        status == 7 & no_protection == 1 ~ 11, # Recovery from sero 3 (primary)
        status == 8 & no_protection == 1 ~ 12, # Recovery from sero 4 (primary),
        status == 25 & no_protection == 1 ~ 37,
        status == 26 & no_protection == 1 ~ 38,
        status == 27 & no_protection == 1 ~ 39,
        status == 28 & no_protection == 1 ~ 40,
        status == 29 & no_protection == 1 ~ 41,
        status == 30 & no_protection == 1 ~ 42,
        status == 31 & no_protection == 1 ~ 43,
        status == 32 & no_protection == 1 ~ 44,
        status == 33 & no_protection == 1 ~ 45,
        status == 34 & no_protection == 1 ~ 46,
        status == 35 & no_protection == 1 ~ 47,
        status == 36 & no_protection == 1 ~ 48,
        status == 73 & no_protection == 1 ~ 97,
        status == 74 & no_protection == 1 ~ 98,
        status == 75 & no_protection == 1 ~ 99,
        status == 76 & no_protection == 1 ~ 100,
        status == 77 & no_protection == 1 ~ 101,
        status == 78 & no_protection == 1 ~ 102,
        status == 79 & no_protection == 1 ~ 103,
        status == 80 & no_protection == 1 ~ 104,
        status == 81 & no_protection == 1 ~ 105,
        status == 82 & no_protection == 1 ~ 106,
        status == 83 & no_protection == 1 ~ 107,
        status == 84 & no_protection == 1 ~ 108,
        status == 85 & no_protection == 1 ~ 109,
        status == 86 & no_protection == 1 ~ 110,
        status == 87 & no_protection == 1 ~ 111,
        status == 88 & no_protection == 1 ~ 112,
        status == 89 & no_protection == 1 ~ 113,
        status == 90 & no_protection == 1 ~ 114,
        status == 91 & no_protection == 1 ~ 115,
        status == 92 & no_protection == 1 ~ 116,
        status == 93 & no_protection == 1 ~ 117,
        status == 94 & no_protection == 1 ~ 118,
        status == 95 & no_protection == 1 ~ 119,
        status == 96 & no_protection == 1 ~ 120,
        .default = status
      )
      
      # We'll track the current number of infections at this point after all the recoveries
      # so we can calculate the number of new infections that happen in the next step
      curr_I1 = c()
      curr_I2 = c()
      curr_I3 = c()
      curr_I4 = c()
      
      curr_I1_ps = c()
      curr_I2_ps = c()
      curr_I3_ps = c()
      curr_I4_ps = c()
      
      lapply(1:11, function(ageGrp) {
        curr_I1[[ageGrp]] <<- sum((status == 1 | status == 16 | status == 19 | status == 22 |
                                     status == 57 | status == 59 | status == 63 | status == 65 | status == 69 | status == 71 |
                                     status == 130 | status == 132 | status == 136 | status == 138 | status == 142 | status == 144) & ageGroup == ageGrp)
        curr_I2[[ageGrp]] <<- sum((status == 2 | status == 13 | status == 20 | status == 23 |
                                     status == 51 | status == 53 | status == 61 | status == 66 | status == 67 | status == 72 |
                                     status == 124 | status == 126 | status == 134 | status == 137 | status == 140 | status == 143) & ageGroup == ageGrp)
        curr_I3[[ageGrp]] <<- sum((status == 3 | status == 14 | status == 17 | status == 24 |
                                     status == 49 | status == 54 | status == 55 | status == 60 | status == 68 | status == 70 |
                                     status == 122 | status == 125 | status == 128 | status == 131 | status == 139 | status == 141) & ageGroup == ageGrp)
        curr_I4[[ageGrp]] <<- sum((status == 4 | status == 15 | status == 18 | status == 21 |
                                     status == 50 | status == 52 | status == 56 | status == 58 | status == 62 | status == 64 |
                                     status == 121 | status == 123 | status == 127 | status == 129 | status == 133 | status == 135) & ageGroup == ageGrp)
        
        curr_I1_ps[[ageGrp]] <<- sum((status == 1 | status == 16 | status == 19 | status == 22) & ageGroup == ageGrp)
        curr_I2_ps[[ageGrp]] <<- sum((status == 2 | status == 13 | status == 20 | status == 23) & ageGroup == ageGrp)
        curr_I3_ps[[ageGrp]] <<- sum((status == 3 | status == 14 | status == 17 | status == 24) & ageGroup == ageGrp)
        curr_I4_ps[[ageGrp]] <<- sum((status == 4 | status == 15 | status == 18 | status == 21) & ageGroup == ageGrp)
      })

      # Resolving co-inections - randomly select one to 'win'
      co_infect1234 = which(transmission1 == 1 & transmission2 == 1 & transmission3 == 1 & transmission4 == 1)
      co_infect123 = which(transmission1 == 1 & transmission2 == 1 & transmission3 == 1 & transmission4 == 0)
      co_infect124 = which(transmission1 == 1 & transmission2 == 1 & transmission3 == 0 & transmission4 == 1)
      co_infect234 = which(transmission1 == 0 & transmission2 == 1 & transmission3 == 1 & transmission4 == 1)
      co_infect134 = which(transmission1 == 1 & transmission2 == 0 & transmission3 == 1 & transmission4 == 1)
      
      co_infect12 = which(transmission1 == 1 & transmission2 == 1 & transmission3 == 0 & transmission4 == 0)
      co_infect13 = which(transmission1 == 1 & transmission3 == 1 & transmission2 == 0 & transmission4 == 0)
      co_infect14 = which(transmission1 == 1 & transmission4 == 1 & transmission2 == 0 & transmission3 == 0)
      
      co_infect23 = which(transmission2 == 1 & transmission3 == 1 & transmission1 == 0 & transmission4 == 0)
      co_infect24 = which(transmission2 == 1 & transmission4 == 1 & transmission1 == 0 & transmission3 == 0)
      
      co_infect34 = which(transmission3 == 1 & transmission4 == 1 & transmission1 == 0 & transmission2 == 0)
      
      status[co_infect1234] = ifelse(status[co_infect1234] == 0,
                                     sample(1:4, N, replace = TRUE), 
                                     status[co_infect1234])
      
      status[co_infect123] = ifelse(status[co_infect123] == 0,
                                    sample(1:3, N, replace = TRUE),
                                    ifelse(status[co_infect123] == 12,
                                           sample(22:24, N, replace = TRUE),
                                           status[co_infect123]
                                    ))
      
      status[co_infect124] = ifelse(status[co_infect124] == 0,
                                    sample(c(1,2,4), N, replace = TRUE),
                                    ifelse(status[co_infect124] == 11,
                                           sample(19:21, N, replace = TRUE),
                                           status[co_infect124]
                                    ))
      
      status[co_infect234] = ifelse(status[co_infect234] == 0,
                                    sample(c(2,3,4), N, replace = TRUE),
                                    ifelse(status[co_infect234] == 9,
                                           sample(13:15, N, replace = TRUE),
                                           status[co_infect234]
                                    ))
      
      status[co_infect134] = ifelse(status[co_infect134] == 0,
                                    sample(c(1,3,4), N, replace = TRUE),
                                    ifelse(status[co_infect134] == 10,
                                           sample(16:18, N, replace = TRUE),
                                           status[co_infect134]
                                    ))
      
      status[co_infect12] = ifelse(status[co_infect12] == 0,
                                   sample(1:2, N, replace = TRUE),
                                   ifelse(status[co_infect12] == 11,
                                          sample(19:20, N, replace = TRUE),
                                          ifelse(status[co_infect12] == 12,
                                                 sample(22:23, N, replace = TRUE),
                                                 ifelse(status[co_infect12] == 45,
                                                        sample(65:66, N, replace = TRUE),
                                                        ifelse(status[co_infect12] == 48,
                                                               sample(71:72, N, replace = TRUE),
                                                               status[co_infect12])))
                                   ))
      
      status[co_infect13] = ifelse(status[co_infect13] == 0,
                                   sample(c(1,3), N, replace = TRUE),
                                   ifelse(status[co_infect13] == 10,
                                          sample(c(16,17), N, replace = TRUE),
                                          ifelse(status[co_infect13] == 12,
                                                 sample(c(22,24), N, replace = TRUE),
                                                 ifelse(status[co_infect13] == 42,
                                                        sample(59:60, N, replace = TRUE),
                                                        ifelse(status[co_infect13] == 47,
                                                               sample(69:70, N, replace = TRUE),
                                                               status[co_infect13])))
                                   ))
      
      status[co_infect14] = ifelse(status[co_infect14] == 0,
                                   sample(c(1,4), N, replace = TRUE),
                                   ifelse(status[co_infect14] == 10,
                                          sample(c(16,18), N, replace = TRUE),
                                          ifelse(status[co_infect14] == 11,
                                                 sample(c(19,21), N, replace = TRUE),
                                                 ifelse(status[co_infect14] == 41,
                                                        sample(c(57,58), N, replace = TRUE),
                                                        ifelse(status[co_infect14] == 44,
                                                               sample(c(63,64), N, replace = TRUE),
                                                               status[co_infect14])))
                                   ))
      
      status[co_infect23] = ifelse(status[co_infect23] == 0,
                                   sample(c(2,3), N, replace = TRUE),
                                   ifelse(status[co_infect23] == 9,
                                          sample(c(13,14), N, replace = TRUE),
                                          ifelse(status[co_infect23] == 12,
                                                 sample(c(23,24), N, replace = TRUE),
                                                 ifelse(status[co_infect23] == 39,
                                                        sample(c(53,54), N, replace = TRUE),
                                                        ifelse(status[co_infect23] == 46,
                                                               sample(c(67,68), N, replace = TRUE),
                                                               status[co_infect23])))
                                   ))
      
      status[co_infect24] = ifelse(status[co_infect24] == 0,
                                   sample(c(2,4), N, replace = TRUE),
                                   ifelse(status[co_infect24] == 9,
                                          sample(c(13,15), N, replace = TRUE),
                                          ifelse(status[co_infect24] == 11,
                                                 sample(c(20,21), N, replace = TRUE),
                                                 ifelse(status[co_infect24] == 38,
                                                        sample(c(51,52), N, replace = TRUE),
                                                        ifelse(status[co_infect24] == 43,
                                                               sample(c(61,62), N, replace = TRUE),
                                                               status[co_infect24])))
                                   ))
      
      status[co_infect34] = ifelse(status[co_infect34] == 0,
                                   sample(c(3,4), N, replace = TRUE),
                                   ifelse(status[co_infect34] == 9,
                                          sample(c(14,15), N, replace = TRUE),
                                          ifelse(status[co_infect34] == 10,
                                                 sample(c(17,18), N, replace = TRUE),
                                                 ifelse(status[co_infect34] == 37,
                                                        sample(c(49,50), N, replace = TRUE),
                                                        ifelse(status[co_infect34] == 40,
                                                               sample(c(55,56), N, replace = TRUE),
                                                               status[co_infect34])))
                                   ))
      
      # And now we resolve the remaining infections
      status = case_when(
        transmission1 == 1 & status == 0 ~ 1, # sero 1 primary infection
        transmission2 == 1 & status == 0 ~ 2, # sero 2 primary infection
        transmission3 == 1 & status == 0 ~ 3, # sero 3 primary infection
        transmission4 == 1 & status == 0 ~ 4, # sero 4 primary infection
        transmission2 == 1 & status == 9 ~ 13, # 1->2
        transmission3 == 1 & status == 9 ~ 14, # 1->3
        transmission4 == 1 & status == 9 ~ 15, # 1->4
        transmission1 == 1 & status == 10 ~ 16, # 2->1
        transmission3 == 1 & status == 10 ~ 17, # 2->3
        transmission4 == 1 & status == 10 ~ 18, # 2->4
        transmission1 == 1 & status == 11 ~ 19, # 3->1
        transmission2 == 1 & status == 11 ~ 20, # 3->2
        transmission4 == 1 & status == 11 ~ 21, # 3->4
        transmission1 == 1 & status == 12 ~ 22, # 4->1
        transmission2 == 1 & status == 12 ~ 23, # 4->2
        transmission3 == 1 & status == 12 ~ 24, # 4->3
        transmission3 == 1 & status == 37 ~ 49,
        transmission4 == 1 & status == 37 ~ 50,
        transmission2 == 1 & status == 38 ~ 51,
        transmission4 == 1 & status == 38 ~ 52,
        transmission2 == 1 & status == 39 ~ 53,
        transmission3 == 1 & status == 39 ~ 54,
        transmission3 == 1 & status == 40 ~ 55,
        transmission4 == 1 & status == 40 ~ 56,
        transmission1 == 1 & status == 41 ~ 57,
        transmission4 == 1 & status == 41 ~ 58,
        transmission1 == 1 & status == 42 ~ 59,
        transmission3 == 1 & status == 42 ~ 60,
        transmission2 == 1 & status == 43 ~ 61,
        transmission4 == 1 & status == 43 ~ 62,
        transmission1 == 1 & status == 44 ~ 63,
        transmission4 == 1 & status == 44 ~ 64,
        transmission1 == 1 & status == 45 ~ 65,
        transmission2 == 1 & status == 45 ~ 66,
        transmission2 == 1 & status == 46 ~ 67,
        transmission3 == 1 & status == 46 ~ 68,
        transmission1 == 1 & status == 47 ~ 69,
        transmission3 == 1 & status == 47 ~ 70,
        transmission1 == 1 & status == 48 ~ 71,
        transmission2 == 1 & status == 48 ~ 72,
        transmission4 == 1 & status == 97 ~ 121,
        transmission3 == 1 & status == 98 ~ 122,
        transmission4 == 1 & status == 99 ~ 123,
        transmission2 == 1 & status == 100 ~ 124,
        transmission3 == 1 & status == 101 ~ 125,
        transmission2 == 1 & status == 102 ~ 126,
        transmission4 == 1 & status == 103 ~ 127,
        transmission3 == 1 & status == 104 ~ 128,
        transmission4 == 1 & status == 105 ~ 129,
        transmission1 == 1 & status == 106 ~ 130,
        transmission3 == 1 & status == 107 ~ 131,
        transmission1 == 1 & status == 108 ~ 132,
        transmission4 == 1 & status == 109 ~ 133,
        transmission2 == 1 & status == 110 ~ 134,
        transmission4 == 1 & status == 111 ~ 135,
        transmission1 == 1 & status == 112 ~ 136,
        transmission2 == 1 & status == 113 ~ 137,
        transmission1 == 1 & status == 114 ~ 138,
        transmission3 == 1 & status == 115 ~ 139,
        transmission2 == 1 & status == 116 ~ 140,
        transmission3 == 1 & status == 117 ~ 141,
        transmission1 == 1 & status == 118 ~ 142,
        transmission2 == 1 & status == 119 ~ 143,
        transmission1 == 1 & status == 120 ~ 144,
        .default = status
      )
      
      # Calculating how many new infections we got by subtracting from the earlier stored value
      lapply(1:11, function(ageGrp) {
        newI1[[ageGrp]][tt] <<- sum((status == 1 | status == 16 | status == 19 | status == 22 |
                                       status == 57 | status == 59 | status == 63 | status == 65 | status == 69 | status == 71 |
                                       status == 130 | status == 132 | status == 136 | status == 138 | status == 142 | status == 144) & ageGroup == ageGrp) - curr_I1[[ageGrp]]
        newI2[[ageGrp]][tt] <<- sum((status == 2 | status == 13 | status == 20 | status == 23 |
                                       status == 51 | status == 53 | status == 61 | status == 66 | status == 67 | status == 72 |
                                       status == 124 | status == 126 | status == 134 | status == 137 | status == 140 | status == 143) & ageGroup == ageGrp) - curr_I2[[ageGrp]]
        newI3[[ageGrp]][tt] <<- sum((status == 3 | status == 14 | status == 17 | status == 24 |
                                       status == 49 | status == 54 | status == 55 | status == 60 | status == 68 | status == 70 |
                                       status == 122 | status == 125 | status == 128 | status == 131 | status == 139 | status == 141) & ageGroup == ageGrp) - curr_I3[[ageGrp]]
        newI4[[ageGrp]][tt] <<- sum((status == 4 | status == 15 | status == 18 | status == 21 |
                                       status == 50 | status == 52 | status == 56 | status == 58 | status == 62 | status == 64 |
                                       status == 121 | status == 123 | status == 127 | status == 129 | status == 133 | status == 135) & ageGroup == ageGrp) - curr_I4[[ageGrp]]
        
        detectedI1[[ageGrp]][tt] <<- sum((status == 1 | status == 16 | status == 19 | status == 22) & ageGroup == ageGrp) - curr_I1_ps[[ageGrp]]
        detectedI2[[ageGrp]][tt] <<- sum((status == 2 | status == 13 | status == 20 | status == 23) & ageGroup == ageGrp) - curr_I2_ps[[ageGrp]]
        detectedI3[[ageGrp]][tt] <<- sum((status == 3 | status == 14 | status == 17 | status == 24) & ageGroup == ageGrp) - curr_I3_ps[[ageGrp]]
        detectedI4[[ageGrp]][tt] <<- sum((status == 4 | status == 15 | status == 18 | status == 21) & ageGroup == ageGrp) - curr_I4_ps[[ageGrp]]
      })

      print(tt)
    }
  },
  error=function(cond) {
    message(paste0("error ", cond))
  },
  warning=function(cond) {
    message(paste0("warning ", cond))
  },
  finally={
    message(paste0("finally"))
  }
  )
  N_parameters$status = status
  
  a = toc()
  # Returning the simulation result
  
  # results of the simulation
  res = data.frame(time = 0:time_steps,
                   # sum.I1 = sum_I1,
                   # sum.I2 = sum_I2,
                   # sum.I3 = sum_I3,
                   # sum.I4 = sum_I4,
                   age_I1 = I1,
                   age_I2 = I2,
                   age_I3 = I3,
                   age_I4 = I4,
                   new_age_I1 = newI1,
                   new_age_I2 = newI2,
                   new_age_I3 = newI3,
                   new_age_I4 = newI4,
                   detected.I1 = detectedI1,
                   detected.I2 = detectedI2,
                   detected.I3 = detectedI3,
                   detected.I4 = detectedI4
  )
  
  # parameters$N_parameters = length(status)
  
  output = list(
    "res" = res,
    "status_start" = status_check
  )
  
  return(output)
}
