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
  status = as.numeric(unlist(list.map(N_parameters, status), use.names=FALSE))
  serotype1 = unlist(list.map(N_parameters, serotype1), use.names=FALSE)
  serotype2 = unlist(list.map(N_parameters, serotype2), use.names=FALSE)
  serotype3 = unlist(list.map(N_parameters, serotype3), use.names=FALSE)
  serotype4 = unlist(list.map(N_parameters, serotype4), use.names=FALSE)
  sero1transmission = unlist(list.map(N_parameters, sero1transmission), use.names=FALSE)
  sero2transmission = unlist(list.map(N_parameters, sero2transmission), use.names=FALSE)
  sero3transmission = unlist(list.map(N_parameters, sero3transmission), use.names=FALSE)
  sero4transmission = unlist(list.map(N_parameters, sero4transmission), use.names=FALSE)
  
  ### We'll create a vector to store the statuses of N people
  ## Status numbers:
  # 0: Susceptible to all seros
  # 1: Infected by sero 1
  # 2: Infected by sero 2
  # 3: Infected by sero 3
  # 4: Infected by sero 4
  # 5: Cross protected, past infection w/ 1
  # 6: Cross protected, past infection w/ 2
  # 7: Cross protected, past infection w/ 3
  # 8: Cross protected, past infection w/ 4
  # 9: Susceptible, immune to sero 1
  # 10: Susceptible, immune to sero 2
  # 11: Susceptible, immune to sero 3
  # 12: Susceptible, immune to sero 4
  # 13: Infected by 1 -> 2
  # 14: Infected by 1 -> 3
  # 15: Infected by 1 -> 4
  # 16: Infected by 2 -> 1
  # 17: Infected by 2 -> 3
  # 18: Infected by 2 -> 4
  # 19: Infected by 3 -> 1
  # 20: Infected by 3 -> 2
  # 21: Infected by 3 -> 4
  # 22: Infected by 4 -> 1
  # 23: Infected by 4 -> 2
  # 24: Infected by 4 -> 3
  # 25: Fully immune
  # 26: Dead -- equal chance for everyone to die at each status
  
  ## Setting up population immunity
  selected_immune_1 = sample(N, floor(immuneList[1]*N))
  selected_immune_2 = sample(N, floor(immuneList[2]*N))
  selected_immune_3 = sample(N, floor(immuneList[3]*N))
  selected_immune_4 = sample(N, floor(immuneList[4]*N))

  status[selected_immune_1] = 9
  status[selected_immune_2] = 10
  status[selected_immune_3] = 11
  status[selected_immune_4] = 12
  
  # Immune to 2 serotypes
  immune_12 = intersect(selected_immune_1, selected_immune_2)
  immune_13 = intersect(selected_immune_1, selected_immune_3)
  immune_14 = intersect(selected_immune_1, selected_immune_4)
  immune_23 = intersect(selected_immune_2, selected_immune_3)
  immune_24 = intersect(selected_immune_2, selected_immune_4)
  immune_34 = intersect(selected_immune_3, selected_immune_4)
  
  status[immune_12] = 25
  status[immune_13] = 25
  status[immune_14] = 25
  status[immune_23] = 25
  status[immune_24] = 25
  status[immune_34] = 25
  
  # storing 'seroresponse'
  serotype1[selected_immune_1] = 1
  serotype2[selected_immune_2] = 1
  serotype3[selected_immune_3] = 1
  serotype4[selected_immune_4] = 1
  

  ## Seeding index cases for each serotype -- we'll pick 4 individuals to infect, 1 with each serotype
  # sample from the susceptible individuals
  strain1_patientzero = sample(which(status == 0 | status == 10 | status == 11 | status == 12), 1)
  status[strain1_patientzero] = ifelse(status[strain1_patientzero] == 0, 1,
                                       ifelse(status[strain1_patientzero] == 10, 16,
                                              ifelse(status[strain1_patientzero] == 11, 19,
                                                     22
                                              )))
  
  
  # sero 2 patient zero must be susceptible to sero 2
  strain2_patientzero = sample(which(status == 0 | status == 9 | status == 11 | status == 12), 1)
  status[strain2_patientzero] = ifelse(status[strain2_patientzero] == 0, 2,
                                       ifelse(status[strain2_patientzero] == 9, 13,
                                              ifelse(status[strain2_patientzero] == 11, 20,
                                                     23
                                              )))
  
  # sero 3 patient zero must be susceptible to sero 3
  strain3_patientzero = sample(which(status == 0 | status == 9 | status == 10 | status == 12), 1)
  status[strain3_patientzero] = ifelse(status[strain3_patientzero] == 0, 3,
                                       ifelse(status[strain3_patientzero] == 9, 14,
                                              ifelse(status[strain3_patientzero] == 10, 17,
                                                     24
                                              )))
  
  # sero 4 patient zero must be susceptible to sero 4
  strain4_patientzero = sample(which(status == 0 | status == 9 | status == 10 | status == 11), 1)
  status[strain4_patientzero] = ifelse(status[strain4_patientzero] == 0, 4,
                                       ifelse(status[strain4_patientzero] == 9, 15,
                                              ifelse(status[strain4_patientzero] == 10, 18,
                                                     21
                                              )))
  
  ### Initialising some states that we want to track
  S_t = numeric(time_steps+1) # Susceptible
  R_t = numeric(time_steps+1) # infected twice
  deaths_t = numeric(time_steps+1)
  
  sum_I1 = numeric(time_steps+1) # total serotype 1 infections
  sum_I2 = numeric(time_steps+1) # total serotype 2 infections
  sum_I3 = numeric(time_steps+1) # total serotype 3 infections
  sum_I4 = numeric(time_steps+1) # total serotype 4 infections
  
  new_I1 = numeric(time_steps+1)  # new serotpe 1 infections
  new_I2 = numeric(time_steps+1)  # new serotpe 2 infections
  new_I3 = numeric(time_steps+1)  # new serotpe 3 infections
  new_I4 = numeric(time_steps+1)  # new serotpe 4 infections

  tic("1 simulation run time")
  # starting iterations here
  out = tryCatch({
    for(tt in 1:(time_steps+1)) {
      
      ## Storing values here
      selected = sample(which(status != 26), deaths)
      status[selected] = 26
      deaths_t[tt] = deaths
      
      N = N + births
      
      ## adding new births
      if(births > 0) {
        # adding all the new births into our N_parameters' parameters
        status = c(status, rep(0, births))
        sero1transmission = c(sero1transmission, rep(pr_transmission[1], births))
        sero2transmission = c(sero2transmission, rep(pr_transmission[2], births))
        sero3transmission = c(sero3transmission, rep(pr_transmission[3], births))
        sero4transmission = c(sero4transmission, rep(pr_transmission[4], births))
      }
      # status = c(status, rep(0, births))
      S_t[tt] = sum(status == 0)
      
      total1status = status == 1 | status == 16 | status == 19 | status == 22
      total2status = status == 2 | status == 13 | status == 20 | status == 23
      total3status = status == 3 | status == 14 | status == 17 | status == 24
      total4status = status == 4 | status == 15 | status == 18 | status == 21
      
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
        status == 14 & recover3 == 1 ~ 25, # Recovery from 1 -> 3 (secondary)
        status == 15 & recover4 == 1 ~ 25, # Recovery from 1 -> 4 (secondary)
        status == 16 & recover1 == 1 ~ 25, # Recovery from 2 -> 1 (secondary)
        status == 17 & recover3 == 1 ~ 25, # Recovery from 2 -> 3 (secondary)
        status == 18 & recover4 == 1 ~ 25, # Recovery from 2 -> 4 (secondary)
        status == 19 & recover1 == 1 ~ 25, # Recovery from 3 -> 1 (secondary)
        status == 20 & recover2 == 1 ~ 25, # Recovery from 3 -> 2 (secondary)
        status == 21 & recover4 == 1 ~ 25, # Recovery from 3 -> 4 (secondary)
        status == 22 & recover1 == 1 ~ 25, # Recovery from 4 -> 1 (secondary)
        status == 23 & recover2 == 1 ~ 25, # Recovery from 4 -> 2 (secondary)
        status == 24 & recover3 == 1 ~ 25, # Recovery from 4 -> 3 (secondary)
        .default = status
      )
      
      ## (6) Resolving cross-protections
      status = case_when(
        status == 5 & no_protection == 1 ~ 9, # Recovery from sero 1 (primary)
        status == 6 & no_protection == 1 ~ 10, # Recovery from sero 2 (primary)
        status == 7 & no_protection == 1 ~ 11, # Recovery from sero 3 (primary)
        status == 8 & no_protection == 1 ~ 12, # Recovery from sero 4 (primary)
        .default = status
      )
      
      # We'll track the current number of infections at this point after all the recoveries
      # so we can calculate the number of new infections that happen in the next step
      curr_I1 = c()
      curr_I2 = c()
      curr_I3 = c()
      curr_I4 = c()
      
      curr_I1 = sum(status == 1 | status == 16 | status == 19 | status == 22)
      curr_I2 = sum(status == 2 | status == 13 | status == 20 | status == 23)
      curr_I3 = sum(status == 3 | status == 14 | status == 17 | status == 24)
      curr_I4 = sum(status == 4 | status == 15 | status == 18 | status == 21)
      
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
                                                 status[co_infect12])
                                   ))
      
      status[co_infect13] = ifelse(status[co_infect13] == 0,
                                   sample(c(1,3), N, replace = TRUE),
                                   ifelse(status[co_infect13] == 10,
                                          sample(c(16,17), N, replace = TRUE),
                                          ifelse(status[co_infect13] == 12,
                                                 sample(c(22,24), N, replace = TRUE),
                                                 status[co_infect13])
                                   ))
      
      status[co_infect14] = ifelse(status[co_infect14] == 0,
                                   sample(c(1,4), N, replace = TRUE),
                                   ifelse(status[co_infect14] == 10,
                                          sample(c(16,18), N, replace = TRUE),
                                          ifelse(status[co_infect14] == 11,
                                                 sample(c(19,21), N, replace = TRUE),
                                                 status[co_infect14])
                                   ))
      
      status[co_infect23] = ifelse(status[co_infect23] == 0,
                                   sample(c(2,3), N, replace = TRUE),
                                   ifelse(status[co_infect23] == 9,
                                          sample(c(13,14), N, replace = TRUE),
                                          ifelse(status[co_infect23] == 12,
                                                 sample(c(23,24), N, replace = TRUE),
                                                 status[co_infect23])
                                   ))
      
      status[co_infect24] = ifelse(status[co_infect24] == 0,
                                   sample(c(2,4), N, replace = TRUE),
                                   ifelse(status[co_infect24] == 9,
                                          sample(c(13,15), N, replace = TRUE),
                                          ifelse(status[co_infect24] == 11,
                                                 sample(c(20,21), N, replace = TRUE),
                                                 status[co_infect24])
                                   ))
      
      status[co_infect34] = ifelse(status[co_infect34] == 0,
                                   sample(c(3,4), N, replace = TRUE),
                                   ifelse(status[co_infect34] == 9,
                                          sample(c(14,15), N, replace = TRUE),
                                          ifelse(status[co_infect34] == 10,
                                                 sample(c(17,18), N, replace = TRUE),
                                                 status[co_infect34])
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
        .default = status
      )
      
      # Calculating how many new infections we got by subtracting from the earlier stored value
      new_I1[tt] = sum(status == 1 | status == 16 | status == 19 | status == 22) - curr_I1
      new_I2[tt] = sum(status == 2 | status == 13 | status == 20 | status == 23) - curr_I2
      new_I3[tt] = sum(status == 3 | status == 14 | status == 17 | status == 24) - curr_I3
      new_I4[tt] = sum(status == 4 | status == 15 | status == 18 | status == 21) - curr_I4
      
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
                   sum.I1 = sum_I1,
                   sum.I2 = sum_I2,
                   sum.I3 = sum_I3,
                   sum.I4 = sum_I4,
                   new.I1 = new_I1,
                   new.I2 = new_I2,
                   new.I3 = new_I3,
                   new.I4 = new_I4
  )
  
  # parameters$N_parameters = length(status)
  
  output = list(
    "res" = res,
    # "status" =  status,
    "times" = c(a),
    "serotypes" = list(serotype1, serotype2, serotype3, serotype4)
  )
  
  return(output)
}
