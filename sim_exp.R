
# code to run simulation experiment

# run from the command line with a single argument m to set the number of observations.

# can be used with m = 500, and m = 10000 to generate the existing results

###################################################################################################

### SETUP ###

# clear environment
rm(list=ls())

# clear console 
cat("\014")

# may need to set directory 
# wd <- ""
# setwd(wd)

###################################################################################################

# source simulation model
source("sim_model.R")

# source arguments
source("arguments.R")

# get number of observations as argument
m <- as.numeric(commandArgs(trailingOnly = TRUE)[1])

###################################################################################################

# run initial experiment

# set seed
set.seed(612)

# generate input data
data <- matrix(rexp(m * 4, rate = c(lambda, mu)), ncol = 4, nrow = m, byrow = TRUE)

# compute MLEs 
MLEs <- 1 / colMeans(data)

# run simulation
sim_MLEs <- simulate(MLEs[1], MLEs[2], MLEs[3], MLEs[4], MaintInterval, FixTime, tend)

###################################################################################################

# repeat simulation experiment with MLEs

# list to store results
sim_MLEs_rep <- vector("list", length = n)

# for each replication
for(i in 1:n){
  
  # run simulation
  sim_MLEs_rep[[i]] <- simulate(MLEs[1], MLEs[2], MLEs[3], MLEs[4], MaintInterval, FixTime, tend)
  
  # print progress
  print(round(i / n, 2))
}

###################################################################################################

# run repeated simulation experiment with true parameter values

# list to store results
sim_true_rep <- vector("list", length = n)

# for each replication
for(i in 1:n){
  
  # run simulation
  sim_true_rep[[i]] <- simulate(lambda, mu[1], mu[2], mu[3], MaintInterval, FixTime, tend)
  
  # print progress
  print(round(i / n, 2))
}

###################################################################################################

# experimental design

# create full factorial design

# compute standard deviations of MLEs
MLE_sds <- sqrt(MLEs ^ 2 / m)

# matrix to store design points
dps <- matrix(0, nrow = d, ncol = n_pars)

# set low and high factor levels
dps[, 1:n_pars] <- as.matrix(expand.grid("V1" = c(-MLE_sds[1], MLE_sds[1]),
                                         "V2" = c(-MLE_sds[2], MLE_sds[2]),
                                         "V3" = c(-MLE_sds[3], MLE_sds[3]),
                                         "V4" = c(-MLE_sds[4], MLE_sds[4])))

# add on initial parameter values
dps <- sweep(dps, 2, MLEs, `+`)

# run simulation at design points

# list to store results
sim_dps <- vector("list", d)

# loop over design points
for(j in 1:d){
  
  # create temporary list
  temp_sim <- vector("list", length = n)
  
  # for each replication
  for(i in 1:n){
    
    # run simulation
    temp_sim[[i]] <- simulate(dps[j, 1], dps[j, 2], dps[j, 3], dps[j, 4], MaintInterval, FixTime, tend)
    
    # print progress
    print(round(i / n, 2))
  }
  
  # store results
  sim_dps[[j]] <- temp_sim
  
  # print progress
  print(round(j / d, 2))
  
}

###################################################################################################

# save results
file_name <- paste0("results_m", m, ".RData")
save.image(file_name)
