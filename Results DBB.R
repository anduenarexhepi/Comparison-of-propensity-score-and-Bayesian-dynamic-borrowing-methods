
################################################
################# DBB ##########################
################################################

# LOAD LIBRARIES
library(Manduena)
library(RBesT)
library(matrixStats)
library(doFuture)
library(foreach)
library(doRNG)
library(future.batchtools)

setwd(path)

################################################
# PARALLELIZATION
## register parallel backend
registerDoFuture()
## Parallelization on HPC
job_time  <- 24  # run time for a job in hours
n_worker  <- 24  # number of worker nodes          (maximum 24)
n_cores   <- 16  # number of cores per worker node (maximum 16)
gb_memory <-  2  # RAM per CPU in gigabytes        (maximum 40)

slurm <- tweak(batchtools_slurm,
               template  = system.file('templates/slurm-simple.tmpl',
                                       package = 'batchtools'),
               workers   = n_worker,
               resources = list(
                 walltime  = 60 * 60 * job_time,
                 ncpus     = n_cores,
                 memory    = 1000 * gb_memory))

plan(list(slurm, multisession))
###############################################

# Column names of result matrix
names <- c("ATE", "Bias", "E.Var.", "MSE", "Coverage",
           "PriorESS", "Avg. ip","Adj.PosteriorESS")

# NUMBER OF REPLICATIONS PER SCENARIO
repl <- 1000

# Historical trial sizes
hist_size <- c(300,300,300)

################################################
#################### OHT #######################
################################################

# PARAMETER MATRICES FOR DIFFERENT SCENARIOS
param0     <- readRDS("param0")

# RESULT MATRIX
result_dbb              <- matrix(data=NA, nrow = 16, ncol = 8)
colnames(result_dbb)    <- names

for(i in 1:16){
  set.seed(84909)
  sim_data            <- 
    Manduena::data_simulation_oht(n_sim=repl, 
                                  age_m_historical  = as.numeric(param0[i,1]), 
                                  age_sd_historical = as.numeric(param0[i,2]),
                                  bmi_m_historical  = as.numeric(param0[i,3]), 
                                  bmi_sd_historical = as.numeric(param0[i,4]))
  result_dbb[i,]      <- 
    Manduena::bayesianBorrowing(n_sim=repl, sim_data=sim_data)
}

result_dbb <- as.data.frame(result_dbb) 
round(result_dbb, digits = 2) 

################################################
################### MHT 1 ######################
################################################

# PARAMETER MATRICES FOR DIFFERENT SCENARIOS
param_age1 <- readRDS("param_age1")
param_bmi1 <- readRDS("param_bmi1")

# RESULT MATRIX
result_dbb_mht_i        <- matrix(data=NA, nrow = 25, ncol = 8)
colnames(result_dbb_mht_i) <- names

for(i in 1:25){
  set.seed(84909)
  sim_data                <- 
    Manduena::data_simulation_mht(n_sim=repl, 
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age1[i,1:3]), 
                                  age_sd_historical = as.numeric(param_age1[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi1[i,1:3]), 
                                  bmi_sd_historical = as.numeric(param_bmi1[i,4:6]))
  result_dbb_mht_i[i,]    <- 
    Manduena::bayesianBorrowing(n_sim=repl, sim_data=sim_data)
}

result_dbb_mht_i = as.data.frame(result_dbb_mht_i)
round(result_dbb_mht_i, digits=2)

################################################
################### MHT 2 ######################
################################################

# PARAMETER MATRICES FOR DIFFERENT SCENARIOS
param_age2 <- readRDS("param_age2")
param_bmi2 <- readRDS("param_bmi2")

# RESULT MATRIX
result_dbb_mht_ii       <- matrix(data=NA, nrow = 25, ncol = 8)
colnames(result_dbb_mht_ii) <- names

for(i in 1:25){
  set.seed(84909)
  sim_data                <- 
    Manduena::data_simulation_mht(n_sim=repl, 
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age2[i,1:3]), 
                                  age_sd_historical = as.numeric(param_age2[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi2[i,1:3]), 
                                  bmi_sd_historical = as.numeric(param_bmi2[i,4:6]))
  result_dbb_mht_ii[i,]    <- 
    Manduena::bayesianBorrowing(n_sim=repl, sim_data=sim_data)
}

result_dbb_mht_ii = as.data.frame(result_dbb_mht_ii)
round(result_dbb_mht_ii, digits=2)

################################################
################### MHT 3 ######################
################################################

# PARAMETER MATRICES FOR DIFFERENT SCENARIOS
param_age3 <- readRDS("param_age3")
param_bmi3 <- readRDS("param_bmi3")

# RESULT MATRICES
result_dbb_mht_iii      <- matrix(data=NA, nrow = 25, ncol = 8)
colnames(result_dbb_mht_iii) <- names

for(i in 1:25){
  set.seed(84909)
  sim_data                <- 
    Manduena::data_simulation_mht(n_sim=repl, 
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age3[i,1:3]), 
                                  age_sd_historical = as.numeric(param_age3[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi3[i,1:3]), 
                                  bmi_sd_historical = as.numeric(param_bmi3[i,4:6]))
  result_dbb_mht_iii[i,]    <- 
    Manduena::bayesianBorrowing(n_sim=repl, sim_data=sim_data)
}

result_dbb_mht_iii = as.data.frame(result_dbb_mht_iii)
round(result_dbb_mht_iii, digits=2)

