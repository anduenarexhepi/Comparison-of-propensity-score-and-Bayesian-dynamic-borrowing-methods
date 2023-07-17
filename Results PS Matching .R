


#############################
# Propensity Score Matching #
#############################
library(Manduena)
library(MatchIt)

setwd(path)

# Number of replications
repl <- 1000

# Historical trial sizes
hist_size <- c(300,300,300)

# Column names of result table
names <- c("ATE","Bias","E.Var.","MSE","Coverage","Matched External")

####################################################################
# OHT
####################################################################
# Parameter matrix
param0     <- readRDS("param0")

#Matrix to store results
result_PS_Match           <- matrix(data=NA, nrow=16,ncol=6)
colnames(result_PS_Match) <- names
####################################################################
for(i in 1:4){
  set.seed(84909)
  sim_data             <- Manduena::data_simulation_oht(n_sim=repl,
                                                        age_m_historical  = as.numeric(param0[i,1]),
                                                        age_sd_historical = as.numeric(param0[i,2]),
                                                        bmi_m_historical  = as.numeric(param0[i,3]),
                                                        bmi_sd_historical = as.numeric(param0[i,4]))
  result_PS_Match[i,]  <- Manduena::PS_Match(n_sim=repl,
                                             sim_data=sim_data)
}
saveRDS(result_PS_Match[1:4,], file="OHT_PS_Match_1-4")

for(i in 5:8){
  set.seed(84909)
  sim_data             <- Manduena::data_simulation_oht(n_sim=repl,
                                                        age_m_historical  = as.numeric(param0[i,1]),
                                                        age_sd_historical = as.numeric(param0[i,2]),
                                                        bmi_m_historical  = as.numeric(param0[i,3]),
                                                        bmi_sd_historical = as.numeric(param0[i,4]))
  result_PS_Match[i,]  <- Manduena::PS_Match(n_sim=repl,
                                             sim_data=sim_data)
}
saveRDS(result_PS_Match[5:8,], file="OHT_PS_Match_5-8")

for(i in 9:12){
  set.seed(84909)
  sim_data             <- Manduena::data_simulation_oht(n_sim=repl,
                                                        age_m_historical  = as.numeric(param0[i,1]),
                                                        age_sd_historical = as.numeric(param0[i,2]),
                                                        bmi_m_historical  = as.numeric(param0[i,3]),
                                                        bmi_sd_historical = as.numeric(param0[i,4]))
  result_PS_Match[i,]  <- Manduena::PS_Match(n_sim=repl,
                                             sim_data=sim_data)
}
saveRDS(result_PS_Match[9:12,], file="OHT_PS_Match_9-12")

for(i in 13:16){
  set.seed(84909)
  sim_data             <- Manduena::data_simulation_oht(n_sim=repl,
                                                        age_m_historical  = as.numeric(param0[i,1]),
                                                        age_sd_historical = as.numeric(param0[i,2]),
                                                        bmi_m_historical  = as.numeric(param0[i,3]),
                                                        bmi_sd_historical = as.numeric(param0[i,4]))
  result_PS_Match[i,]  <- Manduena::PS_Match(n_sim=repl,
                                             sim_data=sim_data)
}
saveRDS(result_PS_Match[13:16,], file="OHT_PS_Match_13-16")
##################################################################
# MHT 1
##################################################################
# Parameter matrices
param_age1 <- readRDS("param_age1")
param_bmi1 <- readRDS("param_bmi1")

#Matrix to store results
result_PS_Match_mht_i           <- matrix(data=NA, nrow=25,ncol=6)
colnames(result_PS_Match_mht_i) <- names
##################################################################
for(i in 1:5){
  set.seed(84909)
  sim_data <-
    Manduena::data_simulation_mht(n_sim=repl,
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age1[i,1:3]),
                                  age_sd_historical = as.numeric(param_age1[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi1[i,1:3]),
                                  bmi_sd_historical = as.numeric(param_bmi1[i,4:6]))
  result_PS_Match_mht_i[i,]  <- Manduena::PS_Match(n_sim=repl,
                                                   sim_data=sim_data)
}
saveRDS(result_PS_Match_mht_i[1:5,], file="MHT1_PS_Match_1-5")

for(i in 6:10){
  set.seed(84909)
  sim_data <-
    Manduena::data_simulation_mht(n_sim=repl,
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age1[i,1:3]),
                                  age_sd_historical = as.numeric(param_age1[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi1[i,1:3]),
                                  bmi_sd_historical = as.numeric(param_bmi1[i,4:6]))
  result_PS_Match_mht_i[i,]  <- Manduena::PS_Match(n_sim=repl,
                                                   sim_data=sim_data)
}
saveRDS(result_PS_Match_mht_i[6:10,], file="MHT1_PS_Match_6-10")

for(i in 11:15){
  set.seed(84909)
  sim_data <-
    Manduena::data_simulation_mht(n_sim=repl,
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age1[i,1:3]),
                                  age_sd_historical = as.numeric(param_age1[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi1[i,1:3]),
                                  bmi_sd_historical = as.numeric(param_bmi1[i,4:6]))
  result_PS_Match_mht_i[i,]  <- Manduena::PS_Match(n_sim=repl,
                                                   sim_data=sim_data)
}
saveRDS(result_PS_Match_mht_i[11:15,], file="MHT1_PS_Match_11-15")

for(i in 16:20){
  set.seed(84909)
  sim_data <-
    Manduena::data_simulation_mht(n_sim=repl,
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age1[i,1:3]),
                                  age_sd_historical = as.numeric(param_age1[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi1[i,1:3]),
                                  bmi_sd_historical = as.numeric(param_bmi1[i,4:6]))
  result_PS_Match_mht_i[i,]  <- Manduena::PS_Match(n_sim=repl,
                                                   sim_data=sim_data)
}
saveRDS(result_PS_Match_mht_i[16:20,], file="MHT1_PS_Match_16-20")

for(i in 21:25){
  set.seed(84909)
  sim_data <-
    Manduena::data_simulation_mht(n_sim=repl,
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age1[i,1:3]),
                                  age_sd_historical = as.numeric(param_age1[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi1[i,1:3]),
                                  bmi_sd_historical = as.numeric(param_bmi1[i,4:6]))
  result_PS_Match_mht_i[i,]  <- Manduena::PS_Match(n_sim=repl,
                                                   sim_data=sim_data)
}
saveRDS(result_PS_Match_mht_i[21:25,], file="MHT1_PS_Match_21-25")
###################################################################
# MHT 2
###################################################################
# Parameter matrices
param_age2 <- readRDS("param_age2")
param_bmi2 <- readRDS("param_bmi2")

#Matrix to store results
result_PS_Match_mht_ii            <- matrix(data=NA, nrow=25,ncol=6)
colnames(result_PS_Match_mht_ii)  <- names
###################################################################
for(i in 1:5){
  set.seed(84909)
  sim_data <-
    Manduena::data_simulation_mht(n_sim=repl,
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age2[i,1:3]),
                                  age_sd_historical = as.numeric(param_age2[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi2[i,1:3]),
                                  bmi_sd_historical = as.numeric(param_bmi2[i,4:6]))
  result_PS_Match_mht_ii[i,] <- Manduena::PS_Match(n_sim=repl,
                                                   sim_data=sim_data)
}
saveRDS(result_PS_Match_mht_ii[1:5,], file="MHT2_PS_Match_1-5")

for(i in 6:10){
  set.seed(84909)
  sim_data <-
    Manduena::data_simulation_mht(n_sim=repl,
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age2[i,1:3]),
                                  age_sd_historical = as.numeric(param_age2[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi2[i,1:3]),
                                  bmi_sd_historical = as.numeric(param_bmi2[i,4:6]))
  result_PS_Match_mht_ii[i,] <- Manduena::PS_Match(n_sim=repl,
                                                   sim_data=sim_data)
}
saveRDS(result_PS_Match_mht_ii[6:10,], file="MHT2_PS_Match_6-10")

for(i in 11:15){
  set.seed(84909)
  sim_data <-
    Manduena::data_simulation_mht(n_sim=repl,
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age2[i,1:3]),
                                  age_sd_historical = as.numeric(param_age2[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi2[i,1:3]),
                                  bmi_sd_historical = as.numeric(param_bmi2[i,4:6]))
  result_PS_Match_mht_ii[i,] <- Manduena::PS_Match(n_sim=repl,
                                                   sim_data=sim_data)
}
saveRDS(result_PS_Match_mht_ii[11:15,], file="MHT2_PS_Match_11-15")

for(i in 16:20){
  set.seed(84909)
  sim_data <-
    Manduena::data_simulation_mht(n_sim=repl,
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age2[i,1:3]),
                                  age_sd_historical = as.numeric(param_age2[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi2[i,1:3]),
                                  bmi_sd_historical = as.numeric(param_bmi2[i,4:6]))
  result_PS_Match_mht_ii[i,] <- Manduena::PS_Match(n_sim=repl,
                                                   sim_data=sim_data)
}
saveRDS(result_PS_Match_mht_ii[16:20,], file="MHT2_PS_Match_16-20")

for(i in 21:25){
  set.seed(84909)
  sim_data <-
    Manduena::data_simulation_mht(n_sim=repl,
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age2[i,1:3]),
                                  age_sd_historical = as.numeric(param_age2[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi2[i,1:3]),
                                  bmi_sd_historical = as.numeric(param_bmi2[i,4:6]))
  result_PS_Match_mht_ii[i,] <- Manduena::PS_Match(n_sim=repl,
                                                   sim_data=sim_data)
}
saveRDS(result_PS_Match_mht_ii[21:25,], file="MHT2_PS_Match_21-25")
###################################################################
# MHT 3
###################################################################
# Parameter matrices
param_age3 <- readRDS("param_age3")
param_bmi3 <- readRDS("param_bmi3")

#Matrix to store results
result_PS_Match_mht_iii           <- matrix(data=NA, nrow=25,ncol=6)
colnames(result_PS_Match_mht_iii) <- names
###################################################################
for(i in 1:5){
  set.seed(84909)
  sim_data <-
    Manduena::data_simulation_mht(n_sim=repl,
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age3[i,1:3]),
                                  age_sd_historical = as.numeric(param_age3[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi3[i,1:3]),
                                  bmi_sd_historical = as.numeric(param_bmi3[i,4:6]))
  result_PS_Match_mht_iii[i,] <- Manduena::PS_Match(n_sim=repl,
                                                    sim_data=sim_data)
}
saveRDS(result_PS_Match_mht_iii[1:5,], file="MHT3_PS_Match_1-5")

for(i in 6:10){
  set.seed(84909)
  sim_data <-
    Manduena::data_simulation_mht(n_sim=repl,
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age3[i,1:3]),
                                  age_sd_historical = as.numeric(param_age3[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi3[i,1:3]),
                                  bmi_sd_historical = as.numeric(param_bmi3[i,4:6]))
  result_PS_Match_mht_iii[i,] <- Manduena::PS_Match(n_sim=repl,
                                                    sim_data=sim_data)
}
saveRDS(result_PS_Match_mht_iii[6:10,], file="MHT3_PS_Match_6-10")

for(i in 11:15){
  set.seed(84909)
  sim_data <-
    Manduena::data_simulation_mht(n_sim=repl,
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age3[i,1:3]),
                                  age_sd_historical = as.numeric(param_age3[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi3[i,1:3]),
                                  bmi_sd_historical = as.numeric(param_bmi3[i,4:6]))
  result_PS_Match_mht_iii[i,] <- Manduena::PS_Match(n_sim=repl,
                                                    sim_data=sim_data)
}
saveRDS(result_PS_Match_mht_iii[11:15,], file="MHT3_PS_Match_11-15")

for(i in 16:20){
  set.seed(84909)
  sim_data <-
    Manduena::data_simulation_mht(n_sim=repl,
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age3[i,1:3]),
                                  age_sd_historical = as.numeric(param_age3[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi3[i,1:3]),
                                  bmi_sd_historical = as.numeric(param_bmi3[i,4:6]))
  result_PS_Match_mht_iii[i,] <- Manduena::PS_Match(n_sim=repl,
                                                    sim_data=sim_data)
}
saveRDS(result_PS_Match_mht_iii[16:20,], file="MHT3_PS_Match_16-20")

for(i in 21:25){
  set.seed(84909)
  sim_data <-
    Manduena::data_simulation_mht(n_sim=repl,
                                  n_historical_control = hist_size ,
                                  age_m_historical  = as.numeric(param_age3[i,1:3]),
                                  age_sd_historical = as.numeric(param_age3[i,4:6]),
                                  bmi_m_historical  = as.numeric(param_bmi3[i,1:3]),
                                  bmi_sd_historical = as.numeric(param_bmi3[i,4:6]))
  result_PS_Match_mht_iii[i,] <- Manduena::PS_Match(n_sim=repl,
                                                    sim_data=sim_data)
}
saveRDS(result_PS_Match_mht_iii[21:25,], file="MHT3_PS_Match_21-25")

