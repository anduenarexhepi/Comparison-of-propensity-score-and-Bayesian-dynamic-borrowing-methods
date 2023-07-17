noborrowing <- function(n_sim=1000, sim_data){
  
  ATE_est <- rep(NA, n_sim)
  
  CI <- matrix(NA, nrow = n_sim, ncol = 2)
  colnames(CI) <- c("Lower95","Upper95")
  
  for (i in 1:n_sim){
    
    dat <- as.data.frame(sim_data[[i]])
    
    df <-  dat[dat$Z==1, ] # only current trial data needed
    
    ATE_est[i] <- with(data = df[df$treatment == 1, ], mean(Y)) - 
      with(data = df[df$treatment == 0, ], mean(Y))
    #mean of outcome  of current treated patients -
    #mean of outcome of current control patients
    
    m <- nrow( df[df$treatment == 1, ] ) # number of current treated patients
    n <- nrow( df[df$treatment == 0, ] ) # number of current control patients
    sT_var <- with(data = df[df$treatment == 1, ], var(Y)) # sample variance of Y of current treated patients
    sC_var <- with(data = df[df$treatment == 0, ], var(Y)) # sample variance of Y of current control patients
    S <- sqrt( ( (m-1)*sT_var + (n-1)*sC_var )/( m+n-2 ) )
    aux <- sqrt((1/m)+(1/n))
    
    t <- qt(0.975, m+n-2)
    
    CI[i, 1]<-ATE_est[i]-t*S*aux
    CI[i, 2]<-ATE_est[i]+t*S*aux
    
  }
  
  ATE = mean(ATE_est)
  e.var = (n_sim/(n_sim-1))*mean((ATE_est-ATE)^2)
  bias = ATE - (-2)
  mse = mean((ATE_est-(-2))^2)
  coverage = mean( CI[,1] <= (-2) & CI[,2] >= (-2) )
  res <- c(ATE, bias, e.var, mse, coverage)
  return(res)
  
}

data_simulation_onlycurrent <- function(n_sim=1000){
  
  n_current_treated <- 200
  n_current_control <- 100
  
  age_m_current  <- 50
  age_sd_current <- 10
  bmi_m_current  <- 25
  bmi_sd_current <- 3
  
  
  epsilon_sd <- 20
  
  trt   <- c(rep(1, n_current_treated),
             rep(0, n_current_control))
  Z     <- c(rep(1, n_current_treated + n_current_control))
  study <- c(rep(-1, n_current_treated + n_current_control))
  
  sim_data <- vector(mode = "list", length = n_sim)
  
  for (i in 1:n_sim){
    
    ## current
    
    age_current_treated <- rnorm(n_current_treated,
                                 mean = age_m_current,
                                 sd = age_sd_current)
    age_current_control <- rnorm(n_current_control,
                                 mean = age_m_current,
                                 sd = age_sd_current)
    
    bmi_current_treated <- rnorm(n_current_treated,
                                 mean = bmi_m_current,
                                 sd = bmi_sd_current)
    bmi_current_control <- rnorm(n_current_control,
                                 mean = bmi_m_current,
                                 sd = bmi_sd_current)
    
    ## Outcome - Blood pressure via linear model
    
    age <- c(age_current_treated, age_current_control)
    bmi <- c(bmi_current_treated, bmi_current_control)
    
    epsilon <- rnorm(n_current_treated + n_current_control,
                     mean = 0, sd = epsilon_sd)
    
    ## index 1:200: current trt, 201:300 current control
    outcomes <-  150 + 0.1 * (age-50) + 0.3 *  (bmi-25) +
      -2 * trt + epsilon
    
    sim_data[[i]] <- cbind(age, bmi, Z, treatment = trt,
                           Study = study, Y = outcomes)
    
  }
  
  return(sim_data)
  
}

####################################################################

# Number of replications
repl <- 1000

# Column names of result table
names <- c("ATE","Bias","E.Var.","MSE","Coverage")

result_3               <- matrix(data=NA, nrow=1,ncol=5)
colnames(result_3)     <- names
set.seed(84909)
sim_data <- data_simulation_onlycurrent(n_sim=repl)
result_3 <- noborrowing(n_sim=repl, sim_data=sim_data)
round(result_3, digits=2)
# -1.88  0.12  6.09  6.10  0.95









