# PARAMETER MATRICES
param0 <- as.data.frame(matrix(NA, nrow=16, ncol=4))
param0[1:8,1] <- 50; 
param0[9:16,1] <- 60;
param0[1:4,2] <- 10; 
param0[9:12,2] <- 10; 
param0[5:8,2] <- 5; 
param0[13:16,2] <- 5;
param0[,3] <- c(25,25,30,30,25,25,30,30,25,25,30,30,25,25,30,30);
param0[seq_len(16) %% 2 == 1,4] <- 3; 
param0[seq_len(16) %% 2 == 0,4] <- 6

param_age1 <- as.data.frame(matrix(NA, nrow=25, ncol=6))
param_age1[1:5,1:3] <- 50; 
param_age1[6:10,1] <- 47; 
param_age1[6:10,2] <- 50; 
param_age1[6:10,3] <- 53;
param_age1[11:15,1] <- 40; 
param_age1[11:15,2] <- 50;
param_age1[11:15,3] <- 60;
param_age1[16:20,1] <- 57; 
param_age1[16:20,2] <- 60;
param_age1[16:20,3] <- 63;
param_age1[21:25,1] <- 50; 
param_age1[21:25,2] <- 60;
param_age1[21:25,3] <- 70;
param_age1[1:25,4:6] <- 10;

param_bmi1 <- as.data.frame(matrix(NA, nrow=25, ncol=6))
x <- rbind(c(25,25,25),c(23,25,27),c(20,25,30),
           c(28,30,32),c(25,30,35))
param_bmi1[1:5,1:3] <- x; 
param_bmi1[6:10,1:3] <- x; 
param_bmi1[11:15,1:3] <- x; 
param_bmi1[16:20,1:3] <- x; 
param_bmi1[21:25,1:3] <- x;
param_bmi1[1:25,4:6] <- 3;


param_age2 <- as.data.frame(matrix(NA, nrow=25, ncol=6))
param_age2[1:5,1:3] <- 50; 
param_age2[6:10,1] <- 47; 
param_age2[6:10,2] <- 50; 
param_age2[6:10,3] <- 53;
param_age2[11:15,1] <- 40; 
param_age2[11:15,2] <- 50; 
param_age2[11:15,3] <- 60;
param_age2[16:20,1] <- 57; 
param_age2[16:20,2] <- 60; 
param_age2[16:20,3] <- 63;
param_age2[21:25,1] <- 50; 
param_age2[21:25,2] <- 60; 
param_age2[21:25,3] <- 70;
param_age2[1:25,4:6] <- 5;

param_bmi2 <- as.data.frame(matrix(NA, nrow=25, ncol=6))
x <- rbind(c(25,25,25),c(23,25,27),
           c(20,25,30),c(28,30,32),c(25,30,35))
param_bmi2[1:5,1:3] <- x; 
param_bmi2[6:10,1:3] <- x; 
param_bmi2[11:15,1:3] <- x; 
param_bmi2[16:20,1:3] <- x; 
param_bmi2[21:25,1:3] <- x;
param_bmi2[1:25,4:6] <- 1;

param_age3 <- as.data.frame(matrix(NA, nrow=25, ncol=6))
param_age3[1:5,1:3] <- 50; 
param_age3[6:10,1] <- 47; 
param_age3[6:10,2] <- 50;
param_age3[6:10,3] <- 53;
param_age3[11:15,1] <- 40; 
param_age3[11:15,2] <- 50; 
param_age3[11:15,3] <- 60;
param_age3[16:20,1] <- 57; 
param_age3[16:20,2] <- 60; 
param_age3[16:20,3] <- 63;
param_age3[21:25,1] <- 50; 
param_age3[21:25,2] <- 60; 
param_age3[21:25,3] <- 70;
param_age3[1:25,4] <- 5; 
param_age3[1:25,5] <- 8; 
param_age3[1:25,6] <- 3;

param_bmi3 <- as.data.frame(matrix(NA, nrow=25, ncol=6))
x <- rbind(c(25,25,25),c(23,25,27),c(20,25,30),
           c(28,30,32),c(25,30,35))
param_bmi3[1:5,1:3] <- x; 
param_bmi3[6:10,1:3] <- x; 
param_bmi3[11:15,1:3] <- x; 
param_bmi3[16:20,1:3] <- x; 
param_bmi3[21:25,1:3] <- x;
param_bmi3[1:25,4:6] <- 1; 
param_bmi3[1:25,5] <- 5; 
param_bmi3[1:25,6] <- 7;

setwd(path)
saveRDS(param0, file="param0")
saveRDS(param_age1, file="param_age1")
saveRDS(param_age2, file="param_age2")
saveRDS(param_age3, file="param_age3")
saveRDS(param_bmi1, file="param_bmi1")
saveRDS(param_bmi2, file="param_bmi2")
saveRDS(param_bmi3, file="param_bmi3")