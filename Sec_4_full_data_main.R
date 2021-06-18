options(java.parameters = "-Xmx10g") # Allocate 10GB of memory for Java
library(bartMachine)
library(glmnet)
source("iBART.R")
load("Connor_data.RData")
RMSE_out_sample <- rep(0, 5)
names(RMSE_out_sample) <- c("1D out-of-sample", "2D out-of-sample", 
                            "3D out-of-sample", "4D out-of-sample",
                            "5D out-of-sample")

iBART_results <- iBART(X = X, y = y, 
                       head = head, 
                       dimen = dimen, 
                       opt = 2, # binary operator first
                       iter = 3,
                       out_sample = FALSE, 
                       train_ratio = 1, 
                       K = 5,
                       writeLog = TRUE, 
                       count = NULL, 
                       seed = 888)

RMSE_out_sample[1:5] <- iBART_results$Lzero_out_sample_RMSE
time <- iBART_results$runtime
summary(iBART_results$Lzero_model[[3]]) # summary of the 3-descriptor model


