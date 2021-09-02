options(java.parameters = "-Xmx10g") # Allocate 10GB of memory for Java
library(bartMachine)
library(glmnet)
source("iBART.R")
load("Connor_data.RData")
seeds <- c(12,   2583,  893,  535, 1174, 2552, 1386, 2841,  416, 2749,
           656,  1741,  902,  509, 3376, 5623, 5172, 5993, 3618, 3839,
           3421, 3478, 3427, 4639, 4961, 5634, 5158, 5402, 5470, 5395, 
           3754, 5906, 4520, 5970, 4553, 5596, 4647, 3906, 5800, 8790,
           8051, 8862, 7166, 7867, 8358, 6389, 7827, 7724, 7625, 7389)
count <- 1
S <- length(seeds)
time <- rep(0, S)
RMSE_out_sample <- matrix(0, nrow = S, ncol = 5)
colnames(RMSE_out_sample) <- c("1D out-of-sample", "2D out-of-sample", 
                               "3D out-of-sample", "4D out-of-sample", 
                               "5D out-of-sample")
for(seed in seeds){
  iBART_results <- iBART(X = X, y = y, 
                         head = head, 
                         dimen = dimen, 
                         opt = 2, # binary operator first
                         iter = 3, 
                         out_sample = TRUE, 
                         train_ratio = 0.91, 
                         Lzero = TRUE, 
                         K = 5, 
                         writeLog = FALSE, 
                         count = count, 
                         seed = seed)
  
  RMSE_out_sample[count,] <- iBART_results$Lzero_out_sample_RMSE
  time[count] <- iBART_results$runtime
  cat(paste("Finished iteration: ", count, "/50", sep = ""))
  count <- count + 1
  # save.image(file = "Connor_iBART_out_sample.RData")
}
colMeans(RMSE_out_sample)
boxplot(RMSE_out_sample) # Out-of-sample RMSE plot for k = 1,2,3,4,5