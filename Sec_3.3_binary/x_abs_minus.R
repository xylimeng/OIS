################# Main #################
set.seed(113)
options(java.parameters = "-Xmx10g")
library(bartMachine)
source("operations.R")
S <- 50
n <- 200
p <- 5

abs_minus_TP <- rep(0, S)
abs_minus_FP <- rep(0, S)
abs_minus_TN <- rep(0, S)
abs_minus_FN <- rep(0, S)
for (s in 1:S) {
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste("x.", seq(from = 1, to = p, by = 1), sep = "")
  
  eps <- rnorm(n)
  y <- 10 * abs(X[, 1] - X[, 2]) + eps
  X_opt <- binary(X)
  
  bart_machine <- bartMachine(X_opt, y,
                              num_trees = 20,
                              num_burn_in = 10000,
                              num_iterations_after_burn_in = 5000,
                              run_in_sample = FALSE,
                              serialize = FALSE,
                              verbose = FALSE)
  var_sel <- var_selection_by_permute(bart_machine,
                                      num_permute_samples = 50,
                                      alpha = 0.05,
                                      plot = FALSE)
  pos_idx <- var_sel$important_vars_global_max_col_nums
  true_idx <- which(colnames(X_opt) == "|x.1-x.2|")
  abs_minus_TP[s] <- sum(pos_idx == true_idx)
  abs_minus_FP[s] <- sum(pos_idx != true_idx)
  abs_minus_FN[s] <- ifelse(any(pos_idx == true_idx), 0, 1)
  abs_minus_TN[s] <- ncol(X_opt) - abs_minus_FP[s] - 1
}
mean(abs_minus_TP)
mean(abs_minus_TN)
mean(abs_minus_FN)
mean(abs_minus_FP)
