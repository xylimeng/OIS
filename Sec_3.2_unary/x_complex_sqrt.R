################# Main #################
set.seed(113)
options(java.parameters = "-Xmx10g")
library(bartMachine)
source("operations.R")
S <- 50
n <- 200
p <- 5

sqrt_TP <- rep(0, S)
sqrt_FP <- rep(0, S)
sqrt_TN <- rep(0, S)
sqrt_FN <- rep(0, S)
for (s in 1:S) {
  X <- matrix(rlnorm(n * p, meanlog = 2, sdlog = 0.5), nrow = n, ncol = p)
  colnames(X) <- paste("x.", seq(from = 1, to = p, by = 1), sep = "")

  eps <- rnorm(n)
  y <- 10 * sqrt(X[, 1]) + eps
  X <- as.data.frame(X)
  X_opt <- unary(X)

  bart_machine <- bartMachine(X_opt, y, num_trees = 20,
                              num_burn_in = 10000,
                              num_iterations_after_burn_in = 5000,
                              run_in_sample = FALSE,
                              serialize = FALSE,
                              verbose = FALSE,
                              seed = 99)
  var_sel <- var_selection_by_permute(bart_machine,
                                      num_permute_samples = 50,
                                      alpha = 0.05,
                                      plot = FALSE)
  pos_idx <- var_sel$important_vars_global_se_col_nums
  true_idx <- which(colnames(X_opt) == "x.1^0.5")
  sqrt_TP[s] <- sum(pos_idx == true_idx)
  sqrt_FP[s] <- sum(pos_idx != true_idx)
  sqrt_FN[s] <- 1 - sqrt_TP[s]
  sqrt_TN[s] <- ncol(X_opt) - sqrt_FP[s] - 1
}
mean(sqrt_TP) # 1
mean(sqrt_TN) # 33.92
mean(sqrt_FN) # 0
mean(sqrt_FP) # 5.08
