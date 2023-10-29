set.seed(113)
add_TP <- rep(0, S)
add_FP <- rep(0, S)

for (s in 1:S) {
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste("x.", seq(from = 1, to = p, by = 1), sep = "")
  eps <- rnorm(n)
  y <- 10 * (X[, 1] + X[, 2]) + eps
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
  pos_idx <- var_sel$important_vars_global_se_col_nums
  true_idx <- which(colnames(X_opt) == "(x.1+x.2)")
  add_TP[s] <- sum(pos_idx == true_idx)
  add_FP[s] <- sum(pos_idx != true_idx)
  cat("Operator: add, ", "Iteration: ", s, "/100... \n", sep = "")
}
