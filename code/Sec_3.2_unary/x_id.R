set.seed(113)
id_TP <- rep(0, S)
id_FP <- rep(0, S)

for (s in 1:S) {
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste("x.", seq(from = 1, to = p, by = 1), sep = "")
  
  eps <- rnorm(n)
  y <- 10 * X[, 1] + eps
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
  if (anyNA(pos_idx)) {
    pos_idx <- which.max(var_sel$var_true_props_avg)
    if (names(pos_idx) == "x.1") {
      id_TP[s] <- 1
      id_FP[s] <- 0
    } else {
      id_TP[s] <- 0
      id_FP[s] <- 1
    }
  } else{
    true_idx <- which(colnames(X_opt) == "x.1")
    id_TP[s] <- sum(pos_idx == true_idx)
    id_FP[s] <- sum(pos_idx != true_idx)
  }
  cat("Operator: id, ", "Iteration: ", s, "/100... \n", sep = "")
}
