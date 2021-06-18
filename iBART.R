source("BART_iter.R", local = TRUE)
source("LASSO.R", local = TRUE)
source("Lzero_regression.R", local = TRUE)
source("binary_operations.R", local = TRUE)
source("unary_operations.R", local = TRUE)
iBART <- function(X = NULL, y = NULL,
                  head = NULL,
                  dimen = NULL,
                  num_trees = 20,
                  num_burn_in = 10000,
                  num_iterations_after_burn_in = 5000,
                  num_reps_for_avg = 10,
                  num_permute_samples = 50,
                  opt = 2,
                  sin_cos = FALSE,
                  apply_pos_opt_on_neg_x = TRUE,
                  iter = 3L,
                  out_sample = FALSE,
                  train_ratio = 1L,
                  Lzero = TRUE,
                  K = ifelse(Lzero, 5L, 0L),
                  AIC = FALSE,
                  standardize = TRUE,
                  writeLog = FALSE,
                  count = NULL,
                  seed = NULL) {
  start_time <- Sys.time()
  
  if ((is.null(X)) || is.null(y)){
    stop("You need to give iBART a training set by specifying X and y \n")
  }
  
  if (!is.matrix(X)){
    stop("The training data X must be a matrix", call. = FALSE)
  }
  
  if (ncol(X) == 0){
    stop("Your data matrix must have at least one predictor.")
  }
  
  if (nrow(X) == 0){
    stop("Your data matrix must have at least one observation.")
  }
  
  if (length(y) != nrow(X)){
    stop("The number of responses must be equal to the number of observations in the training data X.")
  }
  
  if (length(num_trees) == 1) {
    num_trees <- rep(num_trees, iter)
  } else if ((length(num_trees) > 1) && (length(num_trees) != iter)) {
    stop("Length of number of trees must equal to number of iterations!")
  }
  
  if (length(num_burn_in) == 1) {
    num_burn_in <- rep(num_burn_in, iter)
  } else if ((length(num_burn_in) > 1) && (length(num_burn_in) != iter)) {
    stop("Length of number of burn-in must equal to number of iterations!")
  }
  
  if (length(num_iterations_after_burn_in) == 1) {
    num_iterations_after_burn_in <- rep(num_iterations_after_burn_in, iter)
  } else if ((length(num_iterations_after_burn_in) > 1) && (length(num_iterations_after_burn_in) != iter)) {
    stop("Length of number of iteration after burn-in must equal to number of iterations!")
  }
  
  if (length(num_reps_for_avg) == 1) {
    num_reps_for_avg <- rep(num_reps_for_avg, iter)
  } else if ((length(num_reps_for_avg) > 1) && (length(num_reps_for_avg) != iter)) {
    stop("Length of number of replicates for average must equal to number of iterations!")
  }
  
  if (length(num_permute_samples) == 1) {
    num_permute_samples <- rep(num_permute_samples, iter)
  } else if ((length(num_permute_samples) > 1) && (length(num_permute_samples) != iter)) {
    stop("Length of number of permutations of the response must equal to number of iterations!")
  }
  
  if ((!is.numeric(opt)) || (!(opt %in% c(1, 2)))){
    stop("opt must be a numeric value of either 1 or 2.")
  }
  
  if ((!is.numeric(iter)) || (iter <= 0)){
    stop("iter must be a positive interger.")
  }
  
  if (!is.logical(out_sample)){
    stop("out_sample must be a logical variable.")
  }
  
  if ((!is.numeric(train_ratio)) || (train_ratio <= 0) || (train_ratio > 1)){
    stop("train_ratio must be a number between 0 and 1.")
  }
  
  if (!is.logical(Lzero)){
    stop("Lzero must be a logical variable.")
  }
  
  if ((!is.numeric(K)) || (K <= 0)){
    stop("K must be a positive integer.")
  }
  
  if (!is.logical(writeLog)){
    stop("writeLog must be a logical variable.")
  }
  
  if (is.null(colnames(X))){
    colnames(X) = paste("V", seq(from = 1, to = ncol(X), by = 1), sep = "")
  }
  
  # Get column names if primary feature names are not provided
  if (is.null(head)) {
    head <- colnames(X)
  }
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  ### Generating training set
  if (out_sample == TRUE) {
    n <- nrow(X)
    train_idx <- sample(1:n, floor(train_ratio * n))
  } else {
    train_idx <- NULL
  }
  
  # Calculate marginal correlation
  # Scalars will cause warning
  cor_mat <- suppressWarnings(apply(X, 2, function(x) abs(cor(x, y))))
  
  # Remove X cols that are independent of Y
  cor_mat[is.na(cor_mat)] <- 0
  zero_idx <- which(cor_mat == 0)
  if (length(zero_idx) > 0){
    X <- X[, -zero_idx]
    head <- head[-zero_idx]
    if (!is.null(dimen)) {
      dimen <- dimen[-zero_idx]
    }
  }
  
  # Capture size of iBART generated space and selected space
  iBART_gen_size <- rep(0, iter + 1)
  iBART_sel_size <- rep(0, iter + 1)
  
  iBART_gen_size[1] <- ncol(X)
  ### iBART for iter
  cat("Start iBART descriptor generation and selection... \n")
  for (i in 1:iter) {
    cat(paste("Iteration", i, "\n", sep = " "))
    cat("iBART descriptor selection... \n")
    ### BART variable selection
    if (i == 1) {
      BART_selection <- BART_iter(X = X, y = y,
                                  head = head,
                                  dimen = dimen,
                                  X_selected = NULL,
                                  head_selected = NULL,
                                  dimen_selected = NULL,
                                  num_trees = num_trees[i],
                                  num_burn_in = num_burn_in[i],
                                  num_iterations_after_burn_in = num_iterations_after_burn_in[i],
                                  num_reps_for_avg = num_reps_for_avg[i],
                                  num_permute_samples = num_permute_samples[i],
                                  standardize = standardize,
                                  train_idx = train_idx,
                                  seed = seed)
    } else {
      BART_selection <- BART_iter(X = X, y = y,
                                  head = head,
                                  dimen = dimen,
                                  X_selected = X_selected,
                                  head_selected = head_selected,
                                  dimen_selected = dimen_selected,
                                  num_trees = num_trees[i],
                                  num_burn_in = num_burn_in[i],
                                  num_iterations_after_burn_in = num_iterations_after_burn_in[i],
                                  num_reps_for_avg = num_reps_for_avg[i],
                                  num_permute_samples = num_permute_samples[i],
                                  standardize = standardize,
                                  train_idx = train_idx,
                                  seed = seed)
    }
    iBART_sel_size[i] = ncol(BART_selection$X_selected)
    
    ### Feature engineering via operations
    if (opt == 1){
      if ((i %% 2) == 1) {
        cat("Constructing descriptors using unary operators... \n")
        Operator_output <- unaryOperation(BART_selection, sin_cos, apply_pos_opt_on_neg_x)
      } else {
        cat("Constructing descriptors using binary operators... \n")
        Operator_output <- binaryOperation(BART_selection, sin_cos)
      }
    } else if (opt == 2){
      # binary first then unary
      if ((i %% 2) == 1) {
        cat("Constructing descriptors using binary operators... \n")
        Operator_output <- binaryOperation(BART_selection, sin_cos)
      } else {
        cat("Constructing descriptors using unary operators... \n")
        Operator_output <- unaryOperation(BART_selection, sin_cos, apply_pos_opt_on_neg_x)
      }
    }
    
    ### Attach output
    X <- Operator_output$X
    head <- Operator_output$head
    dimen <- Operator_output$dimen
    
    X_selected <- Operator_output$X_selected
    head_selected <- Operator_output$head_selected
    dimen_selected <- Operator_output$dimen_selected
    
    iBART_gen_size[i + 1] <- ncol(X)
  }
  
  ### LASSO variable selection
  cat("BART iteration done! \n")
  cat("LASSO descriptor selection... \n")
  LASSO_selection <- LASSO(X = X, y = y,
                           head = head,
                           dimen = dimen,
                           train_idx = train_idx)
  
  ### Attach output
  X_selected <- LASSO_selection$X_selected
  head_selected <- LASSO_selection$head_selected
  dimen_selected <- LASSO_selection$dimen_selected
  LASSO_in_sample_RMSE <- LASSO_selection$In_sample_RMSE
  LASSO_out_sample_RMSE <- LASSO_selection$Out_sample_RMSE
  
  iBART_sel_size[iter + 1] <- ncol(X_selected)
  
  ### Calculate Least Squares RMSEs
  if (is.null(train_idx)) {
    X_train <- X_selected
    y_train <- y
    X_test <- NULL
    y_test <- NULL
  } else {
    # Training data
    X_train <- X_selected[train_idx, ]
    y_train <- y[train_idx]
    
    # Testing data
    X_test <- X_selected[-train_idx, ]
    y_test <- y[-train_idx]
  }
  
  # LS <- lm(y_train ~ ., data = as.data.frame(cbind(y_train, X_train)))
  # coef <- as.vector(LS$coefficients)
  # coef[is.na(coef)] <- 0
  # yhat_train <- cbind(rep(1, length(y_train)), X_train) %*% coef
  # LS_in_sample_RMSE <- sqrt(mean((yhat_train - y_train)^2))
  # 
  # if (!is.null(train_idx)) {
  #   yhat_test <- cbind(rep(1, length(y_test)), X_test) %*% coef
  #   LS_out_sample_RMSE <- sqrt(mean((yhat_test - y_test)^2))
  # }
  
  if (Lzero == TRUE){
    #### L-zero regression
    cat("L-zero regression... \n")
    p <- ncol(X_train)
    best_model <- list()
    best_names <- list()
    rmse_k_model_in <- rep(0, min(p, K))
    rmse_k_model_out <- rep(0, min(p, K))
    if (AIC == TRUE) aic <- c()
    for (k in 1:min(p, K)) {
      k_model <- k_var_model(X_train, y_train, X_test, y_test, k, parallel = FALSE)
      rmse_k_model_out[k] <- k_model$rmse_out
      rmse_k_model_in[k] <- k_model$rmse_in
      best_model[[k]] <- k_model$model
      best_names[[k]] <- k_model$names
      if (AIC == TRUE) aic[k] <- AIC(k_model$model)
    }
    if (AIC == TRUE) {
      lowest_aic_idx <- which.min(aic)
      aic_model <- best_model[[lowest_aic_idx]]
      aic_names <- best_names[[lowest_aic_idx]]
    }
  }
  
  end_time <- Sys.time()
  cat(paste("Total time:", difftime(end_time, start_time, units = "secs"), "secs \n", sep = " "))
  
  if (writeLog == TRUE) {
    if (is.null(count)) {
      filename <- paste("output.txt", sep = "")
    } else {
      filename <- paste("output", count, ".txt", sep = "")
    }
    data_log <- file(filename, "w")
    
    ############# Print Output ##############
    writeLines(paste("seed:", seed), data_log)
    writeLines("LASSO in-sample RMSE:", data_log)
    writeLines(c(as.character(LASSO_in_sample_RMSE), "\n"), data_log)
    if (out_sample == TRUE) {
      writeLines("LASSO out-sample RMSE:", data_log)
      writeLines(c(as.character(LASSO_out_sample_RMSE), "\n"), data_log)
    }
    writeLines("Selected Descriptors:", data_log)
    writeLines(c(head_selected, "\n"), data_log)
    if (Lzero == TRUE) {
      for (k in 1:min(p, K)){
        writeLines("----------------", data_log)
        writeLines(paste("No of descriptors:", k), data_log)
        writeLines("----------------", data_log)
        if (out_sample == TRUE){
          writeLines(c(paste("RMSE out-sample:", rmse_k_model_out[k]), "\n"), data_log)
        }
        writeLines(c(paste("RMSE in-sample:",  rmse_k_model_in[k]), "\n"), data_log)
        writeLines("Descriptors:", data_log)
        writeLines(c(best_names[[k]], "\n"), data_log)
      }
    }
    writeLines(paste("Time:", end_time - start_time), data_log)
    close(data_log)
  }
  
  iBART_output <- list(X_selected = X_selected,
                       descriptor_names = head_selected,
                       iBART_gen_size = iBART_gen_size,
                       iBART_sel_size = iBART_sel_size,
                       LASSO_model = LASSO_selection$LASSO_model,
                       LASSO_in_sample_RMSE = LASSO_in_sample_RMSE,
                       LASSO_out_sample_RMSE = if (out_sample) LASSO_out_sample_RMSE else NA,
                       Lzero_model = if (Lzero) best_model else NULL,
                       Lzero_names = if (Lzero) best_names else NULL,
                       Lzero_in_sample_RMSE = if (Lzero) rmse_k_model_in else NA,
                       Lzero_out_sample_RMSE = if (out_sample && Lzero) rmse_k_model_out else NA,
                       Lzero_aic_model = if (Lzero && AIC) aic_model else NULL,
                       Lzero_aic_names = if (Lzero && AIC) aic_names else NULL,
                       runtime = end_time - start_time)
  return(iBART_output)
}
