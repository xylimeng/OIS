set.seed(123) # seed = 123 for p = 10, 20, 50, 100
              # seed = 1234 for p = 200
options(java.parameters = "-Xmx10g") # Allocate 10GB of memory for Java
library(bartMachine)
library(glmnet)
source("iBART.R")

TruePositive <- function(names) {
  TP <- 0
  if (any(names == "(exp(x.1)-exp(x.2))^2")) {
    TP <- TP + 1
  }
  if (any(names == "sin(pi*(x.3*x.4))")) {
    TP <- TP + 1
  }
  return(TP)
}

FalsePositive <- function(names, TP) {
  FP <- length(names) - TP
  return(FP)
}

FalseNegative <- function(TP) {
  FN <- 2 - TP
  return(FN)
}

Precision <- function(TP, FP) {
  return(TP / (TP + FP))
}

Recall <- function(TP, FN) {
  return(TP / (TP + FN))
}

F1 <- function(precision, recall) {
  return(2 * precision * recall / (precision + recall))
}

seeds <- sample.int(1000, size = 50)
BART_names <- BART_aic_names <- list()
BART_rmse <- BART_aic_rmse <- rep(0, 50)
BART_TP <- BART_FP <- BART_FN <- BART_precision <- BART_recall <- BART_F1 <- rep(0, 50)
BART_aic_TP <- BART_aic_FP <- BART_aic_FN <- BART_aic_precision <- BART_aic_recall <- BART_aic_F1 <- rep(0, 50)
BART_gen_size <- BART_sel_size <- matrix(0, nrow = 50, ncol = 4)

n <- 250
p <- 10 # change p here to reproduce results in Section 3.5

for (j in 1:50) {
  set.seed(seeds[j])
  ################################ Generate data ################################
  X <- matrix(runif(n * p, min = -1, max = 1), nrow = n, ncol = p)
  colnames(X) <- paste("x.", seq(from = 1, to = p, by = 1), sep = "")
  y <- 15 * (exp(X[, 1]) - exp(X[, 2]))^2 + 20 * sin(pi * X[, 3] * X[, 4]) + rnorm(n, mean = 0, sd = 0.5)
  
  iBART_results <- iBART(X = X, y = y,
                         head = colnames(X),
                         dimen = NULL,
                         opt = 1, # unary operator first
                         sin_cos = TRUE,
                         apply_pos_opt_on_neg_x = FALSE,
                         iter = 3,
                         Lzero = TRUE,
                         K = 4,
                         AIC = TRUE,
                         standardize = FALSE,
                         writeLog = FALSE,
                         count = j,
                         seed = 99)
  BART_gen_size[j, ] <- iBART_results$iBART_gen_size
  BART_sel_size[j, ] <- iBART_results$iBART_sel_size
  
  # original model
  BART_TP[j] <- TruePositive(iBART_results$descriptor_names)
  BART_FP[j] <- FalsePositive(iBART_results$descriptor_names, BART_TP[j])
  BART_FN[j] <- FalseNegative(BART_TP[j])
  BART_precision[j] <- Precision(BART_TP[j], BART_FP[j])
  BART_recall[j] <- Recall(BART_TP[j], BART_FN[j])
  BART_F1[j] <- F1(BART_precision[j], BART_recall[j])
  
  # AIC model
  BART_aic_TP[j] <- TruePositive(iBART_results$Lzero_aic_names)
  BART_aic_FP[j] <- FalsePositive(iBART_results$Lzero_aic_names, BART_aic_TP[j])
  BART_aic_FN[j] <- FalseNegative(BART_aic_TP[j])
  BART_aic_precision[j] <- Precision(BART_aic_TP[j], BART_aic_FP[j])
  BART_aic_recall[j] <- Recall(BART_aic_TP[j], BART_aic_FN[j])
  BART_aic_F1[j] <- F1(BART_aic_precision[j], BART_aic_recall[j])

  cat("Finished iteration: ", j, "/50... \n", sep = "")
}
boxplot(BART_F1, BART_aic_F1) # F1 boxplot
