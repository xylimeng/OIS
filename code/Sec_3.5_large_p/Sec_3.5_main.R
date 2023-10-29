options(java.parameters = "-Xmx16g") # Allocate 16GB of memory for Java
                                     # Must be called before library(iBART)
library(iBART)

TruePositive <- function(names) {
  TP <- 0
  if (any(names == "(exp(x.1)-exp(x.2))^2") | any(names == "|exp(x.1)-exp(x.2)|^2")) {
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
  if ((TP + FP) == 0) {
    return(0)
  } else {
    return(TP / (TP + FP))
  }
}

Recall <- function(TP, FN) {
  if ((TP + FN) == 0) {
    return(0)
  } else {
    return(TP / (TP + FN))
  }
}

F1 <- function(precision, recall) {
  if (precision + recall == 0) {
    return(0)
  } else{
    return(2 * precision * recall / (precision + recall))
  }
}

n <- 250                  # Number of samples
ps <- c(200, 20, 50, 100) # Number of initial primary features
s <- 100                  # Number of replicates
iBART_F1 <- data.frame(F1 = rep(0, s*length(ps)), p = rep(ps, each = s))
counter <- 1

# Result for p = 10 can be reproduced using Sec_3.4_main.R
# We will import that result at the end to reproduce Figure 4

for (p in ps) {
  # main_seed <- ifelse(p == 200, 1234, 123)
  # set.seed(main_seed)
  set.seed(123)
  seeds <- sample.int(1000, size = 50)
  seeds <- c(seeds, sample.int(1000, size = 50))
  for (j in 1:s) {
    set.seed(seeds[j])
    ################################ Generate data ################################
    X <- matrix(runif(n * p, min = -1, max = 1), nrow = n, ncol = p)
    colnames(X) <- paste("x.", seq(from = 1, to = p, by = 1), sep = "")
    y <- 15 * (exp(X[, 1]) - exp(X[, 2]))^2 + 20 * sin(pi * X[, 3] * X[, 4]) + rnorm(n, mean = 0, sd = 0.5)

    iBART_results <- iBART(X = X, y = y,
                           head = colnames(X),
                           opt = c("unary", "binary", "unary"), # unary operator first
                           sin_cos = TRUE,
                           apply_pos_opt_on_neg_x = FALSE,
                           Lzero = TRUE,
                           K = 4,
                           aic = TRUE,
                           standardize = FALSE,
                           seed = seeds[j])

    # original model
    iBART_TP <- TruePositive(iBART_results$descriptor_names)
    iBART_FP <- FalsePositive(iBART_results$descriptor_names, iBART_TP)
    iBART_FN <- FalseNegative(iBART_TP)
    iBART_precision <- Precision(iBART_TP, iBART_FP)
    iBART_recall <- Recall(iBART_TP, iBART_FN)
    iBART_F1$F1[counter] <- F1(iBART_precision, iBART_recall)

    cat("p = ", p, ", Iteration: ", j, "/100... \n", sep = "")
    counter <- counter + 1
  }
}

iBART_F1$p <- rep(ps, each = 100)
iBART_F1$p <- factor(iBART_F1$p, levels = ps)


#### Figure 4 ####
library(ggplot2)
ggplot(iBART_F1, aes(x = p, y = F1)) +
  geom_boxplot() + scale_x_discrete(name = "Input dimension p") +
  scale_y_continuous(name = "F1 Scores") +
  ggtitle("Boxplot of iBART F1 scores")
