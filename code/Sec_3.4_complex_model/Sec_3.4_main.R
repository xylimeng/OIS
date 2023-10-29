set.seed(123)
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

s <- 100 # Number of replicates
n <- 250 # Change n to 100 here to reproduce result in Supplementary Materials A.2.3
p <- 10  # Number of primary features
seeds <- sample.int(1000, size = 50)
seeds <- c(seeds, sample.int(1000, size = 50))
iBART_F1 <- iBART_aic_F1 <- rep(0, s)
iBART_gen_size <- iBART_sel_size <- matrix(0, nrow = s, ncol = 4)

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
  iBART_gen_size[j, ] <- iBART_results$iBART_gen_size
  iBART_sel_size[j, ] <- iBART_results$iBART_sel_size

  # original model
  iBART_TP <- TruePositive(iBART_results$descriptor_names)
  iBART_FP <- FalsePositive(iBART_results$descriptor_names, iBART_TP)
  iBART_FN <- FalseNegative(iBART_TP)
  iBART_precision <- Precision(iBART_TP, iBART_FP)
  iBART_recall <- Recall(iBART_TP, iBART_FN)
  iBART_F1[j] <- F1(iBART_precision, iBART_recall)

  # AIC model
  iBART_aic_TP <- TruePositive(iBART_results$Lzero_AIC_names)
  iBART_aic_FP <- FalsePositive(iBART_results$Lzero_AIC_names, iBART_aic_TP)
  iBART_aic_FN <- FalseNegative(iBART_aic_TP)
  iBART_aic_precision <- Precision(iBART_aic_TP, iBART_aic_FP)
  iBART_aic_recall <- Recall(iBART_aic_TP, iBART_aic_FN)
  iBART_aic_F1[j] <- F1(iBART_aic_precision, iBART_aic_recall)

  cat("Iteration: ", j, "/100... \n", sep = "")
}


library(ggplot2)
library(ggpubr)
#### Figure 3B ####
# Prepare data for plot
iBART_size <- data.frame(size = c(as.vector(iBART_gen_size), as.vector(iBART_sel_size)),
                         iter = rep(rep(0:3, each = 100), 2),
                         Stage = rep(c("Generated", "Selected"), each = 400))
iBART_size$iter <- as.character(iBART_size$iter)
medians <- aggregate(size ~ iter + Stage, iBART_size, median)

# Size plot
size_plot <- ggplot(data = iBART_size, aes(x = iter, y = size, fill = Stage)) +
  geom_boxplot(position=position_dodge(1)) +
  stat_summary(fun = median, aes(group = Stage),
               colour = "darkred", position = position_dodge(1),
               geom = "point", shape = 18, size = 3) +
  geom_text(data = medians, aes(label = size, y = size + 8, group = Stage),
            position = position_dodge(1), size = 4, colour = "blue") +
  labs(x = "Iteration", y = "Number of descriptors") +
  theme(axis.text = element_text(size = 12))


#### Figure 3A ####
# Prepare data for plot
F1_data <- data.frame(F1_scores = c(iBART_F1, iBART_aic_F1),
                      Methods = rep(1:2, each = s))
F1_data$Methods <- factor(F1_data$Methods, labels = c("iBART", "iBART+L0"))
F1_data_median <- aggregate(F1_scores ~ Methods, F1_data, median)
F1_data_median$F1_scores <- round(F1_data_median$F1_scores, digits = 2)

# F1 plot
F1_plot <- ggplot(F1_data, aes(x = Methods, y = F1_scores)) +
  geom_boxplot() +
  stat_summary(fun = median, colour = "darkred", geom = "point",
               shape = 18, size = 3, show.legend = FALSE) +
  geom_text(data = F1_data_median, aes(label = F1_scores, y = F1_scores),
            vjust = -0.4,
            colour = "blue") +
  scale_x_discrete(name = "Method") +
  scale_y_continuous(name = "F1 Scores") +
  theme(legend.position = "none")

ggarrange(plotlist = list(F1_plot, size_plot), labels = c("A", "B"), ncol = 2, nrow = 1, widths = c(0.8, 1))


