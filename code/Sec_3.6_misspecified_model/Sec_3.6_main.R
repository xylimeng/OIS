set.seed(123)
options(java.parameters = "-Xmx10g") # Allocate 10GB of memory for Java
                                     # Must be called before library(iBART)
library(iBART)
library(MASS)

n <- 250 # Number of samples
p <- 10  # Number of primary features
s <- 100 # Number of replicates
seeds <- sample.int(10000, size = 100)
iBART_names <- iBART_cor <- iBART_desp_val <- list()
iBART_rmse <- TRUE_rmse <- rep(0, s)

for (j in 1:s) {
  set.seed(seeds[j])
  ################################ Generate data ################################
  X <- matrix(runif(n * p, min = 0, max = 3), nrow = n, ncol = p)
  colnames(X) <- paste("x.", seq(from = 1, to = p, by = 1), sep = "")
  y <- X[, 1]^(1.7) + rnorm(n, mean = 0, sd = 1)

  iBART_results <- iBART(X = X, y = y,
                         head = colnames(X),
                         opt = c("unary"),
                         sin_cos = TRUE,
                         apply_pos_opt_on_neg_x = FALSE,
                         Lzero = TRUE,
                         K = 4,
                         aic = TRUE,
                         standardize = FALSE,
                         seed = seeds[j])

  # Store results
  iBART_names[[j]] <- iBART_results$descriptor_names
  iBART_rmse[j] <- iBART_results$iBART_in_sample_RMSE
  iBART_cor[[j]] <- cor(y, iBART_results$X_selected)
  names(iBART_cor[[j]]) <- iBART_results$descriptor_names
  iBART_desp_val[[j]] <- iBART_results$X_selected
  TRUE_rmse[j] <- sqrt(mean((y - X[, 1]^(1.7))^2))
  
  cat("Iteration: ", j, "/100... \n", sep = "")
}


library(ggplot2)
library(ggpubr)

#### Figure 5A ####
iBART_names_unlist <- unlist(iBART_names)
iBART_names_tb <- as.data.frame(table(iBART_names_unlist))
colnames(iBART_names_tb) <- c("Descriptor", "Freq")
iBART_names_tb <- iBART_names_tb[order(iBART_names_tb$Freq, decreasing = TRUE), ]
iBART_names_tb$Descriptor <- factor(iBART_names_tb$Descriptor,
                                    levels = iBART_names_tb$Descriptor)
iBART_names_tb$Corr <- rep(NA, nrow(iBART_names_tb))
iBART_cor_unlist <- unlist(iBART_cor)
for (i in 1:nrow(iBART_names_tb)) {
  des <- iBART_names_tb$Descriptor[i]
  iBART_names_tb$Corr[i] <- mean(iBART_cor_unlist[which(names(iBART_cor_unlist) == des)])
}
iBART_names_tb$Corr <- round(iBART_names_tb$Corr, digits = 2)


p_freq <- ggplot(iBART_names_tb, aes(x = Descriptor, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(Freq, " (", Corr, ")")), vjust = -0.5, size = 3.5) +
  labs(y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12))


#### Figure 5B ####
dat <- data.frame(RMSE = c(iBART_rmse, TRUE_rmse),
                  Model = c("iBART", "True model"))
p_rmse <- ggplot(dat, aes(x = Model, y = RMSE, color = Model)) +
  geom_boxplot() +
  theme(legend.position = "none", axis.text = element_text(size = 12))


#### Combine Figures 5A and 5B ####
ggarrange(plotlist = list(p_freq, p_rmse), labels = c("A", "B"), ncol = 2, nrow = 1, widths = c(2, 1))
