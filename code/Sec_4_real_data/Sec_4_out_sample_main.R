options(java.parameters = "-Xmx16g") # Allocate 16GB of memory for Java
                                     # Must be called before library(iBART)
library(iBART)
load("data/single_atom_catalysis.RData")
seeds <- c(12,   2583,  893,  535, 1174, 2552, 1386, 2841,  416, 2749,
           656,  1741,  902,  509, 3376, 5623, 5172, 5993, 3618, 3839,
           3421, 3478, 3427, 4639, 4961, 5634, 5158, 5402, 5470, 5395,
           3754, 5906, 4520, 5970, 4553, 5596, 4647, 3906, 5800, 8790,
           8051, 8862, 7166, 7867, 8358, 6389, 7827, 7724, 7625, 7389)
count <- 1
S <- length(seeds)
RMSE_out_sample <- matrix(0, nrow = S, ncol = 5)
colnames(RMSE_out_sample) <- paste0(1:5, "D")

for(seed in seeds){
  iBART_results <- iBART(X = X, y = y,
                         head = head,  # colnames of X
                         unit = unit,  # units of X
                         opt = c("binary", "unary", "binary"), # binary operator first
                         out_sample = TRUE,
                         train_ratio = 0.91,
                         Lzero = TRUE,
                         K = 5,
                         seed = seed)

  RMSE_out_sample[count, ] <- iBART_results$Lzero_out_sample_RMSE
  cat(paste("Finished iteration: ", count, "/50 \n", sep = ""))
  count <- count + 1
}

#### Figure 6 ####
library(ggplot2)
dat <- data.frame(RMSE = as.vector(RMSE_out_sample),
                  Method = rep("iBART", 250),
                  k = rep(as.character(1:5), each = 50))
means <- aggregate(RMSE ~ Method + k, dat, mean)
means$RMSE <- round(means$RMSE, 3)

ggplot(dat, aes(x = Method, y = RMSE, fill = k)) +
  geom_boxplot(position = position_dodge(1)) +
  stat_summary(
    fun = mean, aes(group = k),
    colour = "darkred", position = position_dodge(1),
    geom = "point", shape = 18, size = 3
  ) +
  geom_text(
    data = means, aes(label = RMSE, y = RMSE + 0.14, group = k),
    position = position_dodge(1), size = 3, colour = "blue"
  ) +
  ggtitle("Plot of out-of-sample RMSE") +
  ylab("Out-of-sample RMSE") +
  facet_wrap(~Method, ncol = 4, scales = "free_x")
