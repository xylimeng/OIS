set.seed(123)
options(java.parameters = "-Xmx10g") # Allocate 10GB of memory for Java. Must be called before library(iBART)
library(iBART)


#### Simulation Parameters ####
n <- 250 # Change n to 100 here to reproduce result in Supplementary Materials A.2.3
p <- 10  # Number of primary features


#### Generate Data ####
X <- matrix(runif(n * p, min = -1, max = 1), nrow = n, ncol = p)
colnames(X) <- paste("x.", seq(from = 1, to = p, by = 1), sep = "")
y <- 15 * (exp(X[, 1]) - exp(X[, 2]))^2 + 20 * sin(pi * X[, 3] * X[, 4]) + rnorm(n, mean = 0, sd = 0.5)


#### iBART ####
iBART_results <- iBART(X = X, y = y,
                       head = colnames(X),
                       opt = c("unary", "binary", "unary"), # unary operator first
                       sin_cos = TRUE,
                       apply_pos_opt_on_neg_x = FALSE,
                       Lzero = TRUE,
                       K = 4,
                       aic = TRUE,
                       standardize = FALSE,
                       seed = 123)

# iBART selected descriptors
iBART_results$descriptor_names

# iBART model
class(iBART_results$iBART_model)
coef(iBART_results$iBART_model, s = "lambda.min")

# Generated vs Selected Dimension plot
library(ggplot2)
df_dim <- data.frame(dim = c(iBART_results$iBART_sel_size, iBART_results$iBART_gen_size),
                     iter = rep(0:3, 2),
                     type = rep(c("Selected", "Generated"), each = 4))
ggplot(df_dim, aes(x = iter, y = dim, colour = type, group = type)) +
  theme(text = element_text(size = 15), legend.title = element_blank()) +
  geom_line(size = 1) +
  geom_point(size = 3, shape = 21, fill = "white") +
  geom_text(data = df_dim, aes(label = dim, y = dim + 10, group = type),
            position = position_dodge(0), size = 5, colour = "blue") +
  labs(x = "Iteration", y = "Number of descriptors") +
  scale_x_continuous(breaks = c(0, 1, 2, 3))
