options(java.parameters = "-Xmx10g") # Allocate 10GB of memory for Java. Must be called before library(iBART)
library(iBART)
load("data/single_atom_catalysis.RData")

#### iBART ####
iBART_results <- iBART(X = X, y = y,
                       head = head,  # colnames of X
                       unit = unit,  # units of X
                       opt = c("binary", "unary", "binary"), # binary operator first
                       out_sample = FALSE,
                       Lzero = TRUE,
                       K = 5,
                       seed = 888)

#### non-OIS ####
set.seed(123)
model_no_OIS <- k_var_model(X_train = X, y_train = y, k = 3, parallel = FALSE)


#### Figure 7 ####
library(ggplot2)
library(ggpubr)
model_OIS <- iBART_results$Lzero_model[[3]]

# Prepare data for plotting
data_OIS <- data.frame(y = y, y_hat = model_OIS$fitted.values)
data_no_OIS <- data.frame(y = y, y_hat = model_no_OIS$models$fitted.values)

p1 <- ggplot(data_OIS, aes(x = y_hat, y = y)) +
  geom_point() +
  geom_abline() +
  xlim(c(min(data_OIS$y_hat, data_OIS$y) - 0.2, max(data_OIS$y_hat, data_OIS$y) + 0.2)) +
  ylim(c(min(data_OIS$y_hat, data_OIS$y) - 0.2, max(data_OIS$y_hat, data_OIS$y) + 0.2)) +
  xlab("") +
  ylab("") +
  annotate("text", x = -12, y = -3, parse = TRUE,
           label = paste("R^{2} ==", round(summary(model_OIS)$r.squared, 4)))
p2 <- ggplot(data_no_OIS, aes(x = y_hat, y = y)) +
  geom_point() +
  geom_abline() +
  xlim(c(min(data_no_OIS$y_hat, data_no_OIS$y) - 0.2, max(data_no_OIS$y_hat, data_no_OIS$y) + 0.2)) +
  ylim(c(min(data_no_OIS$y_hat, data_no_OIS$y) - 0.2, max(data_no_OIS$y_hat, data_no_OIS$y) + 0.2)) +
  xlab("") +
  ylab("") +
  annotate("text", x = -12, y = -3, parse = TRUE,
           label = paste("R^{2} ==", round(summary(model_no_OIS$models)$r.squared, 4)))
fig <- ggarrange(p1, p2,
                 labels = c("OIS", "non-OIS"),
                 ncol = 2, nrow = 1)
annotate_figure(fig,
                bottom = text_grob("Predicted binding energy from descriptors (eV)"),
                left = text_grob("DFT binding energy (eV)", rot = 90))
