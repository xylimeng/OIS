# OIS
Variable and Operator Selection for feature engineering.

# Instruction
- The `R` code is tested on `R v4.0.0, v4.0.5, and v4.1.0`.
- `R` packages `bartMachine v1.2.6` and `glmnet v4.1-1` are required. 
- `bartMachine` requires `Java JDK`. Visit https://www.oracle.com/java/technologies/javase/jdk13-archive-downloads.html to download `Java JDK v13.0.2`.
- Please see https://github.com/kapelner/bartMachine for installation instruction for `bartMachine` if installation from R CRAN failed.

# Reproduce results in OIS paper
## Reproduce Section 3.2
- Make sure `operations.R` file from `/Sec_3.2_unary` is in your working directory.
- Run `x_complex_abs.R` to reproduce the simulation when `|x_1|` is the true descriptor, etc.
- 
## Reproduce Section 3.3
- Make sure `operations.R` file from `/Sec_3.3_binary` is in your working directory.
- Run `x_add.R` to reproduce the simulation when `x_1+x_2` is the true descriptor, etc.

## Reproduce Section 3.4
- Run `Sec_3.4_main.R` to reproduce results in Section 3.4

## Reproduce Section 4
- Run `Sec_4_full_data_main.R` to reproduce the results trained on full data.
- Run `Sec_4_out_sample_main.R` to reproduce the out-of-sample results.

## Reproduce Section 4 Plots
```
options(java.parameters = "-Xmx10g") # Allocate 10GB of memory for Java
library(bartMachine)
library(glmnet)
library(ggplot2)
library(ggpubr)
source("iBART.R")
load("Connor_data.RData")

# Run iBART
iBART_results <- iBART(X = X, y = y, 
                       head = head, 
                       dimen = dimen, 
                       opt = 2, # binary operator first
                       iter = 3,
                       out_sample = FALSE, 
                       train_ratio = 1, 
                       K = 5,
                       writeLog = TRUE, 
                       count = NULL, 
                       seed = 888)

# Print in sample RMSE for the best subset selection models
in_sample_RMSE <- iBART_results$Lzero_in_sample_RMSE
names(in_sample_RMSE) <- c("1 Descriptor", "2 Descriptors", "3 Descriptors",
                           "4 Descriptors", "5 Descriptors")
in_sample_RMSE

# Plot y vs yhat
plots <- list()
for (i in 1:5) {
  fitted_data <- data.frame(y = y,
                            y_hat = iBART_results$Lzero_model[[i]]$fitted.values)
  plots[[i]] <- ggplot(fitted_data, aes(x = y_hat, y = y)) +
    geom_point() +
    geom_abline() +
    xlim(c(min(fitted_data$y_hat, fitted_data$y) - 0.2, max(fitted_data$y_hat, fitted_data$y) + 0.2)) +
    ylim(c(min(fitted_data$y_hat, fitted_data$y) - 0.2, max(fitted_data$y_hat, fitted_data$y) + 0.2)) +
    xlab("") +
    ylab("") +
    annotate("text", x = -12, y = -3, parse = TRUE,
             label = paste("R^{2} ==", round(summary(iBART_results$Lzero_model[[i]])$r.squared, 4)))
}
label <- sapply(1:5, function(x) paste(x, "D", sep = ""))
fig <- ggarrange(plotlist = plots,
                 labels = label)
annotate_figure(fig,
                bottom = text_grob("Predicted binding energy from descriptors (eV)"),
                left = text_grob("DFT binding energy (eV)", rot = 90))
fig

# Generate size plot
iteration <- rep(0:3, 2)
type <- rep(c("Selected", "Generated"), each = 4)
size <- c(iBART_results$iBART_sel_size, iBART_results$iBART_gen_size)
iBART_data <- data.frame(size, type, iteration)
ggplot(iBART_data, aes(x = iteration, y = size,
                       colour = type, group = type)) +
  theme(text = element_text(size = 15)) +
  geom_line(size = 1) +
  geom_point(size = 3, shape = 21, fill = "white") +
  geom_text(data = iBART_data, aes(label = size, y = size + 5, group = type),
            position = position_dodge(0), size = 5, colour = "blue") +
  labs(x = "Iteration", y = "Number of descriptors") +
  scale_x_continuous(breaks = (0:4))

```

# R Session Info
```
R version 4.0.5 (2021-03-31)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19043)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252   LC_CTYPE=English_United States.1252    
[2] LC_MONETARY=English_United States.1252  LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  
[6] methods   base     

other attached packages:
 [1] glmnet_4.1-1        Matrix_1.3-4        bartMachine_1.2.6   missForest_1.4      
 [5] itertools_0.1-3     iterators_1.0.13    foreach_1.5.1      
 [8] randomForest_4.6-14 bartMachineJARs_1.1 rJava_1.0-4        

loaded via a namespace (and not attached):
[1] lattice_0.20-44  codetools_0.2-18 grid_4.0.5       splines_4.0.5    tools_4.0.5      
[6] survival_3.2-11  parallel_4.0.5   compiler_4.0.5   shape_1.4.6
```
