options(java.parameters = "-Xmx10g") # Allocate 10GB of memory to Java;
                                     # Must be called before library(bartMachine)
library(bartMachine)
source("operations.R") # Load helper functions

S <- 100 # Number of replicates per operator
n <- 200 # Number of samples
p <- 5   # Number of primary features
operators <- c("abs_minus", "add", "minus", "multiply", "divide", "pi1")

# Allocate data.frame to store results
df <- data.frame(TP = rep(0, S * length(operators)),
                 FP = rep(0, S * length(operators)),
                 Operators = rep(operators, each = S))
df$Operators = factor(df$Operators, levels = operators)

# Iterate over 6 different operators (models)
for (o in operators) {
  source(paste0("x_", o, ".R"))
  df$TP[df$Operators == o] <- eval(parse(text = paste0(o, "_TP")))
  df$FP[df$Operators == o] <- eval(parse(text = paste0(o, "_FP")))
}


library(ggplot2)
#### Figure 2(a) Binary ####
p1 <- ggplot(df, aes(x = Operators, y = TP)) +
  geom_boxplot(position = position_dodge(1)) +
  ggtitle("Plot of TP (Binary)") +
  ylab("TP")


#### Figure 2(b) Unary ####
p2 <- ggplot(df, aes(x = Operators, y = FP)) +
  geom_boxplot(position = position_dodge(1)) +
  ggtitle("Plot of FP (Binary)") +
  ylab("FP")
