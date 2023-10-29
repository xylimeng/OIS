options(java.parameters = "-Xmx10g") # Allocate 10GB of memory to Java;
                                     # Must be called before library(bartMachine)
library(bartMachine)
source("operations.R") # Load helper functions

S <- 100 # Number of replicates per operator
n <- 200 # Number of samples
p <- 5   # Number of primary features
operators <- c("id", "abs", "cos", "exp", "inv", "log", "sin", "sqre", "sqrt")

# Allocate data.frame to store results
df <- data.frame(TP = rep(0, S * length(operators)),
                 FP = rep(0, S * length(operators)),
                 Operators = rep(operators, each = S))
df$Operators = factor(df$Operators, levels = operators)

# Iterate over 9 different operators (models)
for (o in operators) {
  source(paste0("x_", o, ".R"))
  df$TP[df$Operators == o] <- eval(parse(text = paste0(o, "_TP")))
  df$FP[df$Operators == o] <- eval(parse(text = paste0(o, "_FP")))
}


library(ggplot2)
#### Figure 2(a) Unary ####
ggplot(df, aes(x = Operators, y = TP)) +
  geom_boxplot(position = position_dodge(1)) +
  ggtitle("Plot of TP (Unary)") +
  ylab("TP")


#### Figure 2(b) Unary ####
ggplot(df, aes(x = Operators, y = FP)) +
  geom_boxplot(position = position_dodge(1)) +
  ggtitle("Plot of FP (Unary)") +
  ylab("FP")
