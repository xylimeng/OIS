# iBART

R package and code to reproduce simulation and real data analysis results from the paper "Operator-induced structural variable selection for identifying materials genes".

## Installation

Before installing the iBART package in R, you first need to install Java JDK and rJava R package. 

### Install Java JDK (not JRE)

Download [Java 17 JDK or above](https://www.oracle.com/java/technologies/javase/jdk17-archive-downloads.html) and install it properly. Then run `R CMD javareconf` from the command line to configure Java in R. iBART requires bartMachine and rJava which require Java JDK; Java JRE won't work!

### Install rJava

Run `install.packages("rJava", INSTALL_opts = "--no-multriarch")` within R. To reproduce results in the paper, please install `rJava 1.0-4`.

### Install bartMachine

Run `install.packages("bartMachine", INSTALL_opts = "--no-multiarch")` within R. To reproduce results in the paper, please install `bartMachineJARs 1.1` and `bartMachine 1.2.6`. If you experience error, please see the [bartMachine repo](https://github.com/kapelner/bartMachine) for detailed instructions.


### Install glmnet

Run `install.packages("glmnet")` within R. To reproduce results in the paper, please install `glmnet 4.1-1`.

### Install iBART via devtools

Run `devtools::install_github("xylimeng/OIS")` within R or run `devtools::install_github("xylimeng/OIS", build_vignettes = TRUE)` if you want to build the vignettes; this can take a while.


## Reproduce iBART Results in Paper

R scripts for reproducing iBART results and figures are stored in the folder `code/`.

### Sec_3.2_unary
* `Sec_3.2_main.R` produces unary portion of Figures 2(a) and 2(b). 
* `operations.R` contains helper functions to generate descriptors.
* `x_***.R` produces results when `***` is the true descriptor. For example, `x_abs.R` corresponds to the model $y = 10|x_1| + \varepsilon$. These are required to run `Sec_3.2_main.R`

### Sec_3.3_binary
* `Sec_3.3_main.R` produces binary portion of Figures 2(a) and 2(b). 
* `operations.R` contains helper functions to generate descriptors.
* `x_***.R` produces results when `***` is the true descriptor. For example, `x_add.R` corresponds to the model $y = 10(x_1+x_2) + \varepsilon$. These R scripts are required to run `Sec_3.3_main.R`

### Sec_3.4_complex_model
* `Sec_3.4_main.R` produces iBART results in Figure 3.

### Sec_3.5_large_p
* `Sec_3.5_main.R` produces Figure 4 for $p \in \{20, 50, 100, 200\}$. Results for $p = 10$ are the same as in Sec 3.4.

### Sec_3.6_misspecified_model
* `Sec_3.6_main.R` produces Figure 5.

### Sec_4_real_data
* `Sec_4_out_sample_main.R` produces iBART results in Figure 6.
* `Sec_4_full_data_main.R` produces Figure 7.

## R Session Info
```
R version 4.0.5 (2021-03-31)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 22621)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252 
[2] LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] glmnet_4.1-1        Matrix_1.3-4        bartMachine_1.2.6  
 [4] missForest_1.4      itertools_0.1-3     iterators_1.0.13   
 [7] foreach_1.5.1       randomForest_4.6-14 bartMachineJARs_1.1
[10] rJava_1.0-4        

loaded via a namespace (and not attached):
[1] lattice_0.20-44  codetools_0.2-18 grid_4.0.5       splines_4.0.5   
[5] tools_4.0.5      survival_3.2-11  parallel_4.0.5   compiler_4.0.5  
[9] shape_1.4.6     
```
