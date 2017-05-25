############################################################
#
#  R code for the software paper: 
#  RSCA: Regularized Simultaneous Component Analysis for
#  Data Integration in R
#
#  Zhengguo Gu, Katrijn Van Deun
#  Department of Methodology and Statistics
#  Tilburg University
#  email: zhengguo.gu@uvt.nl
#
#############################################################

### 0. install package
devtools::install_github("ZhengguoGu/RSCA")

### 1. load the data in RSCA
library(RSCA)
attach(MezzichSolomon)
names(MezzichSolomon)

### 2. pre-process the data
depressed_data <- mySTD(MezzichSolomon$depressed)
schizophrenic_data <- mySTD(MezzichSolomon$schizophrenic)
psych_data <- cbind(depressed_data, schizophrenic_data)
num_var <- cbind(dim(depressed_data)[2], dim(schizophrenic_data)[2])

### 3. VAF and PCA-GCA
VAF(psych_data, num_var, R = 5)

pca_gca(psych_data, num_var, cor_min = 0.8)

### 4. cross-validation RSCA
set.seed(110)
results_cv <- cv_sparseSCA(DATA = psych_data, Jk = num_var, R = 3, NRSTARTS = 2)
results_cv$plot[[1]]

results_cv$Glasso_values
results_cv$Lasso_values

results_cv$plot[[2]] 

results_cv$RecommendedLambda

# the final model
set.seed(110)
final_results <- sparseSCA(psych_data, num_var, R = 3, 
                           LASSO = 0.7584501, 
                           GROUPLASSO = 0.7674313, 
                           NRSTART = 20)
final_results$Pmatrix

# undo the shrinkage
final_comLoading <- undoShrinkage(psych_data, R = 3, 
                                  Phat = final_results$Pmatrix)
final_comLoading$Pmatrix