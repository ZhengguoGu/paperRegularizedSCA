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

############### SECTION 3.1 ##################################

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

### 5. Interpret the Pmatrix - Heatmap (Note that the following code is not in the article)
# We draw a heatmap 
Pmat <- final_comLoading$Pmatrix
rownames(Pmat) <- c(paste("D:", colnames(depressed_data)), paste("S:", colnames(schizophrenic_data)))
keepname <- rownames(Pmat)
colnames(Pmat) <- c('Component 1', 'Component 2', 'Component 3')
write.csv(Pmat, file='sparseresults.csv')

library(ggplot2)
names <- rownames(Pmat)
component <- colnames(Pmat)
PmatVec <- c(Pmat)
names <- rep(names, 3)
component <- rep(component, each = 34)

# note that part of the ggplot code below is from https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
# which is a website for drawing heatmap using ggplot2. 
Pmat_dataframe <- data.frame(Loadings = PmatVec, Variables = ordered(names, labels = keepname), Components = component)

p <- ggplot(Pmat_dataframe, aes(x = Components, y = Variables) )+
  geom_tile(aes(fill = Loadings), colour = "white") +
  scale_fill_gradient2(low="green", mid = "black", high = "red") 

base_size <- 9
p + theme_grey(base_size = base_size) + labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))

### 6. The T matrix 
final_comLoading$Tmatrix

################ SECTION 3.2 ################################################
# 1. DISCO-SCA
results_disco <- DISCOsca(DATA = psych_data, R = 3, Jk = num_var)
results_disco$comdist

# 2. cv_structuredSCA()
targetmatrix <- matrix(c(1,1,0,1,1,0), nrow = 2, ncol = 3)
targetmatrix	

maxLGlasso(DATA = psych_data, num_var, R = 3)$Lasso

set.seed(110)
results_cvS <- cv_structuredSCA(DATA = psych_data, Jk = num_var, R = 3, 
                                Target = targetmatrix,
                                NRSTARTS = 5, 
                                LassoSequence = seq(from = 0.0000001, 
                                to = 6.41176, 
                                length.out = 200))
results_cvS$plot
