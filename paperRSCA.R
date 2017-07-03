############################################################
#
#  R code for the software paper: 
#  RSCA: Regularized Simultaneous Component Analysis for
#  Data Integration in R
#
#  Zhengguo Gu, Katrijn Van Deun
#  Department of Methodology and Statistics
#  Tilburg University
#  email: z.gu@uvt.nl (Zhengguo)
#         K.VanDeun@uvt.nl (Katrijn)
#
#############################################################

### 0. install package
devtools::install_github("ZhengguoGu/RSCA")

############### SECTION 3.1 ##################################

### 1. load the data in RSCA
library(RegularizedSCA)
names(Herring)

### 2. pre-process the data
ChemPhy <- mySTD(Herring$Herring_ChemPhy)
Sensory <- mySTD(Herring$Herring_Sensory)
herring_data <- c(ChemPhy, Sensory)
num_var <- c(dim(ChemPhy)[2], dim(Sensory)[2])

### 3. VAF, PCA-GCA, and DISCO-SCA
VAF(DATA = herring_data, Jk = num_var, R = 10)

pca_gca(DATA = herring_data, Jk = num_var)

discoresult <- DISCOsca(DATA = herring_data, R = 4, Jk = num_var)
discoresult$comdist

### 4. cross-validation RSCA
set.seed(111)
results_cv <- cv_sparseSCA(DATA = herring_data, Jk = num_var, R = 4)
results_cv$plot[[1]]

results_cv$Glasso_values
results_cv$Lasso_values

results_cv$plot[[2]] 

results_cv$RecommendedLambda

# the final model
set.seed(111)
final_results <- sparseSCA(herring_data, num_var, R = 4, 
                           LASSO = 0.5281094, 
                           GROUPLASSO = 1.028915, 
                           NRSTART = 20)
final_results$Pmatrix

# undo the shrinkage
final_Loading <- undoShrinkage(herring_data, R = 4, 
                                  Phat = final_results$Pmatrix)
final_Loading$Pmatrix

### 5. Interpret the Pmatrix - Heatmap (Note that the following code is not in the article)
# We draw a heatmap 
Pmat <- final_comLoading$Pmatrix
keepname <- rownames(Pmat)
colnames(Pmat) <- c('Component 1', 'Component 2', 'Component 3', 'Component 4')
write.csv(Pmat, file='sparseresults.csv')

library(ggplot2)
names <- rownames(Pmat)
component <- colnames(Pmat)
PmatVec <- c(Pmat)
names <- rep(names, 4)
component <- rep(component, each = 20)

# note that part of the ggplot code below is from https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
# which is a website for drawing heatmap using ggplot2. 
Pmat_dataframe <- data.frame(Loadings = PmatVec, Variables = factor(names, ordered = T, levels = keepname), Components = component)

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

# 1. cv_structuredSCA()
targetmatrix <- matrix(c(1, 1, 1, 1, 1, 0, 0, 1), nrow = 2, ncol = 4)
targetmatrix	

maxLGlasso(DATA = herring_data, num_var, R = 4)$Lasso

set.seed(115)
results_cvS <- cv_structuredSCA(DATA = herring_data, Jk = num_var, R = 4, 
                                Target = targetmatrix,
                                Position = c(1, 2, 3, 4),
                                LassoSequence = seq(from = 0.0000001, 
                                                    to = 4.278383, 
                                                    length.out = 200))
results_cvS$plot
results_cvS$LassoRegion
results_cvS$RecommendedLasso

set.seed(115)
result_str <- structuredSCA(DATA = herring_data, Jk = num_var, R = 4,
                            Target = targetmatrix,
                            Position = c(1, 2, 3, 4), 
                            LASSO = 0.881476)

final_comLoadingS <- undoShrinkage(DATA = herring_data, R = 4, 
                                   Phat = result_str$Pmatrix)
final_comLoadingS$Pmatrix

### again, we draw a healmap
PmatS <- final_comLoadingS$Pmatrix
keepname <- rownames(PmatS)
colnames(PmatS) <- c('Component 1', 'Component 2', 'Component 3', 'Component 4')
write.csv(PmatS, file='sparseresultsStr.csv')

library(ggplot2)
names <- rownames(PmatS)
component <- colnames(PmatS)
PmatSVec <- c(PmatS)
names <- rep(names, 4)
component <- rep(component, each = 20)

# note that part of the ggplot code below is from https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
# which is a website for drawing heatmap using ggplot2. 
PmatS_dataframe <- data.frame(Loadings = PmatSVec, Variables = factor(names, ordered = T, levels = keepname), Components = component)

p <- ggplot(PmatS_dataframe, aes(x = Components, y = Variables) )+
  geom_tile(aes(fill = Loadings), colour = "white") +
  scale_fill_gradient2(low="green", mid = "black", high = "red") 

base_size <- 9
p + theme_grey(base_size = base_size) + labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))

