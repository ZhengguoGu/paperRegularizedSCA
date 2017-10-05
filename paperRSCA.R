############################################################
#  !!! The results should be reproduced on Windows
#  
#  R script for the software paper: 
#  RegularizedSCA: Regularized Simultaneous Component Analysis for
#  Data Integration in R
#
#  Zhengguo Gu, Katrijn Van Deun
#  Department of Methodology and Statistics
#  Tilburg University
#  email: z.gu@uvt.nl (Zhengguo)
#         K.VanDeun@uvt.nl (Katrijn)
#
#
#  NOTE: This R script was run on R version 3.3.0 on windows
#        Latest update: July 4, 2017
#############################################################

### 0. install package
install.packages("RegularizedSCA")
install.packages(pkgs="D://RegularizedSCA_0.5.0.tar.gz", repos = NULL)

############### SECTION: the RegularizedSCA package ##########################
####### subsection: Exploring the component structure and functionalities
### 1. load the package and data
library("RegularizedSCA")
names(Herring)  #one can check the names of the data file "Herring" 

### 2. pre-process the data
ChemPhy <- mySTD(Herring$Herring_ChemPhy)
Sensory <- mySTD(Herring$Herring_Sensory)
herring_data <- cbind(ChemPhy, Sensory)
num_var <- cbind(dim(ChemPhy)[2], dim(Sensory)[2])

### 3. VAF, PCA-GCA, and DISCO-SCA
vaf <- VAF(DATA = herring_data, Jk = num_var, R = 10)
summary(vaf)

pca_gca(DATA = herring_data, Jk = num_var) 
       #note: pca_gca() contains a user-computer interaction phase, once
       #we run pca_gca(DATA = herring_data, Jk = num_var), the console will 
       #display the eigenvalues of block 1 and ask whether the user wants to 
       #see the scree plot. If yes, then the user must enter 1; if No, enter 0.
       #Then the program will also ask how many components to retain for this block
       #and the user must give a number. 
       #The aforementioned procedure will be repeated from the first block till the 
       #last block.


discoresult <- DISCOsca(DATA = herring_data, R = 4, Jk = num_var)
summary(discoresult)

### 4. cross-validation 
set.seed(111)
results_cv <- cv_sparseSCA(DATA = herring_data, Jk = num_var, R = 4)
plot(results_cv)

results_cv$Lasso_values
results_cv$GLasso_values
summary(results_cv, disp = "full")
summary(results_cv)  #to check the recommended tuning parameter values

# the final model
set.seed(111)
final_results <- sparseSCA(herring_data, num_var, R = 4, 
                           LASSO = 0.5281094, 
                           GROUPLASSO = 1.028915, 
                           NRSTART = 20)
summary(final_results, disp = "full") 

# undo the shrinkage
final_Loading <- undoShrinkage(herring_data, R = 4, 
                                  Phat = final_results$Pmatrix)

summary(final_Loading)


### 5. Interpret the Pmatrix - Heatmap (Note that the following code is not in the article)
# We draw a heatmap 
Pmat <- final_Loading$Pmatrix
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

### 6. The T matrix or one can check summary(final_Loading) for T matrix
final_Loading$Tmatrix

####################################################################

###### Subsection: Regularized with known structure

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
plot(results_cvS)

results_cvS$LassoRegion  #to see the proper region 
summary(results_cvS)     #to see the recommended value

set.seed(115)
result_str <- structuredSCA(DATA = herring_data, Jk = num_var, R = 4,
                            Target = targetmatrix,
                            Position = c(1, 2, 3, 4), 
                            LASSO = 0.881476)

final_comLoadingS <- undoShrinkage(DATA = herring_data, R = 4, 
                                   Phat = result_str$Pmatrix)
summary(final_comLoadingS)

### again, we draw a heatmap
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


##############################################################
##### Empirical example
#load data
load("D:\\Dropbox\\Dropbox\\tilburg office\\Research SCA\\Project 2 software Simultaneous\\newdata\\family_data.RData")

library("psych")
library("RegularizedSCA")
describe(family_data[[1]])  #mother
describe(family_data[[2]])  #father
describe(family_data[[3]])  #child

data<- cbind(mySTD(family_data[[1]]), mySTD(family_data[[2]]), mySTD(family_data[[3]]))
num_var <- cbind(dim(family_data[[1]])[2], dim(family_data[[2]])[2], dim(family_data[[3]])[2])

#use VAF method
VAF(DATA = data, Jk = num_var, R = 10)

set.seed(111)
results_cv <- cv_sparseSCA(DATA = data, Jk = num_var, R = 5)
recommended_tuning <- summary(results_cv)
plot(results_cv)

set.seed(111)
final_results <- sparseSCA(data, num_var, R = 5, 
                           LASSO = recommended_tuning[1], 
                           GROUPLASSO = recommended_tuning[2], 
                           NRSTART = 20)
final_results$Pmatrix

final_Loading <- undoShrinkage(data, R = 5, 
                               final_results$Pmatrix)
final_Loading$Pmatrix

write.csv(final_Loading$Pmatrix, file = "D:\\Dropbox\\Dropbox\\tilburg office\\Research SCA\\Project 2 software Simultaneous\\newdata\\empiricalP.csv")
