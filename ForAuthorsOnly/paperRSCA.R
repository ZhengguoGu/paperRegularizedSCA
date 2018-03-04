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
#  NOTE: This R script was run on R version 3.4.2 on windows
#        Latest update: TO BE ADDED
#############################################################

### 0. install package
install.packages("RegularizedSCA")
install.packages(pkgs="D://RegularizedSCA_0.5.2.tar.gz", repos = NULL)
install.packages(pkgs="C://Users//Zhengguo//Documents//RegularizedSCA_0.5.3.tar.gz", repos = NULL)  #pc at home
##############################################################
##### Empirical example
#load data
load("D:\\Dropbox\\Tilburg office\\Research SCA\\Project 2 software Simultaneous\\newdata\\family_data.RData") #pc at home

library(psych)
library(RegularizedSCA)
describe(family_data[[1]])  #mother
describe(family_data[[2]])  #father
describe(family_data[[3]])  #child

data<- cbind(mySTD(family_data[[1]]), mySTD(family_data[[2]]), mySTD(family_data[[3]]))
num_var <- cbind(dim(family_data[[1]])[2], dim(family_data[[2]])[2], dim(family_data[[3]])[2])

#use VAF method
summary(VAF(DATA = data, Jk = num_var, R = 10)) # note: here we choose 5 components because on the 5th component, the variance 
                                                # of the second block is rather large (.104), which is even larger than all the 
                                                # variances in the 4th component. On the other hand, the 4th component cannot 
                                                # be droped because the total variance of the 4th component is larger than 
                                                # the 5th (the total variances of the components are from highest to lowest).
                                                # Of course, the number of R in this case is chosen rather subjectively. 

set.seed(111)
results_cv <- cv_sparseSCA(DATA = data, Jk = num_var, R = 5)
summary(results_cv)  # the recommended tuning parameters. call summary(results_cv, disp = "estimatedPT") to see the estimated P and T matrix 
plot(results_cv)

set.seed(111)
final_results <- sparseSCA(data, num_var, R = 5, 
                           LASSO = 2.82068, 
                           GROUPLASSO = 1.28369, 
                           NRSTART = 20)
final_results$Pmatrix

final_Loading <- undoShrinkage(data, R = 5, 
                               final_results$Pmatrix)
final_Loading$Pmatrix

write.csv(final_Loading$Pmatrix, file = "D:\\Dropbox\\Tilburg office\\Research SCA\\Project 2 software Simultaneous\\newdata\\empiricalP.csv")




############### SECTION: the RegularizedSCA package ##########################
####### subsection: Exploring the functionalities of RegularizedSCA
### 1. load the package and data
library(RegularizedSCA)
names(Herring)  #one can check the names of the data file "Herring" 

### 2. pre-process the data
ChemPhy <- pre_process(Herring$Herring_ChemPhy)
Sensory <- pre_process(Herring$Herring_Sensory)
herring_data <- cbind(ChemPhy, Sensory)
num_var <- cbind(dim(ChemPhy)[2], dim(Sensory)[2])


#### subsubsection: Model 2
### 3. VAF, PCA-GCA, and DISCO-SCA
vaf <- VAF(DATA = herring_data, Jk = num_var, R = 10)
summary(vaf)

discoresult <- DISCOsca(DATA = herring_data, R = 4, Jk = num_var)
summary(discoresult)

targetmatrix <- matrix(c(1, 1, 1, 1, 1, 0, 0, 1), nrow = 2, ncol = 4)
targetmatrix	

maxLasso <- maxLGlasso(DATA = herring_data, num_var, R = 4)$Lasso
set.seed(115)
results_cvS <- cv_structuredSCA(DATA = herring_data, Jk = num_var, R = 4, 
                                Target = targetmatrix,
                                Position = c(1, 2, 3, 4),
                                LassoSequence = seq(from = 0.0000001, 
                                                    to = maxLasso, 
                                                    length.out = 200))
plot(results_cvS)


results_cvS$LassoRegion  #to see the proper region 

set.seed(115)
result_str <- structuredSCA(DATA = herring_data, Jk = num_var, R = 4,
                            Target = targetmatrix,
                            Position = c(1, 2, 3, 4), 
                            LASSO = 0.8922256) #here (0.8814759 + 0.9029753)/2 = 0.8922256

final_comLoadingS <- undoShrinkage(DATA = herring_data, R = 4, 
                                   Phat = result_str$Pmatrix)


summary(final_comLoadingS)

### Now we draw a heatmap
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


#### subsubsection: Model 4
set.seed(115)
results_cv <- cv_sparseSCA(DATA = herring_data, Jk = num_var, R = 4)

summary(results_cv)  #to check the recommended tuning parameter values
summary(results_cv, disp = "full")
#plot(results_cv) #note that this plot is a heat plot for mean squared prediction errors.

# the final model 4
final_results <- sparseSCA(herring_data, num_var, R = 4, 
                           LASSO = 1.503148, 
                           GROUPLASSO = 0.3655355, 
                           NRSTART = 20)

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



#### subsubsection: Model 5


pca_gca(DATA = herring_data, Jk = num_var, cor_min = .85) 
       #note: pca_gca() contains a user-computer interaction phase, once
       #we run pca_gca(DATA = herring_data, Jk = num_var), the console will 
       #display the eigenvalues of block 1 and ask whether the user wants to 
       #see the scree plot. If yes, then the user must enter 1; if No, enter 0.
       #Then the program will also ask how many components to retain for this block
       #and the user must give a number. 
       #The aforementioned procedure will be repeated from the first block till the 
       #last block.


targetmatrix <- matrix(c(1, 1, 1, 1, 1, 0, 0, 1), nrow = 2, ncol = 4)
targetmatrix	

set.seed(115)
result_strModel5 <- structuredSCA(DATA = herring_data, Jk = num_var, R = 4,
                            Target = targetmatrix,
                            LASSO = 0)

final_LoadingModel5 <- undoShrinkage(herring_data, R = 4, 
                                  Phat = result_strModel5$Pmatrix)
summary(final_LoadingModel5)





