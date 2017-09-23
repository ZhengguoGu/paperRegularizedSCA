# a function: replace with mean average
sim_impu <- function(DATA, M_values){
  nrow <- dim(DATA)[1]
  ncol <- dim(DATA)[2]
  DATA <- data.matrix(DATA)
  
  for(i in 1:length(M_values)){
    DATA[which(DATA == M_values[i])] <- NA
  }
  
  for(n in 1:nrow){
    for(m in 1:ncol){
      if(is.na(DATA[n, m])){
          DATA[n, m] <- mean(DATA[n, ], na.rm = T)
      }
    }
  }

 return(DATA)
}

#This is the script for obtaining the data 
DS3 = read.csv("D:/TilburgOffice/Dropbox/tilburg office/Research SCA/Project 2 software Simultaneous/newdata/04549-0003-Data.csv", sep = ";" )
DS4 = read.csv("D:/TilburgOffice/Dropbox/tilburg office/Research SCA/Project 2 software Simultaneous/newdata/04549-0004-Data.csv", sep = ";" )

DS3 = read.csv("D:/Dropbox/Tilburg office/Research SCA/Project 2 software Simultaneous/newdata/04549-0003-Data.csv", sep = ";" ) #PC at home (synced with dropbox)
DS4 = read.csv("D:/Dropbox/Tilburg office/Research SCA/Project 2 software Simultaneous/newdata/04549-0004-Data.csv", sep = ";" ) #PC at home (synced with dropbox)

DS3 <- data.matrix(DS3)
DS4 <- data.matrix(DS4)
# DS3 data, we are going to use the following quetionnaires DS3[, c(352:371, 379:404, 441:460, 540:553, 555:568)]
# some of the questions need to change direction(reverse coding)

# regarding "Please tell us how much you agree with each of the following statements about your relationship with your spouse/partner
summary(DS3[, c(352:367)])
DS3[, c(352:367)] <- sim_impu(DS3[, c(352:367)], 9)
DS3[, c(353, 356, 359, 360, 363, 365)] <- 6 - DS3[, c(353, 356, 359, 360, 363, 365)] #reverse coding
# regarding "These are various ways that couples deal with serious disagreements...
summary(DS3[, c(368:371)])
DS3[, c(368:371)] <- sim_impu(DS3[, c(368:371)], 9)
DS3[, c(370, 371)] <- 6 - DS3[, c(370, 371)] #so the higher the value the calmer.
# regarding "when you and one of your children have had a disagreement..."
summary(DS3[, c(550:553)])
DS3[, c(550:553)] <- sim_impu(DS3[, c(550:553)], 9)
DS3[, 550] <- 5 - DS3[, 550] #note, category 0 to 4
# regarding "Now we're going to list some statements that deal with ways that you may perceive yourself...
summary(DS3[, c(555:568)])
DS3[, c(555:568)] <- sim_impu(DS3[, c(555:568)], 9)
DS3[, c(556:562, 565, 566)] <- 5 -  DS3[, c(556:562, 565, 566)]

# for the remaining questionnaires, we moved the missing values
summary(DS3[,  c(379:404)])
DS3[, c(379:404)] <- sim_impu(DS3[, c(379:404)], c(9, -8))
summary(DS3[,  c(441:460)])
DS3[, c(441:460)] <- sim_impu(DS3[, c(441:460)], c(9, -8))
summary(DS3[,  c(540:549)])
DS3[, c(540:549)] <- sim_impu(DS3[, c(540:549)], 9)

DS3_NEW <- rowMeans(DS3[, c(352:367)])
DS3_NEW <- cbind(DS3_NEW, rowMeans(DS3[, c(368:371)]))
DS3_NEW <- cbind(DS3_NEW, rowMeans(DS3[, c(379:386)]))
DS3_NEW <- cbind(DS3_NEW, rowMeans(DS3[, c(387:404)]))
DS3_NEW <- cbind(DS3_NEW, rowMeans(DS3[, c(441:460)]))
DS3_NEW <- cbind(DS3_NEW, rowMeans(DS3[, c(540:543)]))
DS3_NEW <- cbind(DS3_NEW, rowMeans(DS3[, c(544:549)]))
DS3_NEW <- cbind(DS3_NEW, rowMeans(DS3[, c(550:553)]))
DS3_NEW <- cbind(DS3_NEW, rowMeans(DS3[, c(555:568)]))
DS3_NEW <- cbind(DS3[, 1:3],DS3_NEW)

summary(DS3_NEW)
colnames(DS3_NEW)[4:12] <- c("Relationship with partners", "Argue with partners", "Childs bright future",
                               "Activities with children", "Family hassles", "Feeling about parenting", 
                               "Communation with children", "Argue with children", "Confidence about oneself")


####################################
# now DS4
summary(DS4[, c(176:183)])
DS4[, c(176:183)] <- sim_impu(DS4[, c(176:183)], 9)
DS4[, 176] <- 3 - DS4[, 176]  #recoding, answer category 0~3

summary(DS4[, 304])
DS4[which(DS4[, 304] == 99), 304]<- NA #academic performance
DS4[which(DS4[, 304] == 9), 304] <- NA

summary(DS4[, c(326:342)])
DS4[, c(326:342)] <- sim_impu(DS4[, c(326:342)], 9)
summary(DS4[, c(359:367)])
DS4[, c(359:367)] <- sim_impu(DS4[, c(359:367)], 9)
summary(DS4[, c(381:397)])
DS4[, c(381:397)] <- sim_impu(DS4[, c(381:397)], 9)
DS4[, c(383,384, 386:388, 390:394, 396,397)] <- 4 - DS4[, c(383,384, 386:388, 390:394, 396,397)] #reverse coding
summary(DS4[, c(399:418)])
DS4[, c(399:418)] <- sim_impu(DS4[, c(399:418)], 9)
DS4[, c(399, 400, 402, 403, 405:411, 413, 414, 416:418)] <- 3 - DS4[, c(399, 400, 402, 403, 405:411, 413, 414, 416:418)]
summary(DS4[, c(468:480)])
DS4[, c(468:480)] <- sim_impu(DS4[, c(468:480)], 9)

DS4_NEW <- rowMeans(DS4[, 176:183]) #loneliness
DS4_NEW <- cbind(DS4_NEW, DS4[, 304]) #academic performance
DS4_NEW <- cbind(DS4_NEW, rowMeans(DS4[, 326:342])) # extracurricular
DS4_NEW <- cbind(DS4_NEW, rowMeans(DS4[, 359:367])) # Importance of friendship
DS4_NEW <- cbind(DS4_NEW, rowMeans(DS4[, 381:397])) # self image
DS4_NEW <- cbind(DS4_NEW, rowMeans(DS4[, 399:418])) # happiness
DS4_NEW <- cbind(DS4_NEW, rowMeans(DS4[, 468:480])) #confidence about future
DS4_NEW <- cbind(DS4[, 1:3], DS4_NEW)

colnames(DS4_NEW)[4:10] <- c("Loneliness", "Academic performance", "Extracurricular", "Importance of friendship",
                             "Self Image", "Happiness", "Confidence about the future")
summary(DS4_NEW)


# because "family hassels" in DS3_NEW contains too much NA values, better remove this column
DS3_NEW <- DS3_NEW[, -8]
# remove NAs
DS3_NEW_Final <- DS3_NEW[-sort(unique(which(is.na(DS3_NEW), arr.ind = T)[, 1])), ]
DS4_NEW_Final <- DS4_NEW[-sort(unique(which(is.na(DS4_NEW), arr.ind = T)[, 1])), ]
summary(DS3_NEW_Final)
summary(DS4_NEW_Final)

# devide DS3_NEW_Final into two dataset, one for mother, one for fathor
DS3_Mom <- DS3_NEW_Final[DS3_NEW_Final[, 2]==2, ]
DS3_Dad <- DS3_NEW_Final[DS3_NEW_Final[, 2]==1, ]
DS4_Kid <- DS4_NEW_Final

family_index <- intersect(intersect(DS3_Mom[, 3], DS3_Dad[, 3]), DS4_Kid[, 3])
DS3_Mom_Final <- DS3_Mom[DS3_Mom[, 3]==family_index[1], ]
for(i in 2:length(family_index)){
  DS3_Mom_Final <- rbind(DS3_Mom_Final, DS3_Mom[DS3_Mom[, 3]==family_index[i], ])
}
DS3_Dad_Final <- DS3_Dad[DS3_Dad[, 3]==family_index[1], ]
for(i in 2:length(family_index)){
  DS3_Dad_Final <- rbind(DS3_Dad_Final, DS3_Dad[DS3_Dad[, 3]==family_index[i], ])
}
DS4_Kid_Final <- DS4_Kid[DS4_Kid[, 3]==family_index[1], ]
for(i in 2:length(family_index)){
  DS4_Kid_Final <- rbind(DS4_Kid_Final, DS4_Kid[DS4_Kid[, 3]==family_index[i], ])
}

DS4_Kid_Final <- DS4_Kid_Final[!duplicated(DS4_Kid_Final[, 3]), ]  # a family can have more than 1 child. 

set.seed(111)
index <- sample(1:196, 10)  #create a high-dimensional dataset
Mom <- mySTD(DS3_Mom_Final[index, 4:11])
colnames(Mom) <- c("M: Relationship with partners", "M: Argue with partners", "M: Childs bright future",
                     "M: Activities with children", "M: Feeling about parenting", 
                     "M: Communation with children", "M: Argue with children", 
                     "M: Confidence about oneself")

Dad <- mySTD(DS3_Dad_Final[index, 4:11])
colnames(Dad) <- c("D: Relationship with partners", "D: Argue with partners", "D: Childs bright future",
                   "D: Activities with children", "D: Feeling about parenting", 
                   "D: Communation with children", "D: Argue with children", 
                   "D: Confidence about oneself")
Kid <- mySTD(DS4_Kid_Final[index, 4:10])
colnames(Kid) <- c("C: Loneliness", "C: Academic performance", "C: Extracurricular", "C: Importance of friendship",
                      "C: Self Image", "C: Happiness", "C: Confidence about the future")
family_data<- cbind(Mom, Dad, Kid)

num_var <- cbind(dim(Mom)[2], dim(Dad)[2], dim(Kid)[2])
VAF(DATA = family_data, Jk = num_var, R = 10)

set.seed(111)
results_cv <- cv_sparseSCA(DATA = family_data, Jk = num_var, R = 4)
results_cv$plot[[1]]
results_cv$plot[[2]]
results_cv$RecommendedLambda

set.seed(111)
final_results <- sparseSCA(family_data, num_var, R = 4, 
                           LASSO = 0.5, 
                           GROUPLASSO = 0.108774, 
                           NRSTART = 20)
final_results$Pmatrix

final_Loading <- undoShrinkage(family_data, R = 4, 
                               final_results$Pmatrix)
final_Loading$Pmatrix
