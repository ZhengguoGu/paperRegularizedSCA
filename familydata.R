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
# DS3 data, we are going to use the following quetionnaires DS3[, c(352:371, 379:404, 441:460, 540:553, 555:568)]
# some of the questions need to change direction(reverse coding)

# regarding "Please tell us how much you agree with each of the following statements about your relationship with your spouse/partner
DS3[, c(352:367)] <- sim_impu(DS3[, c(352:367)], 9)
DS3[, c(353, 356, 359, 360, 363, 365)] <- 6 - DS3[, c(353, 356, 359, 360, 363, 365)] #reverse coding
# regarding "These are various ways that couples deal with serious disagreements...
DS3[, c(368:371)] <- sim_impu(DS3[, c(368:371)], 9)
DS3[, c(370, 371)] <- 6 - DS3[, c(370, 371)] #so the higher the value the calmer.
# regarding "when you and one of your children have had a disagreement..."
DS3[, c(550:553)] <- sim_impu(DS3[, c(550:553)], 9)
DS3[, 550] <- 5 - DS3[, 550] #note, category 0 to 4
# regarding "Now we're going to list some statements that deal with ways that you may perceive yourself...
DS3[, c(555:568)] <- sim_impu(DS3[, c(555:568)], 9)
DS3[, c(556:562, 565, 566)] <- 5 -  DS3[, c(556:562, 565, 566)]

# for the remaining questionnaires, we moved the missing values
DS3[, c(379:404)] <- sim_impu(DS3[, c(379:404)], 9)
DS3[, c(441:460)] <- sim_impu(DS3[, c(441:460)], c(9, -8))
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
DS4[, c(176:183)] <- sim_impu(DS4[, c(176:183)], 9)
DS4[, 176] <- 3 - DS4[, 176]  #recoding, answer category 0~3

DS4[which(DS4[, 304] == 99)] <- NA #academic performance
DS4[which(DS4[, 304] == 9)] <- NA

DS4[, c(326:342)] <- sim_impu(DS4[, c(326:342)], 9)
DS4[, c(359:367)] <- sim_impu(DS4[, c(359:367)], 9)
DS4[, c(381:397)] <- sim_impu(DS4[, c(381:397)], 9)
DS4[, c(383,384, 386:388, 390:394, 396,397)] <- 4 - DS4[, c(383,384, 386:388, 390:394, 396,397)] #reverse coding
DS4[, c(399:418)] <- sim_impu(DS4[, c(399:418)], 9)
DS4[, c(399, 400, 402, 403, 405:411, 413, 414, 416:418)] <- 3 - DS4[, c(399, 400, 402, 403, 405:411, 413, 414, 416:418)] 
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
DS3_NEW <- data.matrix(DS3_NEW)
DS3_NEW <- DS3_NEW[-which(is.nan(DS3_NEW))]
which(is.nan(DS3_NEW[1,]))
str(data.matrix(DS3_NEW))
