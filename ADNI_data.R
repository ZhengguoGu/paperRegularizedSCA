################## ADNI data  #################################

MyData <- data.frame(read.csv(file="D:\\Dropbox\\Tilburg office\\Research SCA\\Project 2 software Simultaneous\\newdata\\ADNI\\SelectedDataCombined.csv", 
                              header=F, sep=",", stringsAsFactors=FALSE)[, -746])   #note 746th columns contains nothing. the original data have 645 columns, but somehow it has an extra column once load to R.

Data_ADNIgo <- MyData[, MyData[1, ]=="ADNIGO"]

ADNIgo <- data.frame(Data_ADNIgo, stringsAsFactors=FALSE)


RosterID <- array()
for (i in 1:length(SubjectID)){
  RosterID[i] <- strsplit(as.character(SubjectID[i]), split = "_")[[1]][3]
}

ADNIgo_final <- rbind(ADNIgo, RosterID)
write.csv(ADNIgo_final, file = "D:\\Dropbox\\Tilburg office\\Research SCA\\Project 2 software Simultaneous\\newdata\\ADNI\\ADNIgo_final.csv")