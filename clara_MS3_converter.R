MS3data <- read.table("MS3_L6_edit.txt", sep="\t", header=TRUE)
number_of_columns <- ncol(MS3data)
column_names <- vector()

for (i in 2:number_of_columns) {
  column_names[i] <- colnames(MS3data)[i]
  column_names_splitted <- strsplit(column_names,split='.', fixed=TRUE)
}

column_name_previous <- 1
data_set_start_column <- vector()

for (i in 2:number_of_columns) {
  if (as.numeric(tail(column_names_splitted[[i]], n=1)) > column_name_previous) {
    column_name_previous <- as.numeric(tail(column_names_splitted[[i]], n=1))
  } else {
    data_set_start_column <- c(data_set_start_column, i)
    column_name_previous <- as.numeric(tail(column_names_splitted[[i]], n=1))
  }
}

# adding and extra 'startcolumn' to set a upper limit for the last subset
data_set_start_column <- c(data_set_start_column, ncol(MS3data)+1) 

newMS3data <- MS3data[1]

for (i in 1:(length(data_set_start_column)-1)) {
  subset1 <- MS3data[,c(as.numeric(data_set_start_column[i]):as.numeric(data_set_start_column[i+1]-1))]
  subset1["average"] <- rowMeans(subset1[,])
  subset1["sd"] <- apply(subset1[,1:ncol(subset1)-1],1,sd)
  newMS3data <- cbind(newMS3data, subset1)
}
  
write.table(newMS3data,"MS3_new.txt",sep="\t",row.names=FALSE)