## Script to prepare summary info of a file, outputs will be added to the dictionary files

# Dictinary of Finnish: https://docs.google.com/spreadsheets/d/16E40mY9Avffnq6u5DhtQbisohG3ctzmU/edit#gid=134379014
# Dictinary of Swedish: https://docs.google.com/spreadsheets/d/1pRiNXYGEpp7NnwLCX_bvzu3mvdEyjh0b/edit#gid=1893630016


file_info <- function (d=file_want_to_check, k="file_name"){
  info <- matrix(NA, nrow=ncol(d), ncol=14)
  colnames(info) <- c("FileNumber","File","FileDescription","NoOfRows","NoOfColumns","ColumnNumber","ColumnName","ColumnNameOriginal","ColumnDescription","NumberOfLevels","Min","Max","DataType","FileType")	
  print(paste0("Start for file: ", k))
		
  for (i in 1:ncol(d)){
    print(paste("Column", i ,"of", ncol(d), "columns",sep=" "))
    info[i, c("FileNumber","File")] <- c(which(wfile==k), k)		
    info[i, c("NoOfRows","NoOfColumns")] <- c(nrow(d),ncol(d))		
    info[i, c("ColumnNumber", "ColumnName")] <- c(i,colnames(d)[i])		
    info[i, "NumberOfLevels"] <- length(unique(d[,i]))
    #info[i, c("Min","Max")] <- c(min(d[d[,i]!="",i]), max(d[d[,i]!="",i]))
			
    if (is.numeric(d[,i])==T){
      info[i, c("Min","Max")] <- c(min(d[d[,i]!="",i]), max(d[d[,i]!="",i]))
    }
			
    info[i, "DataType"] <- ifelse(as.numeric(info[i, "NumberOfLevels"])<20, paste(unique(d[, i]),collapse = ","), paste(head(unique(d[, i]),5),collapse = ","))		
    }
}

