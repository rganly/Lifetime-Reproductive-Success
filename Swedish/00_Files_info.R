## This script is to convert all registry data to .Rdata format and comment on each file

# Input: all original files from release 1, 2, 3 & 4 
# Output: "Data_comments_FIN.csv"
# Comments: See "Data_comments_FIN.xlsx", which was adapted from "Data_comments_FIN.csv"


setwd("/home/aoxing/DSGE_LRS/out/registry_edit/")
in_dir <- "/home/aoxing/DSGE_LRS/input/"
r_dir <- paste0(in_dir, "r_files/")

library(haven)



#############################
#   Comment on each files   #
#############################
## release 1 & 2 are incomplete and have been re-extracted in release 3 & 4
## release 1
system("ls /proj/sens2019018/SCB_data/release1/r_files/")

## release 2
system("ls /proj/sens2019018/SCB_data/release2/r_files/")

## release 3 
system("ls /proj/sens2019018/SCB_data/release3")

## release 4
system("ls /proj/sens2019018/SCB_data/release4")




##########################################
#  Convert and check for release 3 & 4   #  
##########################################

## transfer all files of release 3/4 from sas7bdat to Rdata (release 1 & 2 have been transfered by Andrea)
for (release_n in 3:4){
	in_dir <- paste0("/proj/sens2019018/SCB_data/release",release_n)
	wfile <- list.files(in_dir, pattern="*.sas7bdat")

	for (k in wfile){
		d <- read_sas(paste0(in_dir,"/",k), encoding = "UTF-8")
		save(d, file=paste0(r_dir, gsub(".sas7bdat",".Rdata",k)))
		print(k)
	}
}



## check whether the updated release 3 solving the problems of release 2
# check whether there are siblings without children (full list of sibling) (yes)
sib <- get(load(paste0(r_dir, "tove_lev_koppl_index_syskon.Rdata")))   
nrow(sib)                           # 6,023,984

sibchild <- get(load(paste0(r_dir, "tove_lev_koppl_index_sysbarn.Rdata")))    
length(unique(sibchild$LopNr))      # 2,356,638


# check whether there are indexperson without children (full list of indexperson) (yes)
index <- get(load(paste0(r_dir, "tove_lev_index.Rdata")))     


# Did we get parents of sibling's children, parents of indexperson (yes)
index_parent <- get(load(paste0(r_dir, "tove_lev_koppl_sysbarn_foraldr.Rdata")))     
child_parent <- get(load(paste0(r_dir, "tove_lev_koppl_barn_foraldrar.Rdata"))) 
sibchild_parent <- get(load(paste0(r_dir, "tove_lev_koppl_sysbarn_foraldr.Rdata"))) 




###########################
##  Comment on each file  #  See "Data_comments_SWE.xlsx", which was adapted from "Data_comments_SWE.csv".
###########################

for (release_n in 3:4){
	r_dir <- paste0("/proj/sens2019018/SCB_data/release",release_n,"/r_files/")
	wfile <- list.files(r_dir, pattern="*.Rdata")

	for (k in wfile){
		d <- get(load(paste0(r_dir, k)))	
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
		
		rm(d)  # save memory
		write.table(info, paste0("Data_comments_SWE_r",release_n,".csv"), append=T, quote=F, sep=" ", row.names=F, col.names=F)		
		print(paste0("Done for file: ", k))
	}
}





