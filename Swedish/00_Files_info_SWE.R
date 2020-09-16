## 0. This script is to check files (number of files and number of rows/columns of each file) of release 1, 2, 3, and 4, and convert all files to Rdata format.
#  (only copy files from release 3 & 4 to "/home/aoxing/DSGE_LRS/input/r_files/", since release 1 & 2 are incomplete and have been re-extracted in release 3 & 4 )

setwd("/home/aoxing/DSGE_LRS/out/registry_edit/")
in_dir <- "/home/aoxing/DSGE_LRS/input/"
r_dir <- paste0(in_dir, "r_files/")

library(haven)




#############################
#  Upload or download files #
#############################

## upload files to bianca
#  scp lrs_r3.zip  aoxing@rackham.uppmax.uu.se:/home/aoxing/lrs/input            # from mac terminal copy file from local to rackham
#  ssh -A aoxing@rackham.uppmax.uu.se                                            # login rackham
#  sftp -q aoxing-sens2019018@bianca-sftp.uppmax.uu.se:aoxing-sens2019018        # login the sftp prompt of bianca
#  put lrs_r3.zip                                                                # upload in the sftp prompt
#  cp /proj/sens2019018/nobackup/wharf/aoxing/aoxing-sens2019018/lrs_r3.zip   /proj/sens2019018/SCB_data/release3   # from bianca terminal copy files to the target directory



## download files from bianca
#  cp /proj/sens2019018/socialstyrelsen/Variabellista_27035_2018.xlsx  /proj/sens2019018/nobackup/wharf/aoxing/aoxing-sens2019018   # from bianca termianl copy files from bianca to wharf, since only wharf has internet connection due to the security reason
#  ssh -A aoxing@rackham.uppmax.uu.se                                            # login rackham in another window
#  sftp -q aoxing-sens2019018@bianca-sftp.uppmax.uu.se:aoxing-sens2019018        # login the sftp prompt of bianca
#  get Variabellista_27035_2018.xlsx                                             # download in the sftp prompt
#  scp aoxing@rackham.uppmax.uu.se:/domus/h1/aoxing/Variabellista_27035_2018.xlsx  /Users/aoxliu/Documents/LRS/Data/SWE/release4 



## Uncompress files in bianca
#  cd /proj/sens2019018/SCB_data/release3
#  jar xvf  lrs_r3.zip                                # Tove uploaded this file to the cluster 
#  module load p7zip
#  7z e  Tove_Lev_LISA_1977_2017.exe && 7z e  Tove_Lev_Ovriga_tabeller.exe && 7z e  Tove_Lev_RTB_1977_2017.exe                    
#  cp /proj/sens2019018/SCB_data_lev2/Tove_Lev_Index_Part__Sysk_Part_1977_1997.exe  . &&  7z e  Tove_Lev_Index_Part__Sysk_Part_1977_1997.exe   # password: tNCDVZ8M39Ar, this data was not provided previously due to the mistake of SCB 

#  cd /proj/sens2019018/SCB_data/release4
#  cp /proj/sens2019018/socialstyrelsen/Utdata.7z   /proj/sens2019018/SCB_data/release4
#  7z e  Utdata.7z   # password: }*/L:F}pBDU~




#############################
#   Comment on each files   #
#############################

## 0.1 list all datasets for population and SCE registry (from release 1, 2, 3) and health/death/MBR registry (release 4) 

## 0.1.1 release 1
system("ls /proj/sens2019018/SCB_data/release1/r_files/")


#-----------------------------------------------------------------------------------------
## 0.1.2 release 2
system("ls /proj/sens2019018/SCB_data/release2/r_files/")


#-----------------------------------------------------------------------------------------
## 0.1.3 release 3 
system("ls /proj/sens2019018/SCB_data/release3")


#-----------------------------------------------------------------------------------------
## 0.1.4 release 4
system("ls /proj/sens2019018/SCB_data/release4")




##########################################
#  Convert and check for release 3 & 4   #  
##########################################

## 0.2.1 transfer all files of release 3/4 from sas7bdat to Rdata (release 1 & 2 have been transfered by Andrea)


for (release_n in 3:4){
	in_dir <- paste0("/proj/sens2019018/SCB_data/release",release_n)
	wfile <- list.files(in_dir, pattern="*.sas7bdat")

	for (k in wfile){
		d <- read_sas(paste0(in_dir,"/",k), encoding = "UTF-8")
		save(d, file=paste0(r_dir, gsub(".sas7bdat",".Rdata",k)))
		print(k)
	}
}



## 0.2.2 check whether the updated release 3 solving the problems of release 2

# check whether there are siblings without children (full list of sibling) (yes)
sib <- get(load(paste0(r_dir, "tove_lev_koppl_index_syskon.Rdata")))   
nrow(sib)                            # 6,023,984

sibchild <- get(load(paste0(r_dir, "tove_lev_koppl_index_sysbarn.Rdata")))    
length(unique(sibchild$LopNr))      # 2,356,638


# check whether there are indexperson without children (full list of indexperson) (yes)
index <- get(load(paste0(r_dir, "tove_lev_index.Rdata")))     


# parents of sibling's children, parents of indexperson (yes)
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




    

## download files from bianca
#  cp  /home/aoxing/lrs/output/SWE_REGISTRY_INFO_r4.csv  /proj/sens2019018/nobackup/wharf/aoxing/aoxing-sens2019018   # from bianca termianl copy files from bianca to wharf, since only wharf has internet connection due to the security reason
#  ssh -A aoxing@rackham.uppmax.uu.se                                            # login rackham in another window
#  sftp -q aoxing-sens2019018@bianca-sftp.uppmax.uu.se:aoxing-sens2019018        # login the sftp prompt of bianca, need password of bianca
#  get SWE_REGISTRY_INFO_r4.csv                                                  # download in the sftp prompt
#  scp aoxing@rackham.uppmax.uu.se:/domus/h1/aoxing/SWE_REGISTRY_INFO_r4.csv  /Users/aoxliu/Documents/LRS/Data/SWE/release4 






