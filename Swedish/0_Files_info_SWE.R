## 0. This script is to check files (number of files and number of rows/columns of each file) of release 1, 2, 3, and 4, and convert all files to Rdata format.
#  ( only copy files from release 3 & 4 to "/home/aoxing/DSGE_LRS/input/r_files/")

#  ## Ask interactive nodes 
#  interactive -A sens2019018 -n 3 --qos=interact -t 12:00:00
#  salloc -A sens2019018 -p core -n 3 -t 12:00:00
#  jobinfo -u aoxing

# module load R_packages/3.6.0


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

#set 1: tove_lev_lisa_1990.Rdata                   # more individuals/details in release 2, 1990 to 2017
#set 2: tove_lev_rtb_1977.Rdata                    # more individuals/details in release 2, 1977 to 2017
#set 3: tove_lev_dispink_1977.Rdata                # more individuals/details in release 2, 1977 to 1989
#set 4: tove_lev_hog_gymn_1989_1977.Rdata          # more individuals/details in release 2
#set 5: tove_lev_doddatum.Rdata                    # more individuals/details in release 2
#set 6: tove_lev_migrationer.Rdata                 # more individuals/details in release 2
#set 7: tove_lev_koppl_index_barn.Rdata            # more individuals/details in release 2
#set 8: tove_lev_koppl_index_barnbarn.Rdata        # more individuals/details in release 2
#set 9: tove_lev_koppl_index_syskonbarn.Rdata      # more individuals/details in release 2
#set 10: tove_lev_bakgrund.Rdata                   # missing in release 2, background of indexperson 
#set 11: tove_lev_koppl_barn_vardh_1998.Rdata      # missing in release 2, 1999 to 2017 ???what's this file for
#set 12: tove_lev_koppl_index_vhfor_1998.Rdata     # missing in release 2, 1998 to 2017 ???what's this file for
#set 13: tove_lev_koppl_index_foraldrar.Rdata      # missing in release 2, parents of the indexperson
#set 14: tove_lev_koppl_barn_foraldrar.Rdata       # missing in release 2, parents of the child



#-----------------------------------------------------------------------------------------
## 0.1.2 release 2
system("ls /proj/sens2019018/SCB_data/release2/r_files/")

#set 1: tove2_lev_dink_parsyssyspar1977.Rdata      # 1977 to 1989, information [2][id, disposable_income (individual)] of spouse (indexperson), siblings, and siblings' spouse
#set 2: tove2_lev_hoggym_pssp_1989_1977.Rdata      # Information [5][id, year, highest_education_level (SUN2000), highest_educational_orientation, highest_education_level(aggregated to 7 'Swedish' level)] of spouse (indexperson), siblings, and siblings' spouse
#set 3: tove2_lev_lisa_1990.Rdata                  # 1900 to 2017, information [11][id, socioeconomic, highest_education_level(old SUN), highest_education_level(aggregated to 7 'Swedish' level), highest_education_level(SUN2000), highest_educational_orientation, employment_status(old definition), house_bill, disposable_income(individual from family), disposable_income(individual), disposable_income(individual from family)] of spouse (indexperson), siblings, and siblings' spouse
#set 4: tove2_lev_rtb_partsyssyspar1977.Rdata      # 1977 to 2017, information [5][id, residence_country, residence_municipality, house_group, civil_status] of spouse (indexperson), siblings, and siblings' spouse (Register of total population (RTB))

#set 5: tove2_lev_indexpartn1977.Rdata             # 1977 to 2017, information [6][id, id_spouse, spouse_birth_year, spouse_birth_country, spouse_birth_municipality, spouse_gender] of spouse (indexperson)
#set 6: tove2_lev_syskonpartn1977.Rdata            # 1977 to 2017, information [6][id_sibling, id_siblingspouse, siblingspouse_birth_year, siblingspouse_birth_country, siblingspouse_birth_municipality, siblingspouse_gender] of siblings' spouse

#set 7: tove2_lev_doddatum.Rdata                   # Information [2][id, death] of everyone in the population
#set 8: tove2_lev_migr_partsyssyspart.Rdata        # Information [4][id, date, country, type(immigration_emigration)] of spouse (indexperson), siblings, and siblings' spouse ????how about the index person???

#set 9: tove2_lev_koppl_index_barn.Rdata           # Links and information [7][id, id_child, child_birth_year, child_birth_country, child_birth_municipality, child_birth_continent, child_gender] of child (indexperson)
#set 10: tove2_lev_koppl_index_barnbarn.Rdata      # Information [6][id, id_grandchild, grandchild_birth_year, grandchild_birth_country, grandchild_birth_municipality, grand_child_gender]of indexperson's grandchild
#set 11: tove2_lev_koppl_index_syskon.Rdata        # Links and information [7][id, id_sibling, type_of_sibling, sibling_birth_year, sibling_birth_country, sibling_birth_municipality, sibling_gender] of sibling
#set 12: tove2_lev_koppl_ind_barn_bb.Rdata         # Links of [3]indexperson-child-grandchild
#set 13: tove2_lev_koppl_ind_sys_sysbarn.Rdata     # Links of [3]indexperson-sibling-siblingchild
#set 14: tove2_lev_koppl_syskon_syskbarn.Rdata     # Links and information [7][id, id_sibling, id_siblingchild, siblingchild_birth_year, siblingchild_birth_country, siblingchild_birth_municipality, siblingchild_gender] of sibling and siblingchild



#-----------------------------------------------------------------------------------------
## 0.1.3 release 3 
system("ls /proj/sens2019018/SCB_data/release3")

# social economic 
#set 1: tove_lev_rtb_1977-2017.sas7bdat
#set 2: tove_lev_dispink_1977-1989.sas7bdat
#set 3: tove_lev_hog_gymn_1989_1977.sas7bdat
#set 4: tove_lev_lisa_1990-2017.sas7bdat

#set 5: tove_lev_koppl_barn_vardh_1998-2017.sas7bdat
#set 6: tove_lev_koppl_index_vhfor_1998-2017.sas7bdat

# spouse of index and sibling
#set 7: tove_lev_koppl_index_part_1998-2017.sas7bdat
#set 8: tove_lev_koppl_sys_part_1998-2017.sas7bdat

# family relationship (indexperson, child, grandchild, sibling, sibling's child)
#set 9: tove_lev_index.sas7bdat
#set 10: tove_lev_koppl_index_barn.sas7bdat
#set 11: tove_lev_koppl_index_barnbarn.sas7bdat
#set 12: tove_lev_koppl_index_syskon.sas7bdat
#set 13: tove_lev_koppl_index_sysbarn.sas7bdat

# pedigree (parents) information for index, child, and siblings's child
#set 14: tove_lev_koppl_index_foraldrar.sas7bdat
#set 15: tove_lev_koppl_barn_foraldrar.sas7bdat
#set 16: tove_lev_koppl_sysbarn_foraldr.sas7bdat

# migration and death
#set 17: tove_lev_migrationer.sas7bdat
#set 18: tove_lev_doddatum.sas7bdat



#-----------------------------------------------------------------------------------------
## 0.1.4 release 4
system("ls /proj/sens2019018/SCB_data/release4")

#set 1: ut_par_ov_27035_2018.sas7bdat            # Outpatient Registry 1997-2018
#set 2: ut_par_sv_27035_2018.sas7bdat            # Inpatient Registry 1969-2018

#set 3: ut_dors_indexpers_27035_2018.sas7bdat    # Death Registry for Index persons 1956-2019
#set 4: ut_dors_b_bbarn_27035_2018.sas7bdat      # Death Registry for children and grandchildren 1956-2019

#set 5: ut_mfr_indexpers_27035_2018.sas7bdat     # Medical Birth Registry 1973-2018 for index person
#set 6: ut_mfr_b_bbarn_27035_2018.sas7bdat       # Medical Birth Registry 1973-2018 for Children and grandchildren






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
		
		if (which(wfile==k)==1){
			write.table(info, paste0("Data_comments_SWE_r",release_n,".csv"), append=F, quote=F, sep=" ", row.names=F, col.names=T)
		}else{
			write.table(info, paste0("Data_comments_SWE_r",release_n,".csv"), append=T, quote=F, sep=" ", row.names=F, col.names=F)
		}
		
		print(paste0("Done for file: ", k))
	}
}




    

## download files from bianca
#  cp  /home/aoxing/lrs/output/SWE_REGISTRY_INFO_r4.csv  /proj/sens2019018/nobackup/wharf/aoxing/aoxing-sens2019018   # from bianca termianl copy files from bianca to wharf, since only wharf has internet connection due to the security reason
#  ssh -A aoxing@rackham.uppmax.uu.se                                            # login rackham in another window
#  sftp -q aoxing-sens2019018@bianca-sftp.uppmax.uu.se:aoxing-sens2019018        # login the sftp prompt of bianca, need password of bianca
#  get SWE_REGISTRY_INFO_r4.csv                                                  # download in the sftp prompt
#  scp aoxing@rackham.uppmax.uu.se:/domus/h1/aoxing/SWE_REGISTRY_INFO_r4.csv  /Users/aoxliu/Documents/LRS/Data/SWE/release4 






