## This script is to check/combine yearly spouse registry from 1977 to 2017


# Input: "tove_lev_koppl_sys_part_{1977..2017}.Rdata", "tove_lev_koppl_sys_part_{1977..2017}.Rdata"
# Output: "index_spouse_1977_2017.RData", "sib_spouse_1977_2017.RData", Spouse_1977_2017.RData
# Comments: 


setwd("/home/aoxing/DSGE_LRS/out/registry_edit/")
r_dir <- "/home/aoxing/DSGE_LRS/input/r_files/"

library(tidyverse)


############################################
#   Spouses of indexperson or siblings     #
############################################

# Spouses of indexperson
index_spouse <- NULL
for (i in 1977:2017){
	print(i)
	# d <- read_sas(paste0("/proj/sens2019018/SCB_data/release3/tove_lev_koppl_index_part_",i,".sas7bdat"), encoding = "UTF-8")
	# save(d, file=paste0(r_dir,"tove_lev_koppl_index_part_",i,".Rdata"))
	d <- data.frame(get(load(paste0(r_dir, "tove_lev_koppl_index_part_",i,".Rdata"))))
	d[,"spouse_year"] <- i
	d[d==""] <- NA
	index_spouse <- rbind(index_spouse, d)
	print(paste0(nrow(d),"/",nrow(index_spouse)))
}
nrow(index_spouse)    # 37,767,025 
save(index_spouse, file=paste0(r_dir,"index_spouse_1977_2017.Rdata"))  



# Spouses of index person's siblings (data is too large, divided into two periods)
sib_spouse <- NULL
for (i in 1977:2017){
	print(i)
	# d <- read_sas(paste0("/proj/sens2019018/SCB_data/release3/tove_lev_koppl_sys_part_",i,".sas7bdat"), encoding = "UTF-8")
	# save(d, file=paste0(r_dir,"tove_lev_koppl_sys_part_",i,".Rdata"))
	d <- data.frame(get(load(paste0(r_dir, "tove_lev_koppl_sys_part_",i,".Rdata"))))
	d[,"spouse_year"] <- i
	d[d==""] <- NA
	sib_spouse <- rbind(sib_spouse, d)
	print(paste0(nrow(d),"/",nrow(sib_spouse)))
}
nrow(sib_spouse)    # 45,264,922
save(sib_spouse, file=paste0(r_dir,"sib_spouse_1977_2017.Rdata"))  



############################################
#                Combine                   #
############################################

colnames(sib_spouse) <- colnames(index_spouse)
spouse <- union(index_spouse, sib_spouse)
nrow(spouse)  # 51,744,405
save(spouse, file=paste0(r_dir,"Spouse_1977_2017.RData"))  





