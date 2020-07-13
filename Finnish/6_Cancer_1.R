## 6.1 This script is create ASCII list including id, sex and birth_year for index person and sibling


setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"


for (pop in c("index","sib")){
	ped <- get(load(paste0(r_dir, pop ,"_ped.Rdata")))
	bas <- ped[,c("id","sex","b_year")]
	write.table(bas, paste0(pop, "_basic.txt"), append=F, quote=F, sep=" ", row.names=F, col.names=T)
	print(paste0(pop," done."))
}



