## 12. Convert from ICD codes to endpoints using HILMO, CANCER, and DEATH registry


setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"

library(ggplot2, lib.loc="/homes/aliu/anaconda3/lib/R/library")
# library(tidyverse)
'%!in%' <- function(x,y)!('%in%'(x,y))



# set parameters
efiles <- c("thl2019_804_poisto_6986_COMPLETE", "thl2019_804_poisto_8793_COMPLETE", "thl2019_804_hilmo_9495_COMPLETE", "THL2019_804_hilmo_COMPLETE")
vars <- c("DG1,DG2,DG3,DG4", "PDG,SDG1,SDG2,SDG3", "PDG,SDG1,TUTAP", apply(as.matrix(c(paste0(rep("ICD10",56),rep(c("E_","O_"),each=28),1:28),paste0(rep("PITKADIAG",66),rep(c("E_","O_"),each=33),1:33))),2,paste, collapse=","))   # "TUTAP"="SDG2"
palas <- c(rep("inpatient",3),"in/outpatient")
icdvs <- c("8","9","9","10")
dates <- c("TULOPV",rep("TUPVA",3))
hos <- c("SAIR", rep("PALTU",3))
mus <- c("KNT", rep("KOKU",3))



print("Start for HILMO Registry")
#for (i in 1:length(efiles)){
for (i in 1:3){
# i <- 4
	print(paste0("Start for ", efiles[i],"."))
	d <- as.data.frame(get(load(paste0(r_dir, efiles[i], ".Rdata"))))
	v <- unlist(strsplit(vars[i],","))
		
	for (k in 1:length(v)){
		print(paste0("Diagnose ", k, "/", length(v), " start."))
		dat <- d[d[,v[k]]!="" & is.na(d[,v[k]])==F, c("TNRO", dates[i], hos[i], mus[i], v[k])]
		if (nrow(dat)>0){
			colnames(dat) <- c("ID", "EVENT_DATE", "hospital", "municipality", "ICD_CODE")
			dat[, "EVENT_DATE"] <- gsub("-", '', dat[ ,"EVENT_DATE"])
			dat[, "EVENT_FILE"] <- efiles[i]
			dat[, "EVENT_VAR"] <- v[k]
			dat[, "PALA"] <- palas[i]
			dat[, "ICD_VER"] <- icdvs[i]
			dat[, "morpho"] <- NA
		
			if (i==1 & k==1){
#			if (i==4 & k==1){
				HILMO_long <- dat
			} else{
				HILMO_long <- rbind(HILMO_long, dat)
			}
		}
		print(paste0("Diagnose ", k, "/", length(v), " done, including ", nrow(dat), " records."))
	}
	
	print(paste0("Done for ", efiles[i],"."))	
}



HILMO_long <- as.data.frame(HILMO_long)
save(HILMO_long, file=paste0(r_dir, "HILMO_COMPLETE_long.Rdata"))
print(paste0("Done for HILMO Registry, with ", nrow(HILMO_long)," rows in total."))   


