## Convert from ICD codes to endpoints using HILMO, CANCER, and DEATH registry

# Input: 
# Output: 
# Comments: 



################################
#    Set working environment   #
################################

setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"

library(ggplot2)
library(dplyr)
library(tidyverse)


# Parameters
efiles <- c("thl2019_804_poisto_6986_COMPLETE", "thl2019_804_poisto_8793_COMPLETE", "thl2019_804_hilmo_9495_COMPLETE", "THL2019_804_hilmo_COMPLETE")
hilmo_v <- c("DG1,DG2,DG3,DG4", "PDG,SDG1,SDG2,SDG3", "PDG,SDG1,TUTAP", apply(as.matrix(c(paste0(rep("ICD10",56),rep(c("E_","O_"),each=28),1:28),paste0(rep("PITKADIAG",66),rep(c("E_","O_"),each=33),1:33))),2,paste, collapse=","))   # "TUTAP"="SDG2"
fcr_v <- c("icd10_topo")

palas <- c(rep("inpatient",3),"in/outpatient")
icdvs <- c("8","9","9","10")
dates <- c("TULOPV",rep("TUPVA",3))
hos <- c("SAIR", rep("PALTU",3))
mus <- c("KNT", rep("KOKU",3))


# Functions
fmt <- function(x, f="123") {            # Format columns as character(abc) or numeric (123)
    if(f=="123") {
    	x <- as.numeric(as.character(x))
    } else if (f=="abc"){
    	x <- as.character(x)
    }
    return(x)
}

'%!in%' <- function(x,y)!('%in%'(x,y))     # not in




################################################
#      Extract Longtitudinal HILMO (health)    #
################################################

HILMO_long <- NULL  
print("Start for HILMO Registry")

for (i in 1:length(efiles)){
    print(paste0("Start for ", efiles[i],".")) 
    d <- as.data.frame(get(load(paste0(r_dir, efiles[i], ".Rdata"))))
    v <- unlist(strsplit(hilmo_v[i],","))
		
    for (k in 1:length(v)){
        print(paste0("Diagnose ", k, "/", length(v), " start."))    
        dat <- d[d[,v[k]]!="" & is.na(d[,v[k]])==F, c("TNRO", dates[i], hos[i], mus[i], v[k])]     
        if (nrow(dat)>0){
            colnames(dat) <- c("ID", "EVENT_DATE", "hospital", "municipality", "ICD_CODE")
			
            if (efiles[i]=="THL2019_804_hilmo_COMPLETE"){    # format the diagnose date
                dat[, "EVENT_DATE"] <- gsub("-", '',as.Date(dat[ ,"EVENT_DATE"],format="%d%B%Y:%H:%M:%S"))
            } else{
		dat[, "EVENT_DATE"] <- gsub("-", '', dat[ ,"EVENT_DATE"])
            }
			
            dat[, "EVENT_FILE"] <- efiles[i]
            dat[, "EVENT_VAR"] <- v[k]
            dat[, "PALA"] <- palas[i]
            dat[, "ICD_VER"] <- icdvs[i]
            dat[, "morpho"] <- NA
		
            HILMO_long <- rbind(HILMO_long, dat)	
	    }       
	    print(paste0("Diagnose ", k, "/", length(v), " done, including ", nrow(dat), " records."))
	}
	print(paste0("Done for ", efiles[i],"."))
}

HILMO_long <- as.data.frame(HILMO_long)

save(HILMO_long, file=paste0(r_dir, "HILMO_ICD_LONG_COMPLETE.Rdata"))
print(paste0("Done for HILMO Registry, with ", nrow(HILMO_long)," rows in total."))   




################################################
#       Extract Longtitudinal CANCER           #  
################################################

fcr <- get(load(paste0(r_dir, "fcr.Rdata")))
fcr$icd10_topo <- sprintf("C%03d",fcr$topo)    # format the topo column

CANCER_long <- NULL
print("Start for CANCER Registry")

for (k in 1:length(fcr_v)){
    dat <- fcr[fcr[,fcr_v[k]]!="", c("TNRO", "dg_date", "morpho", fcr_v[k])]   # Cancer were defined by both topo and morpho
    colnames(dat) <- c("ID", "EVENT_DATE",  "morpho", "ICD_CODE")
    dat[,"EVENT_DATE"] <- gsub("-", '', dat[ ,"EVENT_DATE"])
    dat[,"hospital"] <- NA
    dat[,"municipality"] <- NA
    dat[, "EVENT_FILE"] <- "CANCER"
    dat[, "EVENT_VAR"] <- fcr_v[k]	
    dat[, "PALA"] <- NA	
    dat[, "ICD_VER"] <- "10"
    CANCER_long <- rbind(CANCER_long, dat)
			
    print(paste0("Diagnose ", k, "/", length(fcr_v), " done, including ", nrow(dat), " records."))
}

CANCER_long <- as.data.frame(CANCER_long)
CANCER_long <- CANCER_long[ ,colnames(HILMO_long)]

save(CANCER_long, file=paste0(r_dir, "CANCER_long.Rdata"))
print(paste0("Done for CANCER Registry, with ", nrow(CANCER_long)," rows in total."))    




################################################
#         Extract Longtitudinal DEATH          #
################################################

dh <- get(load(paste0(r_dir, "kuolemansyyt_u1477_a.Rdata")))
dh_v <- c("tpks", "vks", paste0("m",1:4))

DEATH_long <- NULL

print("Start for DEATH Registry")
for (k in 1:length(dh_v)){
	dat <- dh[dh[,dh_v[k]]!="", c("TNRO", "kuolpvm", dh_v[k])]
	colnames(dat) <- c("ID", "EVENT_DATE",  "ICD_CODE")
	dat[,"EVENT_DATE"] <- gsub("-", '', dat[ ,"EVENT_DATE"])
	dat[,"hospital"] <- NA
	dat[,"municipality"] <- NA
	dat[, "EVENT_FILE"] <- "DEATH"
	dat[, "EVENT_VAR"] <- dh_v[k]	
	dat[, "PALA"] <- NA	
	dat[, "morpho"] <- NA	
	dat[, "ICD_VER"] <- ifelse(dat[ ,"EVENT_DATE"] < 19860000, "8", ifelse(dat[ ,"EVENT_DATE"] > 19960000, "10", "9"))
		
	DEATH_long <- rbind(DEATH_long, dat)
	
	print(paste0("Diagnose ", k, "/", length(dh_v), " done, including ", nrow(dat), " records."))

}

DEATH_long <- as.data.frame(DEATH_long)
DEATH_long <- DEATH_long[ ,colnames(HILMO_long)]

save(DEATH_long, file=paste0(r_dir, "DEATH_long.Rdata"))
print(paste0("Done for DEATH Registry, with ", nrow(DEATH_long)," rows in total"))  




################################################
# ICD codes and their corresponding endpoints  #
################################################

huf <- read.table("/homes/aliu/DSGE_LRS/input/HILMO_UPDATED_FIN.lst", sep="\t", header=T)    # endpoints and corresponding icd8-10
head(huf)

huff <- rl_fa <- NULL

for (i in 8:10){       # loop for ICD version
    huf_i <- huf[huf[ ,paste0("ICD",i)]!="--", c("ENDPOINT", paste0("ICD",i), "morpho")]
    huf_i <- huf_i %>% separate_rows(paste0("ICD",i), sep=",")     # one ICD code per row
    colnames(huf_i) <- c("ENDPOINT", "ICD_CODE", "morpho")
    huf_i[,"ICD_VER"] <- i
    huff <- rbind(huff, huf_i)
    rl_fi <- rl_f[rl_f$ICD_VER==i, ]
  
    for (k in 5:3){
        rl_fik <- merge(rl_fi, huf_i[,c("ENDPOINT","ICD_CODE")], by.x=paste0("ICD_p",k), by.y="ICD_CODE", all.x=T)
        rl_fik <- rl_fik[ , c("ICD_CODE","ICD_VER","ENDPOINT")]
        rl_fa <- rbind(rl_fa, rl_fik)
    }		
}
     
# write.table(huff, "huff_all.lst", append=F, quote=F, sep=" ", row.names=F, col.names=T)



