## This script is to check ICD codes from HILMO and DEATH registers in two perspectives: whether what we got are what we asked, and whether we got all we asked?


setwd("/home/aoxing/DSGE_LRS/out/registry_edit/")
in_dir <- "/home/aoxing/DSGE_LRS/input/"
r_dir <- "/home/aoxing/DSGE_LRS/input/r_files/"

library(tidyverse)



################################################
#               Functions                      #
################################################

'%!in%' <- function(x,y)!('%in%'(x,y))

# Count of 3/4/5-digit ICD codes for each version and whether they are what we asked (endpoint list)
icd_freq <- function(dat, name, ICD9_type){
	f <- data.frame(table(dat[,"ICD_CODE"], dat[,"ICD_VER"], useNA="always"))
	f <- f[f[,"Freq"]>0, ]
	colnames(f) <- c("ICD_CODE","ICD_VER",paste0("count_",name))
	f[ ,"ICD_CODE"] <- as.character(f[ ,"ICD_CODE"])
	f <- f[order(-f$count), ]
	for (k in 3:5){
		f[ ,paste0("ICD_p",k)] <- ifelse(nchar(f[ ,"ICD_CODE"])>=k, substr(f[ ,"ICD_CODE"], 1, k), NA)
	}
	print(paste0(nrow(f), " ICD codes in total."))    
	# write.table(f, paste0(name,"_lf"), append=F, quote=F, sep=" ", row.names=F, col.names=T)

	lfa <- NULL
	for (i in c(8:10)){
		if(i==9){
			huff_i <- huff[huff$ICD_VER==ICD9_type,]
		} else {
			huff_i <- huff[huff$ICD_VER==i,]	
		}	
		lf_i <- f[f$ICD_VER==i,]
		for (k in 3:5){
			lf_i[,paste0("ICD_p",k,"_yn")] <- lf_i[,paste0("ICD_p",k)] %in% huff_i$ICD_CODE
		}		
		lfa <- rbind(lfa, lf_i)
	}
	lfa$ICD_p345_yn <- rowSums(lfa[,paste0("ICD_p",3:5,"_yn")])
	return(lfa)	
}



################################################
#    Check inpatient/outpatient ICD codes      #
################################################

## All ICD codes for endpoints
huff <- read.table("huff_all.lst", header=T)
nrow(huff)  # 7,631

# count of 3/4/5-digit ICD codes for endpoints
table(huff$ICD_VER, nchar(huff$ICD_CODE)) 


## count of icd code in registry
HILMO_long <- data.frame(get(load(paste0(r_dir, "HILMO_long_COMPLETE.Rdata"))))
nrow(HILMO_long)  # 50,855,984
HILMO_freq <- icd_freq(HILMO_long, "HILMO", "9H")  # 11,095 ICD codes in total

table(HILMO_freq$ICD_VER, HILMO_freq$ICD_p3_yn) 
table(HILMO_freq$ICD_VER, HILMO_freq$ICD_p4_yn)    
table(HILMO_freq$ICD_VER, HILMO_freq$ICD_p5_yn)   # ICD9 doesn't have 5-digit codes


## check whether what we got are we asked
table(HILMO_freq$ICD_p345_yn)
HILMO_EXTRA_ICD <- HILMO_freq[HILMO_freq$ICD_p345_yn==0,]
table(HILMO_EXTRA_ICD$ICD_VER)

sum(HILMO_EXTRA_ICD$count_HILMO)  # 6,506
max(HILMO_EXTRA_ICD$count_HILMO)  # 701
 

## check whether we got all we asked
huff_H <- huff[huff$ICD_VER %in% c("8","9H","10"),]
huff_H[,"ICD_VER"] <- ifelse(huff_H[,"ICD_VER"]=="9H", 9, huff_H[,"ICD_VER"])
table(huff_H[,"ICD_VER"] )

# endpoint ICD codes we got in registry
table(huff_H[nchar(huff_H$ICD_CODE)==3 & (paste0(huff_H$ICD_VER,"_",huff_H$ICD_CODE) %in% paste0(HILMO_freq$ICD_VER,"_",HILMO_freq$ICD_p3)), "ICD_VER"])
table(huff_H[nchar(huff_H$ICD_CODE)==4 & (paste0(huff_H$ICD_VER,"_",huff_H$ICD_CODE) %in% paste0(HILMO_freq$ICD_VER,"_",HILMO_freq$ICD_p4)), "ICD_VER"])
table(huff_H[nchar(huff_H$ICD_CODE)==5 & (paste0(huff_H$ICD_VER,"_",huff_H$ICD_CODE) %in% paste0(HILMO_freq$ICD_VER,"_",HILMO_freq$ICD_p5)), "ICD_VER"])

# endpoint ICD codes we missed in registry
table(huff_H[nchar(huff_H$ICD_CODE)==3 & (paste0(huff_H$ICD_VER,"_",huff_H$ICD_CODE) %!in% paste0(HILMO_freq$ICD_VER,"_",HILMO_freq$ICD_p3)),"ICD_VER"])
table(huff_H[nchar(huff_H$ICD_CODE)==4 & (paste0(huff_H$ICD_VER,"_",huff_H$ICD_CODE) %!in% paste0(HILMO_freq$ICD_VER,"_",HILMO_freq$ICD_p4)),"ICD_VER"])
table(huff_H[nchar(huff_H$ICD_CODE)==5 & (paste0(huff_H$ICD_VER,"_",huff_H$ICD_CODE) %!in% paste0(HILMO_freq$ICD_VER,"_",HILMO_freq$ICD_p5)),"ICD_VER"])
 



################################################
#           Check  DEATH ICD codes             #
################################################

## count of icd code in registry
DEATH_long <- data.frame(get(load(paste0(r_dir, "DEATH_long.Rdata"))))
nrow(DEATH_long)  # 1,442,382
DEATH_freq <- icd_freq(DEATH_long, "DEATH", "9D")  # 11,095 ICD codes in total

table(DEATH_freq$ICD_VER, DEATH_freq$ICD_p3_yn) 
table(DEATH_freq$ICD_VER, DEATH_freq$ICD_p4_yn)    
table(DEATH_freq$ICD_VER, DEATH_freq$ICD_p5_yn)   # ICD9 doesn't have 5-digit codes, then check for the 5th digit of ICD10
data.frame(table(substr(DEATH_freq[nchar(DEATH_freq$ICD_CODE)==5 & DEATH_freq$ICD_VER==10,"ICD_CODE"],5,5)))  # most of the 5th digit of death ICD10 are end up with 0 or 9


## check whether what we got are we asked
table(DEATH_freq$ICD_p345_yn)
DEATH_EXTRA_ICD <- DEATH_freq[DEATH_freq$ICD_p345_yn==0,]
table(DEATH_EXTRA_ICD$ICD_VER)

sum(DEATH_EXTRA_ICD$count_DEATH)  # 774,555
max(DEATH_EXTRA_ICD$count_DEATH)  # 24,119


## check whether we got all we asked
huff_D <- huff[huff$ICD_VER %in% c("8","9D","10"),]
huff_D[,"ICD_VER"] <- ifelse(huff_D[,"ICD_VER"]=="9D", 9, huff_D[,"ICD_VER"])
table(huff_D[,"ICD_VER"] )

# endpoint ICD codes we got in registry
table(huff_D[nchar(huff_D$ICD_CODE)==3 & (paste0(huff_D$ICD_VER,"_",huff_D$ICD_CODE) %in% paste0(DEATH_freq$ICD_VER,"_",DEATH_freq$ICD_p3)), "ICD_VER"])
table(huff_D[nchar(huff_D$ICD_CODE)==4 & (paste0(huff_D$ICD_VER,"_",huff_D$ICD_CODE) %in% paste0(DEATH_freq$ICD_VER,"_",DEATH_freq$ICD_p4)), "ICD_VER"])
table(huff_D[nchar(huff_D$ICD_CODE)==5 & (paste0(huff_D$ICD_VER,"_",huff_D$ICD_CODE) %in% paste0(DEATH_freq$ICD_VER,"_",DEATH_freq$ICD_p5)), "ICD_VER"])

# endpoint ICD codes we missed in registry
table(huff_D[nchar(huff_D$ICD_CODE)==3 & (paste0(huff_D$ICD_VER,"_",huff_D$ICD_CODE) %!in% paste0(DEATH_freq$ICD_VER,"_",DEATH_freq$ICD_p3)),"ICD_VER"])
table(huff_D[nchar(huff_D$ICD_CODE)==4 & (paste0(huff_D$ICD_VER,"_",huff_D$ICD_CODE) %!in% paste0(DEATH_freq$ICD_VER,"_",DEATH_freq$ICD_p4)),"ICD_VER"])
table(huff_D[nchar(huff_D$ICD_CODE)==5 & (paste0(huff_D$ICD_VER,"_",huff_D$ICD_CODE) %!in% paste0(DEATH_freq$ICD_VER,"_",DEATH_freq$ICD_p5)),"ICD_VER"])

 

