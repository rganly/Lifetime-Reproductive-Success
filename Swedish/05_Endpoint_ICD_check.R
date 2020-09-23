## This script is to check ICD codes from HILMO and DEATH registers in two perspectives: one is whether what we get are we asked, the other is whether we got all we asked?

# Input: "huff_all.lst", "HILMO_long_COMPLETE.Rdata", "DEATH_long.Rdata"
# Output: 
# Comments:  


setwd("/home/aoxing/DSGE_LRS/out/registry_edit/")
in_dir <- "/home/aoxing/DSGE_LRS/input/"
r_dir <- "/home/aoxing/DSGE_LRS/input/r_files/"

library(tidyverse)



################################################
#               Functions                      #
################################################

'%!in%' <- function(x,y)!('%in%'(x,y))

# Count of 3/4/5-digit ICD codes for each version
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

# All ICD codes for endpoints
huff <- read.table("huff_all.lst", header=T)
nrow(huff)  # 7,631

table(huff$ICD_VER, nchar(huff$ICD_CODE)) 
#        3    4    5
#  10  785 1799   13
#  8   209  371  706
#  9D  241 1633    0
#  9H  241 1633    0



# whether the codes we got are those we asked?
HILMO_long <- data.frame(get(load(paste0(r_dir, "HILMO_long_COMPLETE.Rdata"))))
nrow(HILMO_long)  # 50,855,984
HILMO_freq <- icd_freq(HILMO_long, "HILMO", "9H")  # 11,095 ICD codes in total

table(HILMO_freq$ICD_VER, HILMO_freq$ICD_p3_yn) 
#     FALSE TRUE
#  10  1427 2322
#  8   1707  344
#  9   2612  287

table(HILMO_freq$ICD_VER, HILMO_freq$ICD_p4_yn)    
#     FALSE TRUE
#  10   911 2838
#  8   1286  765
#  9    217 2682

table(HILMO_freq$ICD_VER, HILMO_freq$ICD_p5_yn)   # ICD9 doesn't have 5-digit codes
#     FALSE TRUE
#  10  3728   21
#  8   1399  652
#  9   2899    0



# check whether what we get are we asked
table(HILMO_freq$ICD_p345_yn)
#   0    1    2    3 
# 507 6479 1707    6 

HILMO_EXTRA_ICD <- HILMO_freq[HILMO_freq$ICD_p345_yn==0,]

table(HILMO_EXTRA_ICD$ICD_VER)
# 10   8   9 
#137 370   0 

sum(HILMO_EXTRA_ICD$count_HILMO)  # 6,506
max(HILMO_EXTRA_ICD$count_HILMO)  # 701
 
 

# check whether we got all we asked
huff_H <- huff[huff$ICD_VER %in% c("8","9H","10"),]
huff_H[,"ICD_VER"] <- ifelse(huff_H[,"ICD_VER"]=="9H", 9, huff_H[,"ICD_VER"])
table(huff_H[,"ICD_VER"] )
#  10    8    9 
#2597 1286 1874 

ICD_EPYES_3 <- huff_H[nchar(huff_H$ICD_CODE)==3 & (paste0(huff_H$ICD_VER,"_",huff_H$ICD_CODE) %in% paste0(HILMO_freq$ICD_VER,"_",HILMO_freq$ICD_p3)),]
table(ICD_EPYES_3$ICD_VER)  
#  10   8   9 
# 764 197 232 

ICD_EPYES_4 <- huff_H[nchar(huff_H$ICD_CODE)==4 & (paste0(huff_H$ICD_VER,"_",huff_H$ICD_CODE) %in% paste0(HILMO_freq$ICD_VER,"_",HILMO_freq$ICD_p4)),]
table(ICD_EPYES_4$ICD_VER) 
#   10    8    9 
# 1701  350 1612 

ICD_EPYES_5 <- huff_H[nchar(huff_H$ICD_CODE)==5 & (paste0(huff_H$ICD_VER,"_",huff_H$ICD_CODE) %in% paste0(HILMO_freq$ICD_VER,"_",HILMO_freq$ICD_p5)),]
table(ICD_EPYES_5$ICD_VER) 
#  10   8 
#   8 679 

sum(nrow(ICD_EPYES_3), nrow(ICD_EPYES_4), nrow(ICD_EPYES_5))  # 5,543 codes we asked are there



ICD_EPNO_3 <- huff_H[nchar(huff_H$ICD_CODE)==3 & (paste0(huff_H$ICD_VER,"_",huff_H$ICD_CODE) %!in% paste0(HILMO_freq$ICD_VER,"_",HILMO_freq$ICD_p3)),]
table(ICD_EPNO_3$ICD_VER)  
# 10  8  9 
# 21 12  9 

ICD_EPNO_4 <- huff_H[nchar(huff_H$ICD_CODE)==4 & (paste0(huff_H$ICD_VER,"_",huff_H$ICD_CODE) %!in% paste0(HILMO_freq$ICD_VER,"_",HILMO_freq$ICD_p4)),]
table(ICD_EPNO_4$ICD_VER) 
# 10  8  9 
# 98 21 21 

ICD_EPNO_5 <- huff_H[nchar(huff_H$ICD_CODE)==5 & (paste0(huff_H$ICD_VER,"_",huff_H$ICD_CODE) %!in% paste0(HILMO_freq$ICD_VER,"_",HILMO_freq$ICD_p5)),]
table(ICD_EPNO_5$ICD_VER) 
# 10  8
# 5 27

sum(nrow(ICD_EPNO_3), nrow(ICD_EPNO_4), nrow(ICD_EPNO_5))  # 214 codes we asked are not there
 



################################################
#           Check  DEATH ICD codes             #
################################################







