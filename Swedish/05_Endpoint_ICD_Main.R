## This script is to convert ICD codes from HILMO and DEATH registers to endpoints 

# Input: "HILMO_UPDATED_SWE.lst", "ut_par_sv_27035_2018_COMPLETE.Rdata", "ut_par_ov_27035_2018.Rdata", "ut_dors_indexpers_27035_2018.Rdata", "ut_dors_b_bbarn_27035_2018.Rdata" 
# Output: "huff_all.lst", "ICD_ASK_Count.tsv","SWE_ENDPOINT_Prevalence.tsv",  "HILMO_long_COMPLETE.Rdata", "DEATH_long.Rdata", "ry_long_COMPLETE.Rdata", "ry_first_COMPLETE.Rdata", "ry_first_index_COMPLETE.Rdata", "ry_first_indexW_COMPLETE.Rdata"
# Comments:  


setwd("/home/aoxing/DSGE_LRS/out/registry_edit/")
in_dir <- "/home/aoxing/DSGE_LRS/input/"
r_dir <- "/home/aoxing/DSGE_LRS/input/r_files/"

library(tidyverse)



################################################
#               Functions                      #
################################################

'%!in%' <- function(x,y)!('%in%'(x,y))

# Read in original hospital discharge registry data
read_hilmo <- function(file_name, file_type){
	dat <- data.frame(get(load(paste0(r_dir, file_name))))
	print(paste0("Original ",file_type," registry has ",nrow(dat)," rows."))       
	dat <- dat[dat[,"hdia"]!="", c("LopNr","hdia","AR","INDATUMA")]
	dat[,"INDATUMA"] <- gsub(" |\\-", "", dat[,"INDATUMA"])
	dat[,"EVENT_DATE"] <- as.numeric(as.character(ifelse(nchar(dat[,"INDATUMA"])<8, paste0(dat[,"AR"],"1232"), dat[,"INDATUMA"]) ))  # date with less than 8 digit, use year1232 as date
	dat[,"PALA"] <- file_type
	dat[,"ICD_CODE"] <- gsub("\xc5L|\\\x99|\\,", "", dat[,"hdia"])
	dat <- dat[,c("LopNr", "EVENT_DATE", "ICD_CODE", "PALA")]
	print(paste0("After removing records with empty diagnose ICD codes, ",file_type," registry has ",nrow(dat)," rows."))      
	return(dat)	
}


# Extract inpatient hospital discharge registry for a certain time period and specific ICD version
extract_i_hilmo <- function(start, end, icd_v){
	dat <- i_hilmo[i_hilmo$EVENT_DATE>=start & i_hilmo$EVENT_DATE<=end, ]
	dat[,"ICD_VER"] <- icd_v    # add icd version
	if(icd_v=="10"){
		dat <- dat[substr(dat$ICD_CODE,1,1) %in% c(LETTERS,"Y"), ]
	} else{
		dat <- dat[substr(dat$ICD_CODE,1,1) %in% seq(1,9,1), ]
	}
	print(paste0(nrow(dat) ," records with ", length(unique(dat$hdia)), " ICD-", icd_v, " codes from ", start, " to ", end))		
	return(data.frame(dat))	
}


# QC for ICD codes
qc_icd <- function(dat){
	dat[,"ICD_CODE"] <- gsub("-|\\,|\\ .*|\\.|\\\t|\\\r|\\*|\\'|\\#|\\?|\\/", "", dat[,"ICD_CODE"])
	dat <- dat[nchar(dat[,"ICD_CODE"])>=3, ]
	dat <- dat[nchar(as.character(dat[,"ICD_CODE"]))>=3, ]
	return(dat)	
}



################################################
#    ICD codes and corresponding endpoints     #
################################################

# One ICD codes per row, the 4th digit is letter in HILMO registry but are digit in DEATH registry
huf <- read.csv(paste0(in_dir,"HILMO_UPDATED_SWE.lst"), sep="\t", header=T)    # endpoints and corresponding icd8-10
colnames(huf) <- c("ENDPOINT","ICD8","ICD9H","ICD9D","ICD10","LONGNAME")
huf[] <- lapply(huf, as.character)

for (i in c("8","9H","9D","10")){    
	huf_i <- huf[huf[ ,paste0("ICD",i)]!="--", c("ENDPOINT", paste0("ICD",i))]
	huf_i <- huf_i %>% separate_rows(paste0("ICD",i), sep=",")     	
	colnames(huf_i) <- c("ENDPOINT", "ICD_CODE")
	huf_i[,"ICD_VER"] <- i	
	if(i==8){huff <- huf_i} else {huff <- rbind(huff, huf_i)}	
}
huff <- data.frame(huff)
nrow(huff)   # 7,631
table(huff$ICD_VER,nchar(huff$ICD_CODE))
write.table(huff, "huff_all.lst", append=F, quote=F, sep=" ", row.names=F, col.names=T)




################################################
#         Extract Longtitudinal HILMO          #
################################################

# Inpatient (ICD-8,9,10) 1969-2018 
i_hilmo <- read_hilmo("ut_par_sv_27035_2018_COMPLETE.Rdata", "inpatient")
i_hilmo_8  <- extract_i_hilmo(19690000, 19861300, "8")    
i_hilmo_9  <- extract_i_hilmo(19870000, 19971300, "9")   # 1997 use both ICD9 and ICD10
i_hilmo_10 <- extract_i_hilmo(19970000, 20181300, "10")  # 1997 use both ICD9 and ICD10
i_hilmo <- rbind(i_hilmo_8, i_hilmo_9, i_hilmo_10)
i_hilmo <- i_hilmo[,c("LopNr", "EVENT_DATE", "ICD_VER", "ICD_CODE", "PALA")]
# save(i_hilmo, file=paste0(r_dir, "i_hilmo_COMPLETE.Rdata"))
rm(i_hilmo_8, i_hilmo_9, i_hilmo_10)


# Outpatient (ICD-10) 1997-2018 
o_hilmo <- read_hilmo("ut_par_ov_27035_2018.Rdata", "outpatient")
o_hilmo[,"ICD_VER"] <- "10"
o_hilmo <- o_hilmo[,c("LopNr", "EVENT_DATE", "ICD_VER", "ICD_CODE", "PALA")]
# save(o_hilmo, file=paste0(r_dir, "o_hilmo.Rdata"))


# Combine Inpatient and Outpatient 
HILMO_long <- rbind(o_hilmo, i_hilmo)
nrow(HILMO_long)     # 50,855,953  
HILMO_long[,"EVENT_FILE"] <- "HILMO"
HILMO_long[,"EVENT_VAR"] <- "hdia"
HILMO_long <- HILMO_long[,c("LopNr", "EVENT_DATE", "ICD_CODE", "PALA", "ICD_VER", "EVENT_FILE", "EVENT_VAR")] 
HILMO_long <- qc_icd(HILMO_long)
nrow(HILMO_long)     # 50，855，952
save(HILMO_long, file=paste0(r_dir, "HILMO_long_COMPLETE.Rdata"))
rm(i_hilmo)
rm(o_hilmo)



################################################
#               Extract  DEATH                 #
################################################

# Combine death registers for index person, children and grandchildren's generations
d_i <- data.frame(get(load(paste0(r_dir, "ut_dors_indexpers_27035_2018.Rdata"))))
d_c <- data.frame(get(load(paste0(r_dir, "ut_dors_b_bbarn_27035_2018.Rdata"))))
ut_dors <- rbind(d_i, d_c)
ut_dors <- ut_dors[,c("LopNr","AR","DODSDAT","ICD","ULORSAK","KAP19",paste0("MORSAK",1:10))]  
save(ut_dors, file=paste0(r_dir, "ut_dors.Rdata"))          
rm(d_i, d_c)

dh_v <- c("ULORSAK",paste0("MORSAK",1:10))
DEATH_long <- NULL
for (k in 1:length(dh_v)){
	dat <- ut_dors[ut_dors[,dh_v[k]]!="", c("LopNr", "DODSDAT", "ICD", dh_v[k])]
	colnames(dat) <- c("LopNr", "EVENT_DATE", "ICD_VER", "ICD_CODE")
	dat[,"EVENT_DATE"] <- gsub("-", '', as.character(dat[ ,"EVENT_DATE"]))
	dat[,"EVENT_FILE"] <- "DEATH"
	dat[,"EVENT_VAR"] <- dh_v[k]	
	dat[,"PALA"] <- NA	
	DEATH_long <- rbind(DEATH_long, dat)
	print(paste0("Diagnose ", k, "/", length(dh_v), " done, including ", nrow(dat), " records."))
}

DEATH_long <- DEATH_long[,c("LopNr", "EVENT_DATE", "ICD_CODE", "PALA", "ICD_VER", "EVENT_FILE", "EVENT_VAR") ]
DEATH_long <- qc_icd(DEATH_long)
save(DEATH_long, file=paste0(r_dir, "DEATH_long.Rdata"))



################################################
#         Combine HILMO & DEATH                #
################################################

# Longitudinal ICD codes
rl <- rbind(HILMO_long, DEATH_long)
nrow(rl)      # 52,298,334


# Count of ICD codes (& morpho) by ICD version
rl_f <- as.data.frame(table(rl$ICD_CODE, rl$ICD_VER, useNA="always"))
colnames(rl_f) <- c("ICD_CODE","ICD_VER","count")
nrow(rl_f)    # 103,956
rl_f <- rl_f[rl_f$count>0, ]
rl_f <- rl_f[order(-rl_f$count), ]
nrow(rl_f)    # 18,496   
rl_f[ ,"ICD_CODE"]<- as.character(rl_f[ ,"ICD_CODE"])


# Counts of ICD codes we asked
rl_fa <- NULL
for (k in 3:5){
	rl_f[ ,paste0("ICD_p",k)] <- ifelse(nchar(rl_f[ ,"ICD_CODE"])>=k, substr(rl_f[ ,"ICD_CODE"], 1, k), NA)
	rl_ff <- rl_f[is.na(rl_f[ ,paste0("ICD_p",k)])==F, c("ICD_VER","ICD_CODE", paste0("ICD_p",k))]
	colnames(rl_ff) <- c("ICD_VER", "ICD_CODE_ORIGINAL", "ICD_CODE_FORMAT")
	rl_fa <- rbind(rl_fa, rl_ff)
}

huff[,"ICD_VER_F"] <- ifelse(huff[,"ICD_VER"]=="9D"|huff[,"ICD_VER"]=="9H", 9, huff[,"ICD_VER"])
ep_lst <- inner_join(huff, rl_fa, by=c("ICD_VER_F"="ICD_VER", "ICD_CODE"="ICD_CODE_FORMAT"))
table(ep_lst$ICD_VER_F)
write.table(ep_lst, "ICD_ASK_COUNT.tsv", append=F, quote=F, sep="\t", row.names=F, col.names=T)



################################################
#    Convert from ICD codes to ENDPOINTS      #
################################################

# Longitudinal endpoint and ICD codes (asked) 
ry_long <- inner_join(rl, ep_lst, by=c("ICD_VER"="ICD_VER_F", "ICD_CODE"="ICD_CODE_ORIGINAL"))
nrow(ry_long)  # 67,166,049 
colnames(ry_long) <- c("LopNr","EVENT_DATE","ICD_CODE","PALA","ICD_VER","EVENT_FILE","EVENT_VAR","ENDPOINT","ICD_CODE_FORMAT","ICD_VER_FORMAT") 
save(ry_long, file=paste0(r_dir, "ry_long_COMPLETE.Rdata"))      
rm(rl)


# First endpoint event
ry_first <- ry_long %>% select(LopNr, ENDPOINT, EVENT_DATE) %>% distinct() %>% group_by(LopNr, ENDPOINT) %>% summarize(EVENT_F_DATE=min(EVENT_DATE), EVENT_N=length(EVENT_DATE)) 
nrow(ry_first)  # 28,148,337
ry_first <- data.frame(ry_first)
save(ry_first, file=paste0(r_dir, "ry_first_COMPLETE.Rdata"))      

 
# Extract first endpoint event for index person   
index <- get(load(paste0(r_dir, "tove_lev_index.Rdata")))      
length(index$LopNr)  # 2,893,654
ry_first_index <- ry_first %>% filter(LopNr %in% index$LopNr)  
nrow(ry_first_index) # 15,080,284
length(unique((ry_first_index[, "LopNr"])))      # 2,368,488
length(unique((ry_first_index[, "ENDPOINT"])))   # 1,959
save(ry_first_index, file=paste0(r_dir, "ry_first_index_COMPLETE.Rdata")) 


# Extract first endpoint event for QCed index person   
indexW <- get(load(paste0(r_dir, "indexW_LRS.Rdata")))
length(indexW$LopNr)      # 2,555,541
ry_first_indexW <- ry_first %>% filter(LopNr %in% indexW$LopNr)  
nrow(ry_first_indexW)     # 14,177,074
length(unique((ry_first_indexW[, "LopNr"])))     # 2,191,135
length(unique((ry_first_indexW[, "ENDPOINT"])))  # 1,959
save(ry_first_indexW, file=paste0(r_dir, "ry_first_indexW_COMPLETE.Rdata")) 



################################################
#         Prevalence of ENDPOINTS              #
################################################

# Prevalence in index person
ep_f <- as.data.frame(table(ry_first_index[,"ENDPOINT"]))
colnames(ep_f) <- c("ENDPOINT","count_Index")
ep_f[,"Prevalence_Index"] <- ep_f[,"count_Index"]/length(index$LopNr)
nrow(ep_f)  # 1,959


# Prevalence in QCed index person
ep_fW <- as.data.frame(table(ry_first_indexW[,"ENDPOINT"]))
colnames(ep_fW) <- c("ENDPOINT","count_IndexW")
ep_fW[,"Prevalence_IndexW"] <- ep_fW[,"count_IndexW"]/length(indexW$LopNr)
nrow(ep_fW)  # 1,959


# Combine and add endpoint info
ep_preval <- inner_join(ep_f, ep_fW, by=c("ENDPOINT"="ENDPOINT"))
ep_preval <- inner_join(ep_preval, huf, by=c("ENDPOINT"="ENDPOINT"))
ep_preval <- ep_preval[order(-ep_preval$Prevalence_IndexW),]
nrow(ep_preval)  # 1,959
sum(ep_preval$Prevalence_IndexW>=ep_preval$Prevalence_Index)  # 1,867
write.table(ep_preval, "SWE_ENDPOINT_Prevalence.tsv", append=F, quote=F, sep="\t", row.names=F, col.names=T)



