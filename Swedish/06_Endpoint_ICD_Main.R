## Convert from ICD codes to endpoints using HILMO and DEATH registry

setwd("/home/aoxing/lrs/output/registry_edit/")

library(tidyverse)




##########################################
#        Parameters &  Functions         #
##########################################

## Parameters 
r_dir <- "/home/aoxing/DSGE_LRS/input/r_files/"
v_hilmo <- c("LopNr", "EVENT_DATE", "ICD_VER", "ICD_CODE", "PALA")
v_long <- c(v_hilmo, "EVENT_FILE", "EVENT_VAR")  




#----------------------------------------
## Functions
# Not %in% 
'%!in%' <- function(x,y)!('%in%'(x,y))

# Read in original hospital discharge registry data
read_hilmo <- function(file_name, file_type){
	dat <- data.frame(get(load(paste0(r_dir, file_name))))
	print(paste0("Original ",file_type," registry has ",nrow(dat)," rows."))       
	dat <- dat[dat[,"hdia"]!="", c("LopNr","hdia","AR","INDATUMA")]
	dat[,"EVENT_DATE"] <- as.numeric(as.character(ifelse(nchar(dat[,"INDATUMA"])<8, paste0(dat[,"AR"],"1232"), dat[,"INDATUMA"]) ))  # date with less than 8 digit, use year1232 as date
	dat[,"PALA"] <- "outpatient" 
	dat[,"ICD_CODE"] <- gsub("\xc5L|\\\x99", "", dat[,"hdia"])
	dat <- dat[,v_hilmo]
	print(paste0("After removing records with empty diagnose ICD codes, ",file_type," registry has ",nrow(dat)," rows."))      
	return(dat)	
}


# Extract inpatient hospital discharge registry for a certain time period and specific ICD version
extract_i_hilmo <- function(start, end, icd_v){
	dat <- i_hilmo[i_hilmo$INDATUMA>=start & i_hilmo$INDATUMA<=end, ]
	dat[,"ICD_VER"] <- icd_v    # add icd version
	if(icd_v=="10"){
		dat <- dat[substr(dat$hdia,1,1) %in% c(LETTERS,"Y"), ]
	} else{
		dat <- dat[substr(dat$hdia,1,1) %in% seq(1,9,1), ]
	}
	print(paste0(nrow(dat) ," records with ", length(unique(dat$hdia)), " ICD-", icd_v, " codes from ", start, " to ", end))		
	return(data.frame(dat))	
}


# QC for ICD codes
qc_icd <- function(dat){
	dat[,"ICD_CODE"] <- gsub("-|\\,|\\ .*|\\.|\\\t|\\\r|\\*|\\'|\\#|\\?|\\/", "", dat[,"ICD_CODE"])
	dat <- dat[nchar(dat[,"ICD_CODE"])>=3, ]
	return(dat)	
}


# Count of 3/4/5-digit ICD codes for each version
icd_freq <- function(dat, name){
	f <- data.frame(table(dat[,"ICD_CODE"], dat[,"ICD_VER"], useNA="always"))
	f <- f[f[,3]>0, ]
	colnames(f) <- c("ICD_CODE","ICD_VER",paste0("count_",name))
	f[ ,"ICD_CODE"] <- character(f[ ,"ICD_CODE"])
	f <- f[order(-f$count), ]
	for (k in 3:5){
		f[ ,paste0("ICD_p",k)] <- ifelse(nchar(f[ ,"ICD_CODE"])>=k, substr(f[ ,"ICD_CODE"], 1, k), NA)
	}
	print(paste0(nrow(f), " ICD codes in total."))    
	write.table(f, paste0(name,"_lf"), append=F, quote=F, sep=" ", row.names=F, col.names=T)
	return(f)	
}




################################################
#    ICD codes and corresponding endpoints     #
################################################

## One ICD codes per row, the 4th digit is letter in HILMO registry but are digit in DEATH registry
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

nrow(huff)   # 7,631
table(huff$ICD_VER,nchar(huff$ICD_CODE))
write.table(huff, "huff_all.lst", append=F, quote=F, sep=" ", row.names=F, col.names=T)



################################################
#         Extract Longtitudinal HILMO          #
################################################

print("Start for HILMO Registry")
#----------------------------------------
## Inpatient (ICD-8,9,10) 1969-2018 
i_hilmo <- read_hilmo("ut_par_sv_27035_2018.Rdata", "inpatient")

i_hilmo_8  <- extract_i_hilmo(19690000, 19861300, "8")    
i_hilmo_9  <- extract_i_hilmo(19870000, 19971300, "9")   # 1997 use both ICD9 and ICD10
i_hilmo_10 <- extract_i_hilmo(19970000, 20181300, "10")  # 1997 use both ICD9 and ICD10
i_hilmo <- rbind(i_hilmo_8, i_hilmo_9, i_hilmo_10)

colnames(i_hilmo) <- c("LopNr","ICD_CODE","INDATUMA","ICD_VER")
i_hilmo <- i_hilmo[,c("LopNr", "INDATUMA", "ICD_VER", "ICD_CODE", "PALA")]
save(i_hilmo, file=paste0(r_dir, "i_hilmo.Rdata"))

# i_hilmo <- as.data.frame(get(load(paste0(r_dir, "ut_par_sv_27035_2018.Rdata"))))
# nrow(i_hilmo)          # 17,230,229
# i_hilmo$INDATUMA <- as.numeric(as.character(ifelse(nchar(i_hilmo$INDATUMA)<8, paste0(i_hilmo[,"AR"],"1232"), i_hilmo[,"INDATUMA"])))  # 
# i_hilmo <- i_hilmo[i_hilmo$hdia!="", c("LopNr","hdia","INDATUMA")]
# nrow(i_hilmo)          # 13,235,209
# i_hilmo[,"PALA"] <- "inpatient"


#----------------------------------------
## Outpatient (ICD-10) 1997-2018 

o_hilmo <- read_hilmo("ut_par_ov_27035_2018.Rdata", "outpatient")

# o_hilmo <- as.data.frame(get(load(paste0(r_dir, "ut_par_ov_27035_2018.Rdata"))))
# nrow(o_hilmo)         # 40,495,883
# o_hilmo$INDATUMA <- ifelse(nchar(o_hilmo$INDATUMA)<8, paste0(o_hilmo[,"AR"],"1232"),o_hilmo[,"INDATUMA"])
# o_hilmo <- o_hilmo[o_hilmo$hdia!="", c("LopNr","hdia","INDATUMA")]
# nrow(o_hilmo)         # 36,113,976
# o_hilmo[,"PALA"] <- "outpatient" 

table(nchar(o_hilmo$INDATUMA))

colnames(o_hilmo) <- c("LopNr","ICD_CODE","INDATUMA")
o_hilmo[,"ICD_VER"] <- "10"

# o_hilmo[,"ICD_CODE"] <- gsub("\xc5L|\\\x99", "", o_hilmo[,"ICD_CODE"])
o_hilmo <- o_hilmo[, colnames(i_hilmo)]
save(o_hilmo, file=paste0(r_dir, "o_hilmo.Rdata"))




#----------------------------------------
## Combine Inpatient and Outpatient 

HILMO_long <- rbind(o_hilmo, i_hilmo)
nrow(HILMO_long)     # 49,348,819   

HILMO_long[,"EVENT_FILE"] <- "HILMO"
HILMO_long[,"EVENT_VAR"] <- "hdia"
colnames(HILMO_long) <- v_long

HILMO_long <- qc_icd(HILMO_long)
save(HILMO_long, file=paste0(r_dir, "HILMO_long.Rdata"))


HILMO_long_index <- HILMO_long[HILMO_long$ &in% index_lst]


#----------------------------------------
## Convert from ICD codes to Endpoints

HILMO_lf <- icd_freq(HILMO_long, "HILMO")  # "6811 ICD codes in total."
##   HILMO_lf <- read.table("HILMO_lf", header=T)
HILMO_lf[] <- lapply(HILMO_lf, as.character)
##   huff <- read.table("huff_all.lst", header=T)




## whether ICD codes in HILMO registry are what we asked (huff)

for (i in 8:10){
	print(i)
	huff_i <- huff[huff$ICD_VER==i,]
	HILMO_lf_i <- HILMO_lf[HILMO_lf$ICD_VER==i,]
	
	for (k in 3:5){
		print(k)
		HILMO_lf_i[,paste0("ICD_p",k,"_yn")] <- HILMO_lf_i[,paste0("ICD_p",k)] %in% huff_i$ICD_CODE
	}
		
	if(i==8){HILMO_lfa <- HILMO_lf_i} else {HILMO_lfa <- rbind(HILMO_lfa, HILMO_lf_i)}
	
}

HILMO_lfa$ICD_p345_yn <- rowSums(HILMO_lfa[,paste0("ICD_p",3:5,"_yn")])

table(HILMO_lfa$ICD_p345_yn)
#   0    1    2 
# 145 5026 1640 

HILMO_lfa[HILMO_lfa$ICD_p345_yn==0 & as.numeric(HILMO_lfa$count)>10, 1:7]  # only a few ICD codes are not what we asked.
write.table(HILMO_lfa, "HILMO_lfa.lst", append=F, quote=F, sep=" ", row.names=F, col.names=T)



## count of required icd code (huff) in HILMO registry

for (k in 3:5){
	print(k)	
	HILMO_lfa_k <- HILMO_lfa[HILMO_lfa[,paste0("ICD_p",k,"_yn")]==T, c("ICD_CODE","ICD_VER",paste0("ICD_p",k),"count_HILMO")]	
	colnames(HILMO_lfa_k) <- c("ICD_CODE","ICD_VER","ICD_CODE_F","count_HILMO")
	
	if(k==3){HILMO_freq <- HILMO_lfa_k} else {HILMO_freq <- rbind(HILMO_freq, HILMO_lfa_k)}
}

HILMO_freq <- unique(HILMO_freq)
HILMO_freq$count_HILMO <- as.numeric(HILMO_freq$count_HILMO)	
HILMO_freq$n <- paste0(HILMO_freq$ICD_VER, "_", HILMO_freq$ICD_CODE)
HILMO_freq$nf <- paste0(HILMO_freq$ICD_VER, "_", HILMO_freq$ICD_CODE_F)
dim(HILMO_freq)

HILMO_freq_icdf <- aggregate(count_HILMO ~ ICD_VER + ICD_CODE_F, data=HILMO_freq, sum)
HILMO_freq_icdf[,"n"] <- paste0(HILMO_freq_icdf$ICD_VER, "_", HILMO_freq_icdf$ICD_CODE_F)

huff$n <- paste0(huff$ICD_VER, "_", huff$ICD_CODE)
huff_u <- unique(huff[,c("n", "ICD_CODE", "ICD_VER")])

HILMO_ask <- merge(huff_u, HILMO_freq_icdf[,c("n","count_HILMO")], by="n", all.x=T)[,c("n","ICD_VER","ICD_CODE","count_HILMO")]
colnames(HILMO_ask) <- c("n","ICD_VER","ICD_CODE","n_consider_subcat")
HILMO_ask <- merge(HILMO_ask, unique(HILMO_freq[,c("n","count_HILMO")]), by="n", all.x=T)[,-1]
colnames(HILMO_ask) <- c("ICD_VER", "ICD_CODE", "n_consider_subcat", "n_exact_match")
HILMO_ask[is.na(HILMO_ask)] <- 0    

write.table(HILMO_ask, "HILMO_ask.tsv", append=F, quote=F, sep=" ", row.names=F, col.names=T)


# list of ENDPOINT and ICD_VER & original ICD_CODE for converting phenotypes
HILMO_endpoint <- merge(huff[,c("ENDPOINT","n")], HILMO_freq, by.x="n", by.y="nf")[,c("ICD_VER", "ICD_CODE", "ICD_CODE_F", "count_HILMO", "ENDPOINT")]
write.table(HILMO_endpoint, "HILMO_endpoint.tsv", append=F, quote=F, sep=" ", row.names=F, col.names=T)


################
# HILMO_endpoint <- read.table("HILMO_endpoint.tsv", header=T)
length(unique(HILMO_endpoint$ENDPOINT))   # 1,946

# HILMO_long <- as.data.frame(get(load(paste0(r_dir,"HILMO_long.Rdata"))))
dim(HILMO_long)


##
ICD_10_1 <- unique(substr(HILMO_endpoint[HILMO_endpoint$ICD_VER==10,"ICD_CODE"],1,1))

for (i in 8:10){ 
	print(i)
	if (i!=10){
		HILMO_l_i <- HILMO_long[HILMO_long$ICD_VER==i, ]
		print(nrow(HILMO_l_i))   
		h_ep_i <- HILMO_endpoint[HILMO_endpoint$ICD_VER==i, c("ICD_CODE","ENDPOINT")]
		print(nrow(h_ep_i)) 	
		HILMO_l_io <- merge(HILMO_l_i, h_ep_i, by="ICD_CODE")	
		print(nrow(HILMO_l_io)) 
	} else{
		for (n in 1:length(ICD_10_1)){
			print(n)		
			HILMO_l_i_n <- HILMO_long[HILMO_long$ICD_VER==i & substr(HILMO_long$ICD_CODE,1,1)==ICD_10_1[n], ]
			print(nrow(HILMO_l_i_n))					
			h_ep_i_n <- HILMO_endpoint[HILMO_endpoint$ICD_VER==i & substr(HILMO_endpoint$ICD_CODE,1,1)==ICD_10_1[n], c("ICD_CODE","ENDPOINT")]
			print(nrow(h_ep_i_n))		
			HILMO_l_i_no_n <- merge(HILMO_l_i_n, h_ep_i_n, by="ICD_CODE")	
			print(nrow(HILMO_l_i_no_n)) 	
			if(n==1){HILMO_l_io <- HILMO_l_i_no_n} else {HILMO_l_io <- rbind(HILMO_l_io, HILMO_l_i_no_n)}
		}	
	}
	if(i==8){HILMO_long_ep <- HILMO_l_io} else {HILMO_long_ep <- rbind(HILMO_long_ep, HILMO_l_io)}
}

dim(HILMO_long_ep)  # 61,548,718
save(HILMO_long_ep, file=paste0(r_dir,"HILMO_long_ep.Rdata"))

table(HILMO_long_ep$ICD_VER)
#       10        8        9 
# 53574400  2916388  5047378 

table(HILMO_long_ep$PALA)
# inpatient outpatient 
#  15405638   46132528 


length(unique(HILMO_long_ep$ENDPOINT))   # 1,946
length(unique(HILMO_long_ep$ID))

table(nchar(HILMO_long_ep$EVENT_DATE))
#        8 
# 61538166 




######################################################################
#   Extract frist diagnose for each endpoint from longitudinal HILMO #
######################################################################

## Extract the first diagnose for each endpoint for each

HILMO_long_b <- HILMO_long_ep[ , c("ID", "EVENT_DATE", "ENDPOINT")]

HILMO_long_b$ENDPOINT <- as.character(HILMO_long_b$ENDPOINT)
dup <- duplicated(HILMO_long_b)
HILMO_long_b <- HILMO_long_b[!dup, ]
nrow(HILMO_long_b)    # 60,527,117
HILMO_long_b <- HILMO_long_b[order(HILMO_long_b$ID, HILMO_long_b$ENDPOINT, HILMO_long_b$EVENT_DATE), ]
save(HILMO_long_b, file=paste0(r_dir, "HILMO_long_b.Rdata"))  



HILMO_first <- aggregate(EVENT_DATE ~ ID+ENDPOINT, data=HILMO_long_b, FUN=function(x) c(EVENT_F_DATE = head(x, n=1), EVENT_N = length(x)) )
HILMO_first <- as.matrix(HILMO_first)
dim(HILMO_first)     # 26595728 
colnames(HILMO_first) <- c("ID", "ENDPOINT", "EVENT_F_DATE", "EVENT_N")
#HILMO_first <- gsub("[[:space:]]", "", HILMO_first)

HILMO_first <- as.data.frame(HILMO_first)
HILMO_first[] <- lapply(HILMO_first, as.character)
HILMO_first[,"EVENT"] <- paste(HILMO_first[,"ID"], HILMO_first[,"ENDPOINT"], sep="_")

save(HILMO_first, file=paste0(r_dir, "HILMO_first.Rdata"))  



################
################
index <- get(load(paste0(r_dir, "tove_lev_index.Rdata")))      
index_lst <- unique(index$LopNr); length(index_lst)       # 2,893,654

HILMO_first_index <- HILMO_first[HILMO_first[ ,"ID"] %in% index_lst, ]
nrow(HILMO_first_index)       # 14,659,605
length(unique((HILMO_first_index[ , "ID"])))         # 2,339,456
length(unique((HILMO_first_index[ , "ENDPOINT"])))   # 1，936
save(HILMO_first_index, file=paste0(r_dir, "HILMO_first_index.Rdata")) 

ep_f <- as.data.frame(table(HILMO_first[ ,"ENDPOINT"]))
colnames(ep_f) <- c("ENDPOINT","count")
ep_f <- ep_f[order(-ep_f$count), ]


ep_f_index <- as.data.frame(table(HILMO_first_index[ ,"ENDPOINT"]))
colnames(ep_f_index) <- c("ENDPOINT","count")
ep_f_index <- ep_f_index[order(-ep_f_index$count), ]

endpoint_f <- merge(ep_f, ep_f_index, by="ENDPOINT", all=T)
endpoint_f[is.na(endpoint_f)] <- 0
colnames(endpoint_f) <- c("ENDPOINT", "COUNT_FULL_CASES", "COUNT_INDEX_CASES")

endpoint_f <- merge(endpoint_f, huf, by="ENDPOINT")
endpoint_f <- data.frame(lapply(endpoint_f, as.character), stringsAsFactors=FALSE)
endpoint_f <- endpoint_f[order(-as.numeric(endpoint_f$COUNT_INDEX_CASES)), ]
endpoint_f[ ,"RANK"] <- 1:nrow(endpoint_f)
endpoint_f$prevalence_index <- round(as.numeric(endpoint_f[,"COUNT_INDEX_CASES"])/length(index_lst),4)

write.table(endpoint_f, "endpoint_f.tsv", append=F, quote=F, sep="\t", row.names=F, col.names=T)



################################################
#               Extract  DEATH                 #
################################################

## Combine files index person, children and grandchildren's generations
print("Start for DEATH Registry")
d_i <- get(load(paste0(r_dir, "ut_dors_indexpers_27035_2018.Rdata")))
d_c <- get(load(paste0(r_dir, "ut_dors_b_bbarn_27035_2018.Rdata")))
ut_dors <- rbind(d_i, d_c)
ut_dors <- ut_dors[,c("LopNr","AR","DODSDAT","ICD","ULORSAK","KAP19",paste0("MORSAK",1:10))]  
save(ut_dors, file=paste0(r_dir, "ut_dors.Rdata"))          
ut_dors <- as.data.frame(get(load(paste0(r_dir,"ut_dors.Rdata"))))

 
dh_v <- c("ULORSAK",paste0("MORSAK",1:10))
for (k in 1:length(dh_v)){
	dat <- ut_dors[ut_dors[,dh_v[k]]!="", c("LopNr", "DODSDAT", "ICD", dh_v[k])]
	colnames(dat) <- c("ID", "EVENT_DATE", "ICD_VER", "ICD_CODE")
	dat[,"EVENT_DATE"] <- gsub("-", '', dat[ ,"EVENT_DATE"])
	dat[,"EVENT_FILE"] <- "DEATH"
	dat[,"EVENT_VAR"] <- dh_v[k]	
	dat[,"PALA"] <- NA	
		
	if (k==1){
		DEATH_long <- dat
	} else{
		DEATH_long <- rbind(DEATH_long, dat)
	}
	
	print(paste0("Diagnose ", k, "/", length(dh_v), " done, including ", nrow(dat), " records."))

}

DEATH_long <- DEATH_long[,v_long]
DEATH_long <- qc_icd(DEATH_long)
save(DEATH_long, file=paste0(r_dir, "DEATH_long.Rdata"))
[1] "Diagnose 1/11 done, including 466980 records."
[1] "Diagnose 2/11 done, including 362522 records."
[1] "Diagnose 3/11 done, including 262459 records."
[1] "Diagnose 4/11 done, including 152146 records."
[1] "Diagnose 5/11 done, including 89067 records."
[1] "Diagnose 6/11 done, including 50099 records."
[1] "Diagnose 7/11 done, including 28008 records."
[1] "Diagnose 8/11 done, including 15441 records."
[1] "Diagnose 9/11 done, including 8548 records."
[1] "Diagnose 10/11 done, including 4619 records."
[1] "Diagnose 11/11 done, including 2495 records."
[1] "Done for DEATH Registry, with 1442384 rows in total"

######


DEATH_lf <- icd_freq(DEATH_long, "DEATH")  # "11095 ICD codes in total."
table(DEATH_lf$ICD_VER, nchar(DEATH_lf$ICD_CODE))
 
dim(DEATH_lf[DEATH_lf$ICD_nchar>=5 & DEATH_lf$ICD_VER=="8", ])
DEATH_lf_85 <- DEATH_lf_85[order(DEATH_lf_85$ICD_CODE),]
dim(DEATH_lf_85)
table(substr(DEATH_lf_85$ICD_CODE,5,5))


#----------------------------------------
## Convert from ICD codes to Endpoints

DEATH_lf <- icd_freq(DEATH_long, "DEATH")  # "6811 ICD codes in total."
##   DEATH_lf <- read.table("DEATH_lf", header=T)
DEATH_lf[] <- lapply(DEATH_lf, as.character)
##   huff <- read.table("huff_all.lst", header=T)
huff[] <- lapply(huff, as.character)
huff$ICD_VER <- ifelse(huff$ICD_VER=="9","9_H",huff$ICD_VER)
huff$ICD_VER <- ifelse(huff$ICD_VER=="9_D","9",huff$ICD_VER)


## whether ICD codes in DEATH registry are what we asked (huff)

for (i in c(8:10)){
	print(i)
	huff_i <- huff[huff$ICD_VER==i,]
	DEATH_lf_i <- DEATH_lf[DEATH_lf$ICD_VER==i,]
	
	for (k in 3:5){
		print(k)
		DEATH_lf_i[,paste0("ICD_p",k,"_yn")] <- DEATH_lf_i[,paste0("ICD_p",k)] %in% huff_i$ICD_CODE
	}
		
	if(i==8){DEATH_lfa <- DEATH_lf_i} else {DEATH_lfa <- rbind(DEATH_lfa, DEATH_lf_i)}
	
}

DEATH_lfa$ICD_p345_yn <- rowSums(DEATH_lfa[,paste0("ICD_p",3:5,"_yn")])

table(DEATH_lfa$ICD_p345_yn)
#   0    1    2    3 
#6106 3402  473    1 

table(DEATH_lfa[DEATH_lfa$ICD_p345_yn!=0,"ICD_VER"])  # many of them are external (category Y & Z)
#  10    8    9 
#2281  547 1048 

table(nchar(DEATH_lfa[DEATH_lfa$ICD_p345_yn!=0 & DEATH_lfa$ICD_VER==9, "ICD_CODE"]))  # many of them are external (category Y & Z)
#  3   4 
# 56 992

table(DEATH_lfa[DEATH_lfa$ICD_p345_yn==0,"ICD_VER"])  # many of them are external (category Y & Z)
#  10    8    9 
#2690 1152 2264 



write.table(DEATH_lfa, "DEATH_lfa.lst", append=F, quote=F, sep=" ", row.names=F, col.names=T)


## count of required icd code (huff) in HILMO registry

for (k in 3:5){
	print(k)	
	DEATH_lfa_k <- DEATH_lfa[DEATH_lfa[,paste0("ICD_p",k,"_yn")]==T, c("ICD_CODE","ICD_VER",paste0("ICD_p",k),"count_DEATH")]	
	colnames(DEATH_lfa_k) <- c("ICD_CODE","ICD_VER","ICD_CODE_F","count_DEATH")
	
	if(k==3){DEATH_freq <- DEATH_lfa_k} else {DEATH_freq <- rbind(DEATH_freq, DEATH_lfa_k)}
}

DEATH_freq <- unique(DEATH_freq)
DEATH_freq$count_DEATH <- as.numeric(DEATH_freq$count_DEATH)	
DEATH_freq$n <- paste0(DEATH_freq$ICD_VER, "_", DEATH_freq$ICD_CODE)
DEATH_freq$nf <- paste0(DEATH_freq$ICD_VER, "_", DEATH_freq$ICD_CODE_F)
dim(DEATH_freq)

DEATH_freq_icdf <- aggregate(count_DEATH ~ ICD_VER + ICD_CODE_F, data=DEATH_freq, sum)
DEATH_freq_icdf[,"n"] <- paste0(DEATH_freq_icdf$ICD_VER, "_", DEATH_freq_icdf$ICD_CODE_F)

huff$n <- paste0(huff$ICD_VER, "_", huff$ICD_CODE)
huff_u <- unique(huff[,c("n", "ICD_CODE", "ICD_VER")])

DEATH_ask <- merge(huff_u, DEATH_freq_icdf[,c("n","count_DEATH")], by="n", all.x=T)[,c("n","ICD_VER","ICD_CODE","count_DEATH")]
colnames(DEATH_ask) <- c("n","ICD_VER","ICD_CODE","n_consider_subcat")
DEATH_ask <- merge(DEATH_ask, unique(DEATH_freq[,c("n","count_DEATH")]), by="n", all.x=T)[,-1]
colnames(DEATH_ask) <- c("ICD_VER", "ICD_CODE", "n_consider_subcat", "n_exact_match")
DEATH_ask[is.na(DEATH_ask)] <- 0    


table(nchar(DEATH_ask[DEATH_ask$n_exact_match!=0 & DEATH_ask$ICD_VER=="8", "ICD_CODE"]))
#   5 
# 104 

table(nchar(DEATH_ask[DEATH_ask$n_exact_match!=0 & DEATH_ask$ICD_VER=="9", "ICD_CODE"]))
#  3   4 
# 56 618 
 
table(nchar(DEATH_ask[DEATH_ask$n_exact_match!=0 & DEATH_ask$ICD_VER=="10", "ICD_CODE"]))
#  3   4 
# 90 756 


write.table(DEATH_ask, "DEATH_ask.tsv", append=F, quote=F, sep=" ", row.names=F, col.names=T)


# list of ENDPOINT and ICD_VER & original ICD_CODE for converting phenotypes
DEATH_endpoint <- merge(huff[,c("ENDPOINT","n")], DEATH_freq, by.x="n", by.y="nf")[,c("ICD_VER", "ICD_CODE", "ICD_CODE_F", "count_DEATH", "ENDPOINT")]
write.table(DEATH_endpoint, "DEATH_endpoint.tsv", append=F, quote=F, sep=" ", row.names=F, col.names=T)


##
ICD_10_1 <- unique(substr(DEATH_endpoint[DEATH_endpoint$ICD_VER==10,"ICD_CODE"],1,1))

for (i in 8:10){ 
	print(i)
	
	if (i!=10){
		DEATH_l_i <- DEATH_long[DEATH_long$ICD_VER==i, ]
		print(nrow(DEATH_l_i))   
	
		h_ep_i <- DEATH_endpoint[DEATH_endpoint$ICD_VER==i, c("ICD_CODE","ENDPOINT")]
		print(nrow(h_ep_i)) 	
	
		DEATH_l_io <- merge(DEATH_l_i, h_ep_i, by="ICD_CODE")	
		print(nrow(DEATH_l_io)) 
		
	} else{
	
		for (n in 1:length(ICD_10_1)){
			print(n)
			
			DEATH_l_i_n <- DEATH_long[DEATH_long$ICD_VER==i & substr(DEATH_long$ICD_CODE,1,1)==ICD_10_1[n], ]
			print(nrow(DEATH_l_i_n))
						
			h_ep_i_n <- DEATH_endpoint[DEATH_endpoint$ICD_VER==i & substr(DEATH_endpoint$ICD_CODE,1,1)==ICD_10_1[n], c("ICD_CODE","ENDPOINT")]
			print(nrow(h_ep_i_n))
			
			DEATH_l_i_no_n <- merge(DEATH_l_i_n, h_ep_i_n, by="ICD_CODE")	
			print(nrow(DEATH_l_i_no_n)) 
			
			if(n==1){DEATH_l_io <- DEATH_l_i_no_n} else {DEATH_l_io <- rbind(DEATH_l_io, DEATH_l_i_no_n)}
			
		}
		
	}
	
	if(i==8){DEATH_long_ep <- DEATH_l_io} else {DEATH_long_ep <- rbind(DEATH_long_ep, DEATH_l_io)}
}

dim(DEATH_long_ep)  # 902,106
save(DEATH_long_ep, file=paste0(r_dir,"DEATH_long_ep.Rdata"))

table(DEATH_long_ep$ICD_VER)
#     8      9     10 
#139543 239627 522936 


length(unique(DEATH_long_ep$ENDPOINT))   # 1,202
length(unique(DEATH_long_ep$ID))         # 230,627

table(nchar(DEATH_long_ep$EVENT_DATE))
#        8 
# 902106 



## Extract the first diagnose for each endpoint for each
DEATH_long_b <- DEATH_long_ep[ , c("ID", "EVENT_DATE", "ENDPOINT")]

DEATH_long_b$ENDPOINT <- as.character(DEATH_long_b$ENDPOINT)
dup <- duplicated(DEATH_long_b)
DEATH_long_b <- DEATH_long_b[!dup, ]
nrow(DEATH_long_b)    # 658885
DEATH_long_b <- DEATH_long_b[order(DEATH_long_b$ID, DEATH_long_b$ENDPOINT, DEATH_long_b$EVENT_DATE), ]
save(DEATH_long_b, file=paste0(r_dir, "DEATH_long_b.Rdata"))  


DEATH_first <- aggregate(EVENT_DATE ~ ID+ENDPOINT, data=DEATH_long_b, FUN=function(x) c(EVENT_F_DATE = head(x, n=1), EVENT_N = length(x)) )
DEATH_first <- as.matrix(DEATH_first)
dim(DEATH_first)     # 658885
colnames(DEATH_first) <- c("ID", "ENDPOINT", "EVENT_F_DATE", "EVENT_N")
DEATH_first <- gsub("[[:space:]]", "", DEATH_first)
DEATH_first <- as.data.frame(DEATH_first)
DEATH_first[] <- lapply(DEATH_first, as.character)
DEATH_first[,"EVENT"] <- paste(DEATH_first[,"ID"], DEATH_first[,"ENDPOINT"], sep="_")
save(DEATH_first, file=paste0(r_dir, "DEATH_first.Rdata"))  
print(paste0("Done for DEATH Registry, with ", nrow(DEATH_long)," rows in total"))  # "Done for DEATH Registry, with 1,442,382 rows in total"

index <- get(load(paste0(r_dir, "tove_lev_index.Rdata")))      
index_lst <- unique(index$LopNr); length(index_lst)       # 2,893,654

DEATH_first_index <- DEATH_first[DEATH_first[ ,"ID"] %in% index_lst, ]
nrow(DEATH_first_index)       # 124,576
length(unique((DEATH_first_index[ , "ID"])))         # 47,085
length(unique((DEATH_first_index[ , "ENDPOINT"])))   # 972
save(DEATH_first_index, file=paste0(r_dir, "DEATH_first_index.Rdata")) 


d_ep_f <- as.data.frame(table(DEATH_first[ ,"ENDPOINT"]))
colnames(d_ep_f) <- c("ENDPOINT","count")
d_ep_f <- d_ep_f[order(-d_ep_f$count), ]


d_ep_f_index <- as.data.frame(table(DEATH_first_index[ ,"ENDPOINT"]))
colnames(d_ep_f_index) <- c("ENDPOINT","count")
d_ep_f_index <- d_ep_f_index[order(-d_ep_f_index$count), ]

d_endpoint_f <- merge(d_ep_f, d_ep_f_index, by="ENDPOINT", all=T)
d_endpoint_f[is.na(d_endpoint_f)] <- 0
colnames(d_endpoint_f) <- c("ENDPOINT", "COUNT_FULL_CASES", "COUNT_INDEX_CASES")

d_endpoint_f <- merge(d_endpoint_f, huf, by="ENDPOINT")
d_endpoint_f <- data.frame(lapply(d_endpoint_f, as.character), stringsAsFactors=FALSE)
d_endpoint_f <- d_endpoint_f[order(-as.numeric(d_endpoint_f$COUNT_INDEX_CASES)), ]
d_endpoint_f[ ,"RANK"] <- 1:nrow(d_endpoint_f)
d_endpoint_f$prevalence_index <- round(as.numeric(d_endpoint_f[,"COUNT_INDEX_CASES"])/length(index_lst),4)

write.table(d_endpoint_f, "d_endpoint_f.tsv", append=F, quote=F, sep="\t", row.names=F, col.names=T)


## combine DEATH_first with HILMO_first
DEATH_first_add <- DEATH_first[DEATH_first$EVENT %!in% HILMO_first$EVENT, ]
DEATH_first_add[, "EVENT_FILE"] <- "DEATH"
HILMO_first[, "EVENT_FILE"] <- "HILMO"

ry_first <- rbind(HILMO_first, DEATH_first_add)
save(ry_first, file=paste0(r_dir, "ry_first.Rdata"))



##
index <- get(load(paste0(r_dir, "tove_lev_index.Rdata")))      
index_lst <- unique(index$LopNr); length(index_lst)       # 2,893,654


ry_first_index <- ry_first[ry_first[ ,"ID"] %in% index_lst, ]
nrow(ry_first_index)       # 124,576
length(unique((ry_first_index[ , "ID"])))         # 47,085
length(unique((ry_first_index[ , "ENDPOINT"])))   # 972
save(ry_first_index, file=paste0(r_dir, "ry_first_index.Rdata")) 




ep_f <- as.data.frame(table(ry_first[ ,"ENDPOINT"]))
colnames(d_ep_f) <- c("ENDPOINT","count")
ep_f <- d_ep_f[order(-d_ep_f$count), ]


ep_f_index <- as.data.frame(table(ry_first_index[ ,"ENDPOINT"]))
colnames(ep_f_index) <- c("ENDPOINT","count")
ep_f_index <- ep_f_index[order(-ep_f_index$count), ]

endpoint_f <- merge(ep_f, ep_f_index, by="ENDPOINT", all=T)
endpoint_f[is.na(endpoint_f)] <- 0
colnames(endpoint_f) <- c("ENDPOINT", "COUNT_FULL_CASES", "COUNT_INDEX_CASES")

endpoint_f <- merge(endpoint_f, huf, by="ENDPOINT")
endpoint_f <- data.frame(lapply(endpoint_f, as.character), stringsAsFactors=FALSE)
endpoint_f <- endpoint_f[order(-as.numeric(endpoint_f$COUNT_INDEX_CASES)), ]
endpoint_f[ ,"RANK"] <- 1:nrow(endpoint_f)
endpoint_f$prevalence_index <- round(as.numeric(endpoint_f[,"COUNT_INDEX_CASES"])/length(index_lst),4)

write.table(endpoint_f, "endpoint_all_f.tsv", append=F, quote=F, sep="\t", row.names=F, col.names=T)





