## 12. Convert from ICD codes to endpoints using HILMO, CANCER, and DEATH registry


setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"

library(ggplot2, lib.loc="/homes/aliu/anaconda3/lib/R/library")
library(tidyverse)
'%!in%' <- function(x,y)!('%in%'(x,y))




################################################
#         Extract Longtitudinal HILMO          #
################################################

# Split registry from 96 to 2018 into inpatient and outpatient
h9618 <- get(load(paste0(r_dir,"thl2019_804_hilmo_9618.Rdata")))	

for (i in c("inpatient","outpatient")){
	print(paste0("Start to extract ", i, " for h9618."))
	
	if (i=="inpatient"){
		dat <- h9618[as.numeric(h9618$PALA)<10,]
	}else{
		dat <- h9618[as.numeric(h9618$PALA)>10,]
	}
	save(dat, file=paste0(r_dir, "thl2019_804_hilmo_9618_", i, ".Rdata"))
	print(paste0("Done to extract ", i, " for h9618, with ", nrow(dat), " rows"))
}


# set parameters
efiles <- c("poisto_6986", "poisto_8793", "hilmo_9495", "hilmo_9618_inpatient", "hilmo_9618_outpatient")
vars <- c("DG1,DG2,DG3,DG4", "PDG,SDG1,SDG2,SDG3", "PDG,SDG1,TUTAP", rep(apply(as.matrix(c(paste0(rep("ICD10",20),rep(c("E_","O_"),each=10),1:10),paste0(rep("PITKADIAG",36),rep(c("E_","O_"),each=18),1:18))),2,paste, collapse=","),2))   # "TUTAP"="SDG2"
palas <- c(rep("inpatient",4),"outpatient")
icdvs <- c("8","9","9","10","10")
dates <- c("TULOPV",rep("TUPVA",4))
hos <- c("SAIR", rep("PALTU",4))
mus <- c("KNT", rep("KOKU",4))


print("Start for HILMO Registry")
for (i in 1:length(efiles)){

	print(paste0("Start for ", efiles[i],"."))
	d <- as.data.frame(get(load(paste0(r_dir,"thl2019_804_", efiles[i], ".Rdata"))))
	v <- unlist(strsplit(vars[i],","))
		
	for (k in 1:length(v)){
		dat <- d[d[,v[k]]!="", c("TNRO", dates[i], hos[i], mus[i], v[k])]
		colnames(dat) <- c("ID", "EVENT_DATE", "hospital", "municipality", "ICD_CODE")
		dat[, "EVENT_DATE"] <- gsub("-", '', dat[ ,"EVENT_DATE"])
		dat[, "EVENT_FILE"] <- efiles[i]
		dat[, "EVENT_VAR"] <- v[k]
		dat[, "PALA"] <- palas[i]
		dat[, "ICD_VER"] <- icdvs[i]
		dat[, "morpho"] <- NA
		
		if (i==1 & k==1){
			HILMO_long <- dat
		} else{
			HILMO_long <- rbind(HILMO_long, dat)
		}
	
		print(paste0("Diagnose ", k, "/", length(v), " done, including ", nrow(dat), " records."))
	}
	
	print(paste0("Done for ", efiles[i],"."))
	
}

HILMO_long <- as.data.frame(HILMO_long)
save(HILMO_long, file=paste0(r_dir, "HILMO_long_all.Rdata"))
print(paste0("Done for HILMO Registry, with ", nrow(HILMO_long)," rows in total."))   




################################################
#               Extract  CANCER                #  # cancer were defined by both topo and morpho
################################################

fcr <- get(load(paste0(r_dir, "fcr.Rdata")))
fcr$icd10_topo <- sprintf("C%03d",fcr$topo)
fcr_v <- c("icd10_topo")

print("Start for CANCER Registry")
for (k in 1:length(fcr_v)){
	dat <- fcr[fcr[,fcr_v[k]]!="", c("TNRO", "dg_date", "morpho", fcr_v[k])]
	colnames(dat) <- c("ID", "EVENT_DATE",  "morpho", "ICD_CODE")
	dat[,"EVENT_DATE"] <- gsub("-", '', dat[ ,"EVENT_DATE"])
	dat[,"hospital"] <- NA
	dat[,"municipality"] <- NA
	dat[, "EVENT_FILE"] <- "CANCER"
	dat[, "EVENT_VAR"] <- fcr_v[k]	
	dat[, "PALA"] <- NA	
	dat[, "ICD_VER"] <- "10"
			
	print(paste0("Diagnose ", k, "/", length(fcr_v), " done, including ", nrow(dat), " records."))
	
	if (k==1){
		CANCER_long <- dat
	} else{
		CANCER_long <- rbind(CANCER_long, dat)
	}
	
}

CANCER_long <- CANCER_long[ ,colnames(HILMO_long)]
CANCER_long <- as.data.frame(CANCER_long)
save(CANCER_long, file=paste0(r_dir, "CANCER_long.Rdata"))
print(paste0("Done for CANCER Registry, with ", nrow(CANCER_long)," rows in total."))    # "Done for CANCER Registry, with 590,803 rows in total."






################################################
#               Extract  DEATH                 #
################################################

dh <- get(load(paste0(r_dir, "kuolemansyyt_u1477_a.Rdata")))
dh_v <- c("tpks", "vks", paste0("m",1:4))


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
		
	if (k==1){
		DEATH_long <- dat
	} else{
		DEATH_long <- rbind(DEATH_long, dat)
	}
	
	print(paste0("Diagnose ", k, "/", length(dh_v), " done, including ", nrow(dat), " records."))

}

DEATH_long <- DEATH_long[ ,colnames(HILMO_long)]
DEATH_long <- as.data.frame(DEATH_long)
save(DEATH_long, file=paste0(r_dir, "DEATH_long.Rdata"))
print(paste0("Done for DEATH Registry, with ", nrow(DEATH_long)," rows in total"))  # "Done for DEATH Registry, with 1,795,484 rows in total"






###############################################################
#  Combine and add endpoint to three registry (longitudinal)  #
###############################################################

rl <- rbind(HILMO_long, CANCER_long, DEATH_long)
nrow(rl)      # 54,554,742

## format ICD codes
rl[ ,"ICD_CODE"] <- gsub("-|\\+|\\?|\\*|\\?|\\&", '', rl[ ,"ICD_CODE"])    # remove unexpected characters i
rl <- rl[!( (rl$ICD_VER!="9" & substr(rl$ICD_CODE,5,5) %in% LETTERS)  |  (rl$ICD_VER=="9" & substr(rl$ICD_CODE,5,5) %in% 0:9) ), ]
rl <- rl[!( (rl$ICD_VER!="10" & substr(rl$ICD_CODE,1,1) %in% LETTERS[c(1:24,26)])  |  (rl$ICD_VER=="10" & substr(rl$ICD_CODE,1,1) %in% LETTERS[c(22:24,26)]) ), ] 
rl <- rl[!(nchar(rl$ICD_CODE)<3|nchar(rl$ICD_CODE)>6), ]
print(paste0("A total of ", nrow(rl), " records for ", length(unique(rl$ID)), " individuals"))     # "A total of 54,323,398 records for 4,051,467 individuals"
save(rl, file=paste0(r_dir, "rl.Rdata"))


## count of ICD codes (& morpho) by ICD version
rl_f <- as.data.frame(table(rl$ICD_CODE, rl$ICD_VER, rl$morpho, useNA="always"))
colnames(rl_f) <- c("ICD_CODE","ICD_VER","morpho","count")

rl_f[ ,"ICD_CODE"]<- as.character(rl_f[ ,"ICD_CODE"])
rl_f <- rl_f[rl_f$count>0, ]
rl_f <- rl_f[order(-rl_f$count), ]
nrow(rl_f)    # 29,054

for (k in 3:5){
	rl_f[ ,paste0("ICD_p",k)] <- ifelse(nchar(rl_f[ ,"ICD_CODE"])>=k, substr(rl_f[ ,"ICD_CODE"], 1, k), NA)
}



## List of ICD codes and the corresponding endpoints
huf <- read.table("/homes/aliu/DSGE_LRS/input/HILMO_UPDATED_FIN.lst", sep="\t", header=T)    # endpoints and corresponding icd8-10
head(huf)


for (i in 8:10){
	huf_i <- huf[huf[ ,paste0("ICD",i)]!="--", c("ENDPOINT", paste0("ICD",i), "morpho")]
	huf_i <- huf_i %>% separate_rows(paste0("ICD",i), sep=",")       # one ICD code per row
	colnames(huf_i) <- c("ENDPOINT", "ICD_CODE", "morpho")
	huf_i[,"ICD_VER"] <- i
	if(i==8){huff <- huf_i} else {huff <- rbind(huff, huf_i)}
	
	rl_fi <- rl_f[rl_f$ICD_VER==i, ]
	
	for (k in 5:3){
		rl_fik <- merge(rl_fi, huf_i[,c("ENDPOINT","ICD_CODE")], by.x=paste0("ICD_p",k), by.y="ICD_CODE", all.x=T)
		rl_fik <- rl_fik[ , c("ICD_CODE","ICD_VER","ENDPOINT")]
		
		if(i==8 & k==5){rl_fa <- rl_fik} else {rl_fa <- rbind(rl_fa, rl_fik)}
	}		
}


hucff <- huff[huff$morpho!="--" & huff$ICD_VER==10, ]
rl_fa <- unique(rl_fa[is.na(rl_fa$ENDPOINT)==F, ])
rl_fao <- rl_fa[rl_fa$ENDPOINT %!in% hucff$ENDPOINT, ]
rl_fao[ , "morpho"] <- "--"
rl_fao <- rl_fao[ , c("ICD_CODE", "ICD_VER", "morpho","ENDPOINT")]

rl_fac <- rl_fa[rl_fa$ENDPOINT %in% hucff$ENDPOINT & rl_fa$ICD_VER==10, ]
rl_fac <- merge(rl_fac, hucff[,c("ENDPOINT", "ICD_CODE", "morpho")], by="ENDPOINT")
rl_fac <- rl_fac[substr(rl_fac$ICD_CODE.x,1,3)==rl_fac$ICD_CODE.y, c("ICD_CODE.x", "ICD_VER", "morpho","ENDPOINT")]
colnames(rl_fac) <- c("ICD_CODE", "ICD_VER", "morpho","ENDPOINT")
ICD_ENDPOINT <- rbind(rl_fao, rl_fac)

write.table(huff, "huff_all.lst", append=F, quote=F, sep=" ", row.names=F, col.names=T)
write.table(ICD_ENDPOINT, "ICD_ENDPOINT_all.lst", append=F, quote=F, sep=" ", row.names=F, col.names=T)



## Add endpoints to longitudinal registry (full population)
# don't need morpho
for (i in 8:10){ 
	print(i)
	rl_i <- rl[rl$ICD_VER==i, ]
	print(nrow(rl_i))    # 1,472,302, 1,354,307, 47,014,912
	
	ICD_ENDPOINT_oi <- ICD_ENDPOINT[ICD_ENDPOINT$morpho=="--" & ICD_ENDPOINT$ICD_VER==i, c("ICD_CODE","ENDPOINT")]
	rl_io <- merge(rl, ICD_ENDPOINT_oi, by="ICD_CODE")	
	print(nrow(rl_io))   # 2080217, 2,101,764, 76596617
	
	if(i==8){rl_o <- rl_io} else {rl_o <- rbind(rl_o, rl_io)}
}
dim(rl_o)  # 89,692,733       11



# need morpho
ICD_ENDPOINT_c <- ICD_ENDPOINT[ICD_ENDPOINT$morpho!="--" & ICD_ENDPOINT$ICD_VER==10, ]
ICD_ENDPOINT_c[ ,"ICD_END"] <- paste(ICD_ENDPOINT_c[ ,"ICD_CODE"], ICD_ENDPOINT_c[ ,"morpho"], sep="_")   # create a variable for merge

rl_c <- rl[rl$morpho %in% ICD_ENDPOINT_c$morpho  &  rl$ICD_CODE %in% ICD_ENDPOINT_c$ICD_CODE, ]
rl_c[ ,"ICD_END"] <- paste(rl_c[ ,"ICD_CODE"], rl_c[ ,"morpho"], sep="_")   # create a variable for merge
dim(rl_c)     # 67814     11


rl_c <- merge(rl_c, ICD_ENDPOINT_c[ , c("ICD_END", "ENDPOINT")], by="ICD_END")
rl_c <- rl_c[, colnames(rl_o)]
dim(rl_c)     # 67200  11

ry_long <- rbind(rl_o, rl_c)
length(unique((ry_long$ENDPOINT)))   # 2,395
length(unique((ry_long$ID)))         # 4,031,894
ry_long$EVENT_DATE <- substr(ry_long$EVENT_DATE, 1, 8)
save(ry_long, file=paste0(r_dir, "ry_long_all.Rdata"))   # 89,759,933


## longitudinal for index person only 
index_lrs <- get(load(paste0(r_dir, "index_lrs_all.Rdata")))
nrow(index_lrs)           # 2,365,707


ry_long_index <- ry_long[ry_long[ ,"ID"] %in% index_lrs$KANTAHENKILON_TNRO, ]
nrow(ry_long_index)       #  24,607,766   ## 26,996,652
length(unique((ry_long_index[ , "ID"])))         # 1,306,244
length(unique((ry_long_index[ , "ENDPOINT"])))   # 2,339 
save(ry_long_index, file=paste0(r_dir, "ry_long_index_all.Rdata"))   






#################################################################
#   Extract frist diagnose for each endpoint from longitudinal  #
#################################################################

## Extract the first diagnose for each endpoint for each 
ry_long_b <- ry_long[ , c("ID", "EVENT_DATE", "ENDPOINT")]
ry_long_b$EVENT_DATE <- as.numeric(as.character(ry_long_b$EVENT_DATE))
ry_long_b$ENDPOINT <- as.character(ry_long_b$ENDPOINT)
ry_long_b <- ry_long_b[!duplicated(ry_long_b), ]
nrow(ry_long_b)    # 80,829,145
ry_long_b <- ry_long_b[order(ry_long_b$ID, ry_long_b$ENDPOINT, ry_long_b$EVENT_DATE), ]
save(ry_long_b, file=paste0(r_dir, "ry_long_b_all.Rdata"))  


ry_first <- aggregate(EVENT_DATE ~ ID+ENDPOINT, data=ry_long_b, FUN=function(x) c(EVENT_F_DATE = head(x, n=1), EVENT_N = length(x)) )
ry_first <- as.matrix(ry_first)
colnames(ry_first) <- c("ID", "ENDPOINT", "EVENT_F_DATE", "EVENT_N")
ry_first <- gsub("[[:space:]]", "", ry_first)
save(ry_first, file=paste0(r_dir, "ry_first_all.Rdata"))  



## Summary of ry_first for full population
nrow(ry_first)                              # 21,070,545
length(unique((ry_first[ , "ID"])))         # 4,031,894
length(unique((ry_first[ , "ENDPOINT"])))   # 2,395

ep_f <- as.data.frame(table(ry_first[ ,"ENDPOINT"]))
colnames(ep_f) <- c("ENDPOINT","count")
ep_f <- ep_f[order(-ep_f$count), ]



## Summary of ry_first for index person
ry_first_index <- ry_first[ry_first[ ,"ID"] %in% index_lrs$KANTAHENKILON_TNRO, ]
nrow(ry_first_index)       # 5,775,290
length(unique((ry_first_index[ , "ID"])))         # 1,306,241
length(unique((ry_first_index[ , "ENDPOINT"])))   # 2,339
save(ry_first_index, file=paste0(r_dir, "ry_first_index_all.Rdata")) 


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
write.table(endpoint_f, "endpoint_f_all.lst", append=F, quote=F, sep="\t", row.names=F, col.names=T)


for (k in 8:10){
	e_f_k <- endpoint_f[endpoint_f[ ,paste0("ICD",k)]!="--", c("ENDPOINT", paste0("ICD",k), "RANK")]
	e_f_k <- e_f_k %>% separate_rows(paste0("ICD",k), sep=",")       # one ICD code per row
	colnames(e_f_k) <- c("ENDPOINT", "ICD_CODE", "RANK")
	e_f_k[,"ICD_VER"] <- k
	if(k==8){e_f_a <- e_f_k} else {e_f_a <- rbind(e_f_a, e_f_k)}
}


for (i in 1:nrow(endpoint_f)){
	ck <- 0
	
	for (k in 8:10){
		icd_k <- unlist(strsplit(endpoint_f[i, paste0("ICD",k)],","))
		icd_k345 <- unique(c(icd_k, substr(icd_k,1,3), substr(icd_k,1,4)))
		
		if (length(intersect(icd_k345, e_f_a[e_f_a[ ,"ICD_VER"]==k & e_f_a[ ,"RANK"]<i, "ICD_CODE"]))==0 | endpoint_f[i, paste0("ICD",k)]=="--"){c <- 1} else{c <- 0}
		if (c==0){r <- which (e_f_a[e_f_a[ ,"ICD_VER"]==k, "ICD_CODE"] %in% icd_k345)[1]}
		ck <- ck + c
	}
	
	if (ck==3){
	
		if(as.numeric(endpoint_f[i, "COUNT_INDEX_CASES"]) >= 1000){
			endpoint_f[i, "exclude_reason"] <- "Keep"
		} else {
			endpoint_f[i, "exclude_reason"] <- "Less than 1000 cases in index person"
		}
		
	} else {
		endpoint_f[i, "exclude_reason"] <- paste0("Overlapped with ", e_f_a[e_f_a[ ,"ICD_VER"]==k, "ENDPOINT"][r], ", which has more cases")
	}
	
}

write.csv(endpoint_f, "endpoint_filter_all.csv")







########################
## copy to local
# scp   aliu@ssh.fimm.fi:/homes/aliu/DSGE_LRS/output/registry_edit/endpoint_filter_all.lst    /Users/aoxliu/Downloads/Files/
 
## format N cases provided by Vincent
# awk 'BEGIN {FS="},{";OFS="\n"}{$1=$1; print $0}' aoxing.json|grep '"sex":0'|awk 'BEGIN {FS=",";OFS="\t"}{$1=$1; print $3,$1,$2,$4,$5}'|sed -e "s/\"age\"://" -e "s/\"name\"://" -e "s/\"median_events\"://"  -e "s/\"nindivs\"://" -e "s/\"prevalence\"://"  -e "s/\"//"  -e "s/\"//"  -e "s/\[{//" > Finngen_endpoint_count_vincent
 
 

 

