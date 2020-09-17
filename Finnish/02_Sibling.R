
## 1. This script is to calculate LRS (N of children) and age of having the first/last child for each sibling.


setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
in_dir <- "/homes/aliu/DSGE_LRS/input/"
r_dir <- paste0(in_dir, "r_files/")



###########################
#   LRS (N of children)   #
###########################

## 2.1.1 Read in data 

# sib
sib <- get(load(paste0(r_dir,"sib.Rdata")))

nrow(sib)        # 8,066,956
length(unique(sib$KANTAHENKILON_TNRO))    # 1,819,317 indexperson with sibling
length(unique(sib$SUKULAISEN_TNRO))       # 2,307,224 siblings 
length(unique(sib$VALIHENKILO1_TNRO))     # 1,600,825, VALIHENKILO1_TNRO is shared father/mother between indexperson-sibling 
unique(sib$VALIHENKILO2_TNRO)             # NULL

sib <- within(sib, rm("KANTAHENKILON_TNRO","SUKUL_SUHDE","VALIHENKILO1_TNRO","VALIHENKILO2_TNRO"))
dup <- duplicated(sib[,c("SUKULAISEN_TNRO")])
sib_uniq <- sib[!dup, ]
nrow(sib_uniq)              # 2,307,224
table(sib_uniq$SUKUPUOLI)   # 1,186,320 with 1 and 1,120,904 with 2
save(sib_uniq, file=paste0(r_dir,"sib_uniq.Rdata"))    # only keep useful variables for unique sib



# sib's children
sibchild <- get(load(paste0(r_dir,"sibchild.Rdata")))

nrow(sibchild)          # 14,486,780
length(unique(sibchild$KANTAHENKILON_TNRO))   # 1,521,419 index person have sibling's with children
length(unique(sibchild$VALIHENKILO1_TNRO))    # 1,397,102 VALIHENKILO1_TNRO is shared father/mother between indexperson-sibling
length(unique(sibchild$VALIHENKILO2_TNRO))    # 1,597,982 VALIHENKILO2_TNRO is sibling
length(unique(sibchild$SUKULAISEN_TNRO))      # 2,392,471 sibling's children 

sibchild_bas <- within(sibchild, rm("KANTAHENKILON_TNRO","SUKUL_SUHDE","VALIHENKILO1_TNRO"))
dup <- duplicated(sibchild_bas[,c("VALIHENKILO2_TNRO","SUKULAISEN_TNRO")])
sibchild_uniq <- sibchild_bas[!dup, ]
nrow(sibchild_uniq)      # 3,687,737 sib-child pairs                        
save(sibchild_uniq, file=paste0(r_dir,"sibchild_uniq.Rdata"))      # only keep useful variables for unique sib-sibchild pairs



#--------------------------------------------
## 2.1.2 n_child for each sibling 

sib_lrs <- as.data.frame(table(sibchild_uniq$VALIHENKILO2_TNRO))    
colnames(sib_lrs) <- c("SUKULAISEN_TNRO","n_child") 
nrow(sib_lrs)                                           # 1,597,982   
summary(sib_lrs$n_child)                                # mean=2.308, max=19, for sibling with children


# add those without children
sib_lrs_all <- merge(sib_uniq, sib_lrs, by="SUKULAISEN_TNRO", all=T)    # add number of children for each sibling
nrow(sib_lrs_all)        # 2,307,224
sib_lrs_all[is.na(sib_lrs_all)] <- 0 
summary(sib_lrs_all$n_child)      # mean=1.598, max=19, for all siblings 
sib_lrs_all[,"b_year"] <- substr(sib_lrs_all$SUKULAISEN_SYNTYMAPV,1,4)
save(sib_lrs_all, file=paste0(r_dir,"sib_lrs_all.Rdata"))


# distribution 
years <- sort(unique(sib_lrs_all[,"b_year"]))  # from 1932 to 2018
n_year <- length(years)                                 # 87 years in total

sib_lrs_summary <- matrix(NA,ncol=7,nrow=n_year)                     # summary of n_child for each birth_year for male and female separately for siblings
colnames(sib_lrs_summary) <- c("birth_year","n_male","n_female","male_n_child","female_n_child","male_n_child_max","female_n_child_max")
kons <- c("male","female")


for (year_n in 1:n_year){
	sib_lrs_summary[year_n,"birth_year"] <- years[year_n]
	sib_lrs_yearn <- sib_lrs_all[sib_lrs_all[,"b_year"]==years[year_n], c("SUKUPUOLI","n_child")]	
	for (kon_n in 1:length(kons)){
		sib_lrs_kon <- sib_lrs_yearn[sib_lrs_yearn$SUKUPUOLI == kon_n, ]
		sib_lrs_summary[year_n,paste0("n_",kons[kon_n])] <- nrow(sib_lrs_kon)
		sib_lrs_summary[year_n,paste0(kons[kon_n],"_n_child")] <- ifelse(nrow(sib_lrs_kon)>0, round(mean(sib_lrs_kon$n_child), 3), NA)  	
		sib_lrs_summary[year_n,paste0(kons[kon_n],"_n_child_max")] <- ifelse(nrow(sib_lrs_kon)>0, max(sib_lrs_kon$n_child), NA)
		print(paste(year_n, kon_n, sep="_"))
	}	
}    

write.table(sib_lrs_summary, "sib_lrs_summary", append=F, quote=F, sep=" ", row.names=F, col.names=T)




####################################
#  Age of having first/last child  #
####################################

## 2.2.1 age at first/last delivery  
sibchild_uniq[,"b_year"] <- substr(sibchild_uniq$SUKULAISEN_SYNTYMAPV,1,4)
sibchild_bas <- sibchild_uniq[order(sibchild_uniq$VALIHENKILO2_TNRO,sibchild_uniq$SUKULAISEN_SYNTYMAPV), c("VALIHENKILO2_TNRO", "SUKULAISEN_TNRO", "b_year")]     


# first child 
sibchild_f_lst <- by(sibchild_bas, sibchild_bas$VALIHENKILO2_TNRO, head, n=1)                                    
sibchild_f <- do.call("rbind", sibchild_f_lst)    # Reduce is 10 time slower than do.call, do.call is 10 times slower than rebindlist


# last child
sibchild_l_lst <- by(sibchild_bas, sibchild_bas["VALIHENKILO2_TNRO"], tail, n=1)                
sibchild_l <- do.call("rbind",sibchild_l_lst) 


# combine
sibchild_fl <- merge(sibchild_f, sibchild_l, by="VALIHENKILO2_TNRO")
colnames(sibchild_fl) <- c("VALIHENKILO2_TNRO","child_f_TNRO","bf_year","child_l_TNRO","bl_year")

sib_del <- merge(sib_lrs_all, sibchild_fl, by.x="SUKULAISEN_TNRO",by.y="VALIHENKILO2_TNRO")
nrow(sib_del)     # 1,597,982

sib_del$afc <- as.numeric(sib_del$bf_year) - as.numeric(sib_del$b_year)
sib_del$alc <- as.numeric(sib_del$bl_year) - as.numeric(sib_del$b_year)

sib_del <- sib_del[sib_del$afc >10 & sib_del$alc >10, ]  # remove age_at first/last child younger than 10   
save(sib_del, file=paste0(r_dir,"sib_afc_alc.Rdata"))  
nrow(sib_del)     # 1,597,974


#-----------------------------------------------------------------------------------------
## 2.2.2 distribution of age at first/last delivery

for (kon_n in 1:2){
	for (ac in c("afc","alc")){
		print(paste(kon_n,ac,sep="_"))
		print(table(sib_del[sib_del$SUKUPUOLI==kon_n, ac]))  
	}
}

# age_first(last)_delivery is 9-73(14-73) for male and 13-53(14-56) for female






