## This script is to calculate LRS (N of children) and age of having the first/last child for each sibling.


# Input: "tove_lev_koppl_index_syskon.Rdata", "tove_lev_koppl_index_sysbarn.Rdata"
# Output: "sib_uniq.Rdata", "sib_sibchild_uniq.Rdata", "Sib_indexW.Rdata",
, "index_lrs_summary", "index_lrs_count_summary", "index_age_at_having_child_count", "index_age_at_having_child_summary"
# Comments: 


setwd("/home/aoxing/DSGE_LRS/out/registry_edit/")
r_dir <- "/home/aoxing/DSGE_LRS/input/r_files/"




############################################
#             Read in data                 #
############################################

# sib
sib <- get(load(paste0(r_dir, "tove_lev_koppl_index_syskon.Rdata")))       
nrow(sib)     # 6,023,984
sib <- sib[, c("LopNrSyskon","SyskonFodelseAr","SyskonFodelseLan","SyskonFodelseKommun","SyskonKon")] 
dup <- duplicated(sib[,c("LopNrSyskon")])
sib_uniq <- sib[!dup, ]
nrow(sib_uniq)                        # 3,397,292
table(sib$SyskonTyp)                  # sbling_type, XXX HalvhelsyskonFar(paternal half-sib, mother missing), XXX HalvhelsyskonMor(maternal half-sib, father missing), XXX HalvsyskonFar(paternal half-sib), XXX HalvsyskonMor(maternal half-sib), XXX Helsyskon(full-sib)                                     
table(sib_uniq$SyskonKon)             # 1,186,320 with 1 and 1,120,904 with 2
save(sib_uniq, file=paste0(r_dir, "sib_uniq.Rdata"))    


# sib's children
sibchild <- get(load(paste0(r_dir, "tove_lev_koppl_index_sysbarn.Rdata"))) 
nrow(sibchild)                                        # 10,081,702 indexperson-sibling-siblingchildren combinations
length(unique(sibchild$LopNr))                        # 2,356,638 indexperson with nephew (sibing with children)
length(unique(sibchild$LopNrSyskon))                  # 2,493,352 sibling with children
length(unique(sibchild$LopNrSyskonBarn))              # 3,658,399 sibling's children 

sibchild_bas <- sibchild[ ,c("LopNrSyskon","LopNrSyskonBarn","SyskonBarnFodelseAr","SyskonBarnFodelseLan","SyskonBarnFodelseKommun","SyskonBarnKon")]
dup <- duplicated(sibchild_bas[ ,c("LopNrSyskon","LopNrSyskonBarn")])
sib_sibchild_uniq <- sibchild_bas[!dup, ]
nrow(sib_sibchild_uniq)                               # 5,540,829
save(sib_sibchild_uniq, file=paste0(r_dir, "sib_sibchild_uniq.Rdata")) 



###########################
#   LRS (N of children)   #
###########################

# n_child for each sibling 

# count from the data
sib_lrs <- as.data.frame(table(sib_sibchild_uniq$LopNrSyskon))    
colnames(sib_lrs) <- c("LopNrSyskon","n_child")    
nrow(sib_lrs)                                           # 2,493,352
summary(sib_lrs$n_child)                                # mean=2.222, max=23, for sibling with children, the same as "AntalSyskonBarn"


# add those without children
sib_lrs_all <- merge(sib_uniq, sib_lrs, by="LopNrSyskon", all=T)   
nrow(sib_lrs_all)        # 3,397,292
sib_lrs_all[is.na(sib_lrs_all)] <- 0 
summary(sib_lrs_all$n_child)   # mean=XXX, max=23, for all siblings    
sib_lrs_all[, "childless"] <- ifelse(sib_lrs_all$n_child!=0,0,1)        # 1 for childless 



####################################
#  Age of having first/last child  #
####################################

## age at first/last delivery  
sibchild_bas <- sib_sibchild_uniq[order(sib_sibchild_uniq[,"LopNrSyskon"],sib_sibchild_uniq[,"SyskonBarnFodelseAr"]), c("LopNrSyskon", "LopNrSyskonBarn", "SyskonBarnFodelseAr")]  


# first child 
sibchild_f_lst <- by(sibchild_bas, sibchild_bas["LopNrSyskon"], head, n=1)                             
sibchild_f <- do.call("rbind", sibchild_f_lst)                                               


# last child
sibchild_l_lst <- by(sibchild_bas, sibchild_bas["LopNrSyskon"], tail, n=1)           
sibchild_l <- do.call("rbind", sibchild_l_lst) 


# combine
sibchild_fl <- merge(sibchild_f, sibchild_l, by="LopNrSyskon")
colnames(sibchild_fl) <- c("LopNrSyskon","child_f_LopNr","bf_year","child_l_LopNr","bl_year")

sib_delivery <- merge(sib_lrs_all, sibchild_fl, by="LopNrSyskon", all.x=T)
nrow(sib_delivery)     # 3,397,292

sib_delivery$afc <- as.numeric(sib_delivery$bf_year) - as.numeric(sib_delivery$SyskonFodelseAr)
sib_delivery$alc <- as.numeric(sib_delivery$bl_year) - as.numeric(sib_delivery$SyskonFodelseAr)

sib_delivery <- sib_delivery[,c("LopNrSyskon", "SyskonFodelseAr", "SyskonKon", "n_child", "childless","afc","alc")]
save(sib_delivery, file=paste0(r_dir, "Sib_delivery.Rdata"))
sib_delivery <- sib_delivery[sib_delivery$afc >10 & sib_delivery$alc >10, ]   # 0 was removed due to age_at first/last child younger than 10   
nrow(sib_delivery)     # 3,397,292





############################################
#      Distribution of LRS                 #
############################################

# n_child by gender and birth_year
years <- sort(unique(sib_lrs_all[ ,"SyskonFodelseAr"]))  # from 1932 to 2018
kons <- c("male","female")

sib_lrs_summary <- matrix(NA, ncol=7, nrow=length(years))      # summary of n_child for each birth_year for male and female separately for siblings
colnames(sib_lrs_summary) <- c("b_year", paste0("n_",kons), paste0(kons,"_n_child"), paste0(kons,"_n_child_max"))


for (year_n in 1:length(years)){
	sib_lrs_summary[year_n,"b_year"] <- years[year_n]
	sib_lrs_yearn <- sib_lrs_all[sib_lrs_all[,"SyskonFodelseAr"]==years[year_n], c("SyskonKon","n_child")]
	
	for (kon_n in 1:length(kons)){
		sib_lrs_kon <- sib_lrs_yearn[sib_lrs_yearn$SyskonKon==kon_n, ]
		sib_lrs_summary[year_n,paste0("n_",kons[kon_n])] <- nrow(sib_lrs_kon)
		sib_lrs_summary[year_n,paste0(kons[kon_n],"_n_child")] <- ifelse(nrow(sib_lrs_kon)>0, round(mean(sib_lrs_kon$n_child), 3), NA)  	
		sib_lrs_summary[year_n,paste0(kons[kon_n],"_n_child_max")] <- ifelse(nrow(sib_lrs_kon)>0, max(sib_lrs_kon$n_child), NA)
		print(paste(year_n, kon_n, sep="_"))
	}
}    
write.table(sib_lrs_summary, "sib_lrs_summary", append=F, quote=F, sep=" ", row.names=F, col.names=T)



# count of age at first/last delivery
for (kon_n in 1:2){
	for (ac in c("afc","alc")){
		print(paste(kon_n,ac,sep="_"))
		print(table(sib_delivery[sib_delivery$SyskonKon==kon_n, ac]))  
	}
}
# age_first(last)_delivery is 14-69(14-75) for male and 13-55(13-60) for female



