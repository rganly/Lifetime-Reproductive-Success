## This script is to calculate LRS (N of children and N of grandchildren), childless, and age of having the first/last child for each index person.


# Input: "tove_lev_index.Rdata", "tove_lev_koppl_index_barn.Rdata", "tove_lev_koppl_index_barnbarn.Rdata"
# Output: "indexW.Rdata", "indexW_delivery.Rdata", "indexW_LRS.Rdata", "indexW_lrs_summary", "indexW_lrs_count_summary", "indexW_age_at_having_child_count", "indexW_age_at_having_child_summary"
# Comments: 


setwd("/home/aoxing/DSGE_LRS/out/registry_edit/")
r_dir <- "/home/aoxing/DSGE_LRS/input/r_files/"

'%!in%' <- function(x,y)!('%in%'(x,y))



############################################
#             Read in data                 #
############################################

# Index person (born 1956-1982)  
index <- data.frame(get(load(paste0(r_dir, "tove_lev_index.Rdata"))))    
nrow(index)   # 2,893,654


# Children of index person
child <- data.frame(get(load(paste0(r_dir, "tove_lev_koppl_index_barn.Rdata"))))     
length(unique(child[,"LopNr"]))                                  # 2,202,668 indexperson with children
length(unique(child[,"LopNrBarn"]))                              # 2,900,503 children


# Grandchildren of index person
grandchild <- data.frame(get(load(paste0(r_dir, "tove_lev_koppl_index_barnbarn.Rdata"))))  
nrow(grandchild)        # 1,241,918
length(unique(grandchild[,c("LopNr")]))                          # 476,252 indexperson with grandchildren
length(unique(grandchild[,c("LopNrBarn")]))                      # 450,005 children
length(unique(grandchild[,c("LopNrBarnBarn")]))                  # 591,843 grandchildren


# Migration
migra <- data.frame(get(load(paste0(r_dir,"tove_lev_migrationer.Rdata"))))
nrow(migra)  # 2,066,867


# Death
death <- data.frame(get(load(paste0(r_dir,"tove_lev_doddatum.Rdata"))))
nrow(death)  # 1,301,514




############################################
#         QC for index person              #
############################################

# birth year 1956-1982, born in Sweden, not imigrated/emigrated from Sweden, and alive until age 15
index <- merge(index, death, by="LopNr", all.x=T)
index$dead_age <- as.numeric(substr(index$DodDatum,1,4)) - as.numeric(index$FodelseAr)

indexW <- index[(index$LopNr %!in% unique(migra$LopNr)) & (index$dead_age>=15|is.na(index$dead_age)) & (index$AterPnr==0), c("LopNr","FodelseAr","FodelseLandNamn","FodelseLan","FodelseKommun","Kon","DodDatum")]
nrow(indexW)   # 2,555,541
table(indexW$Kon)                                                 # 1,318,132 males and 1,237,409 females 
sum(indexW$Kon==1)/sum(indexW$Kon==2)   # 1.065236
save(indexW, file=paste0(r_dir, "indexW.Rdata"))




############################################
#   LRS (N of children or grandchildren)   #
############################################

# n_child
index_lrs <- data.frame(table(child[,"LopNr"]))                 # n_child for indexperson with children
colnames(index_lrs) <- c("LopNr","n_child")                     # 2,202,668 indexperson with children
mean(index_lrs[,"n_child"])                                     # 2.217 for indexperson with children
table(index_lrs[,"n_child"])                                    # from 1 to 27


# n_child_Age4550
child_index <- merge(indexW[,c("LopNr","FodelseAr","Kon")], child, by="LopNr")
nrow(child_index)  # 4,576,536
child_index$age_of_child <- as.numeric(child_index$BarnFodelseAr) - as.numeric(child_index$FodelseAr)

child_index_m <- child_index[child_index$age_of_child<=50 & child_index$Kon==1,]
child_index_f <- child_index[child_index$age_of_child<=45 & child_index$Kon==2,]
nrow(child_index_m)  # 4,541,722
nrow(child_index_f)  # 4,541,722
index_lrs50 <- data.frame(table(child_index_m[,"LopNr"]))
index_lrs45 <- data.frame(table(child_index_f[,"LopNr"]))

index_lrs4550 <- rbind(index_lrs50, index_lrs45)
colnames(index_lrs4550) <- c("LopNr","n_child_Age4550")
nrow(index_lrs4550)  # 2,055,483


# n_grandchild
index_glrs <- data.frame(table(unique(grandchild[,c("LopNr","LopNrBarnBarn")])["LopNr"]))    # 476,252 indexperson with grandchildren
colnames(index_glrs) <- c("LopNr","n_gchild")
mean(index_glrs[,"n_gchild"])                                   # 2.608 for indexperson with grandchildren
table(index_glrs[,"n_gchild"])                                  # from 1 to 34


# combine
index_lrs_glrs <- merge(index_lrs, index_glrs, by="LopNr", all=T)  # 2,202,668, same as N of indexperson with children   
index_lrs_glrs$LopNr <- as.character(index_lrs_glrs$LopNr)     

lrs_all <- merge(index_lrs_glrs, indexW, by="LopNr", all.y=T)    # add indexperson without children
nrow(lrs_all)   # 2,555,541
lrs_all[is.na(lrs_all)] <- 0                                 

summary(lrs_all[lrs_all[,"Kon"]==1, c("n_child","n_gchild")])   # male, mean(n_child/n_grandchild) = 1.665/0.3581, max(n_child/n_grandchild) = 21/31
summary(lrs_all[lrs_all[,"Kon"]==2, c("n_child","n_gchild")])   # female, mean(n_child/n_grandchild) = 1.925/0.5899, max(n_child/n_grandchild) = 19/34
lrs_all[, "childless"] <- ifelse(lrs_all$n_child!=0,0,1)        # 1 for childless 




############################################
#     Age of having first/last child       #
############################################

## age at first/last delivery  
child_bas <- child[order(child[,"LopNr"],child[,"BarnFodelseAr"]), c("LopNr","LopNrBarn","BarnFodelseAr")]  

# first child 
child_f_lst <- by(child_bas, child_bas["LopNr"], head, n=1)                       
child_f <- do.call("rbind", child_f_lst)     


# last child
child_l_lst <- by(child_bas, child_bas["LopNr"], tail, n=1)   
child_l <- do.call("rbind", child_l_lst) 


# combine
child_fl <- merge(child_f, child_l, all=T, by="LopNr") 
colnames(child_fl) <- c("LopNr","child_f_LopNr","bf_year","child_l_LopNr","bl_year")

index_bas <- lrs_all[lrs_all[,"n_child"]!=0, c("LopNr","FodelseAr","Kon")]
index_delivery <- merge(index_bas, child_fl, by="LopNr")
nrow(index_delivery)     # 2,057,099

index_delivery$afc <- as.numeric(index_delivery$bf_year) - as.numeric(index_delivery$FodelseAr)
index_delivery$alc <- as.numeric(index_delivery$bl_year) - as.numeric(index_delivery$FodelseAr)
save(index_delivery, file=paste0(r_dir, "indexW_delivery.Rdata"))  

lrs_all <- merge(lrs_all, index_delivery[,c("LopNr","afc","alc")], by="LopNr", all.x=T)
nrow(lrs_all)   # 2,555,541
lrs_all <- merge(lrs_all, index_lrs4550, by="LopNr", all.x=T)
lrs_all$infertility <- NA
lrs_all$lamda <- NA
lrs_all <- lrs_all[,c("LopNr", "FodelseAr", "Kon", "n_child", "n_child_Age4550", "n_gchild", "childless", "infertility", "lamda", "afc","alc")]
lrs_all[is.na(lrs_all)] <- 0 
save(lrs_all, file=paste0(r_dir, "indexW_LRS.Rdata"))                             





############################################
#      Distribution of LRS                 #
############################################

# remove age_at first/last child younger than 10
index_delivery <- index_delivery[index_delivery$afc > 10 & index_delivery$alc > 10, ]     

# n_child and n_grandchild by gender and birth_year
years <- sort(unique(lrs_all[,"FodelseAr"]))
kons <- c("male","female")

lrs_summary <- matrix(NA, ncol=11, nrow=length(years))          # average of LRS   
colnames(lrs_summary) <- c("b_year",paste0("n_",kons),paste0(kons,"_n_child"),paste0(kons,"_n_gchild"),paste0(kons,"_n_child_max"),paste0(kons,"_n_gchild_max"))
lrs_count_summary <- matrix(NA, ncol=13, nrow=length(years))    # count of LRS
colnames(lrs_count_summary) <- c("b_year","n_male","n_female",paste("n_", rep(kons,each=5), "_", seq(0,4,1), "child", sep=""))

for (year_n in 1:length(years)){
	lrs_summary[year_n,"b_year"] <- lrs_count_summary[year_n,"b_year"] <- years[year_n]	
	for (kon_n in 1:length(kons)){	
		lrs_yearn_konn <- lrs_all[lrs_all[,"FodelseAr"]==years[year_n] & lrs_all[,"Kon"]==kon_n, c("Kon","n_child","n_gchild")]
		lrs_summary[year_n, paste("n_",kons[kon_n],sep="")] <- lrs_count_summary[year_n, paste("n_",kons[kon_n],sep="")] <- nrow(lrs_yearn_konn)		
		for (pop in c("child","gchild")){	
			lrs_summary[year_n, paste(kons[kon_n],"n",pop,sep="_")] <- round(mean(lrs_yearn_konn[, paste0("n_",pop)]), 3)  
			lrs_summary[year_n, paste(kons[kon_n],"n",pop,"max",sep="_")] <- max(lrs_yearn_konn[, paste0("n_",pop)])
		}	
		for (n in 0:4){	
			lrs_count_summary[year_n, paste0("n_",kons[kon_n],"_",n,"child")] <- nrow(lrs_yearn_konn[lrs_yearn_konn[,"n_child"]==n, ])
		}		
		print(paste(year_n, kon_n, sep="_"))
	}	
}

write.table(lrs_summary, "indexW_lrs_summary", append=F, quote=F, sep=" ", row.names=F, col.names=T)
write.table(lrs_count_summary, "indexW_lrs_count_summary", append=F, quote=F, sep=" ",row.names=F, col.names=T)



# count of age at first/last delivery for each gender
k <- 1
for (ac in c("afc","alc")){
	for (kon_n in 1:length(kons)){	
		summary(index_delivery[index_delivery[,"Kon"]==kon_n, ac])	
		age <- as.data.frame(table(index_delivery[index_delivery[,"Kon"]==kon_n, ac]))
		colnames(age) <- c("age", paste("count",ac,kons[kon_n],sep="_"))		
		if (k==1){
			age_total <- age
		} else {
			age_total <- merge(age_total, age, by="age", all=T)	
		}			
		k <- k+1					
	}		
}

age_total[is.na(age_total)] <- 0
write.table(age_total, "indexW_age_at_having_child_count", append=F, quote=F, sep=" ", row.names=F, col.names=T)



# count of age at first/last delivery for each gender for each birth year
years <- sort(unique(index_delivery[,"FodelseAr"]))
del_sum <- matrix(NA, ncol=7, nrow=length(years))           
colnames(del_sum) <- c("b_year","male_n","female_n","male_afc","female_afc","male_alc","female_alc")

for (year_n in 1:length(years)){
	del_sum[year_n,"b_year"] <- years[year_n]
	del_yearn <- index_delivery[index_delivery[,"FodelseAr"]==years[year_n], c("Kon","afc","alc")]
	for (kon_n in 1:length(kons)){	
		del_sum[year_n,paste(kons[kon_n],"_n",sep="")] <- nrow(del_yearn[del_yearn[,"Kon"]==kon_n, ])
		for (ac in c("afc","alc")){			
			del_sum[year_n, paste(kons[kon_n],ac,sep="_")] <- round(mean(del_yearn[del_yearn$Kon==kon_n,ac],na.rm=T), 3)       	
			print(paste(year_n, kon_n, ac, sep="_"))		
		}	
	}
}

write.table(del_sum, "indexW_age_at_having_child_summary", append=F, quote=F, sep=" ", row.names=F, col.names=T)




