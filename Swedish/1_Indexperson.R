## This script is to calculate LRS (N of children and N of grandchildren) and age of having the first/last child for each index person.


r_dir <- "/home/aoxing/DSGE_LRS/input/r_files/"
setwd(r_dir)





############################################
#   LRS (N of children or grandchildren)   #
############################################

## 1.1.1 Read in data 

# Index person (born 1956-1982)  
index <- get(load(paste0(r_dir, "tove_lev_index.Rdata")))    
index <- index[index$AterPnr==0, c("LopNr","FodelseAr","FodelseLan","FodelseKommun","Kon")]    # 2,892,333, after drop individuals emigrated from Sweden
table(index$Kon)                                                 # 1,488,254 males and 1,404,079 females 


# Children of index person
child <- as.data.frame(get(load(paste0(r_dir, "tove_lev_koppl_index_barn.Rdata"))))     
length(unique(child[,"LopNr"]))                                  # 2,202,668 indexperson with children
length(unique(child[,"LopNrBarn"]))                              # 2,900,503 children


# Grandchildren of index person
grandchild <- as.data.frame(get(load(paste0(r_dir, "tove_lev_koppl_index_barnbarn.Rdata"))))  
length(unique(grandchild[,c("LopNr")]))                          # 476,252 indexperson with grandchildren
length(unique(grandchild[,c("LopNrBarn")]))                      # 450,005 children
length(unique(grandchild[,c("LopNrBarnBarn")]))                  # 591,843 grandchildren



#-----------------------------------------------------------------------------------------
## 1.1.2 n_child and n_grandchild for each indexperson

# n_child
index_lrs <- as.data.frame(table(child[,"LopNr"]))              # n_child for indexperson with children
colnames(index_lrs) <- c("LopNr","n_child")                     # 2,202,668 indexperson with children
nrow(index) - nrow(index_lrs)                                   #   689,665 indexperson without children
mean(index_lrs[,"n_child"])                                     # 2.217 for indexperson with children
table(index_lrs[,"n_child"])                                    # from 1 to 27


# n_grandchild
index_glrs <- as.data.frame(table(unique(grandchild[,c("LopNr","LopNrBarnBarn")])["LopNr"]))    # 476,252 indexperson with grandchildren
colnames(index_glrs) <- c("LopNr","n_gchild")
nrow(index) - nrow(index_glrs)                                  # 2,416,081 indexperson without grandchildren
mean(index_glrs[,"n_gchild"])                                   # 2.608 for indexperson with grandchildren
table(index_glrs[,"n_gchild"])                                  # from 1 to 34


# combine
index_lrs_glrs <- merge(index_lrs, index_glrs, by="LopNr", all=T)  # 2,202,671, same as N of indexperson with children   
index_lrs_glrs$LopNr <- as.character(index_lrs_glrs$LopNr)     

lrs_all <- merge(index_lrs_glrs, index, by="LopNr", all.y=T)    # add indexperson without children
lrs_all[is.na(lrs_all)] <- 0                                 

summary(lrs_all[lrs_all[,"Kon"]==1, c("n_child","n_gchild")])   # male, mean(n_child/n_grandchild) = 1.573/0.3298, max(n_child/n_grandchild) = 27/31
summary(lrs_all[lrs_all[,"Kon"]==2, c("n_child","n_gchild")])   # female, mean(n_child/n_grandchild) = 1.81/0.5344, max(n_child/n_grandchild) = 19/34
save(lrs_all, file=paste0(r_dir, "index_lrs_all.Rdata"))                             


# distribution of n_child and n_grandchild by gender and birth_year of index person
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

write.table(lrs_summary, "index_lrs_summary", append=F, quote=F, sep=" ", row.names=F, col.names=T)
write.table(lrs_count_summary, "index_lrs_count_summary", append=F, quote=F, sep=" ",row.names=F, col.names=T)






############################################
#     Age of having first/last child       #
############################################

## 1.2.1 age at first/last delivery  
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
nrow(index_delivery)     # 2,202,668


index_delivery$afc <- as.numeric(index_delivery$bf_year) - as.numeric(index_delivery$FodelseAr)
index_delivery$alc <- as.numeric(index_delivery$bl_year) - as.numeric(index_delivery$FodelseAr)


index_delivery <- index_delivery[index_delivery$afc > 10 & index_delivery$alc > 10, ]     # remove age_at first/last child younger than 10
save(index_delivery, file=paste0(r_dir, "index_delivery.Rdata"))  



#-----------------------------------------------------------------------------------------
## 1.2.2 distribution of age at first/last delivery

# count of age at first/last delivery for each gender

k <- 1

for (ac in c("afc","alc")){

	for (kon_n in 1:length(kons)){	
		summary(index_delivery[index_delivery[,"Kon"]==kon_n, ac])	
		age <- as.data.frame(table(ind_del[index_delivery[,"Kon"]==kon_n, ac]))
		colnames(age) <- c("age", paste("count",ac,kons[kon_n],sep="_"))
		
		if (k == 1){
			age_total <- age
		} else {
			age_total <- merge(age_total, age, by="age", all=T)	
		}	
		
		k <- k+1					
	}	
	
}

age_total[is.na(age_total)] <- 0
write.table(age_total, "index_age_at_having_child_count", append=F, quote=F, sep=" ", row.names=F, col.names=T)



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

write.table(del_sum, "index_age_at_having_child_summary", append=F, quote=F, sep=" ", row.names=F, col.names=T)




