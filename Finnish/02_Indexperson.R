
## This script is to calculate LRS (N of children and N of grandchildren) and age of having the first/last child for each index person.


setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
in_dir <- "/homes/aliu/DSGE_LRS/input/"
r_dir <- paste0(in_dir, "r_files/")



############################################
#   LRS (N of children or grandchildren)   #
############################################

## Read in data 

# Index person (born 1956-1982)  
index <- get(load(paste0(r_dir,"index.Rdata")))                 # 2,365,707 indexperson
table(index$SUKUPUOLI)                                          # 1,260,350 with 1 and 1,105,357 with 2


#  Children of index person  
child <- get(load(paste0(r_dir,"child.Rdata")))  
length(unique(child$KANTAHENKILON_TNRO))                        # 1,555,839 indexperson with children
length(unique(child$SUKULAISEN_TNRO))                           # 2,060,722 children


#  Grandchildren of index person  
grandchild <- get(load(paste0(r_dir,"grandchild.Rdata")))  
length(unique(grandchild$KANTAHENKILON_TNRO))                   # 371,109 indexperson with grandchildren
length(unique(grandchild$SUKULAISEN_TNRO))                      # 482,777 grandchildren



#--------------------------------------------
## n_child and n_grandchild for each indexperson

# n_child
index_lrs <- as.data.frame(table(child$KANTAHENKILON_TNRO))     # n_child for indexperson with children
colnames(index_lrs) <- c("KANTAHENKILON_TNRO","n_child")        # 1,555,839 indexperson with children
nrow(index) - nrow(index_lrs)                                   #   809,868 indexperson are childless
mean(index_lrs[,"n_child"])                                     # 2.308 for indexperson with children
table(index_lrs[,"n_child"])                                    # from 1 to 24


# n_grandchild
index_glrs <- as.data.frame(table(grandchild$KANTAHENKILON_TNRO))     #   371,109 indexperson with grandchildren
colnames(index_glrs) <- c("KANTAHENKILON_TNRO","n_gchild")  
nrow(index) - nrow(index_glrs)                                        # 1,994,598 indexperson without grandchildren
mean(index_glrs[,"n_gchild"])                                         # 2.884 for indexperson with children 
table(index_glrs[,"n_gchild"])                                        # from 1 to 89   ## much more than Swedish



# combine 
index_lrs_glrs <- merge(index_lrs, index_glrs, by="KANTAHENKILON_TNRO", all=T)   # 1,555,839, same as N of indexperson with children           
index_lrs_glrs$KANTAHENKILON_TNRO <- as.character(index_lrs_glrs$KANTAHENKILON_TNRO)

lrs_all <- merge(index_lrs_glrs, index, by="KANTAHENKILON_TNRO", all=T)          # add indexperson without children, 2,365,707, same as N of indexperson
lrs_all[is.na(lrs_all)] <- 0                                                                                

summary(lrs_all[lrs_all[,"SUKUPUOLI"]==1, c("n_child","n_gchild")])      #   male: mean(n_child/n_grandchild) = 1.359/0.3469, max(n_child/n_grandchild) = 27/31
summary(lrs_all[lrs_all[,"SUKUPUOLI"]==2, c("n_child","n_gchild")])      # female: mean(n_child/n_grandchild) = 1.7/0.5727,   max(n_child/n_grandchild) = 19/34
save(lrs_all, file=paste0(r_dir,"index_lrs_all.Rdata"))                            



# distribution of n_child and n_grandchild by gender and birth_year of index person
lrs_all[,"b_year"] <- substr(lrs_all$SUKULAISEN_SYNTYMAPV,1,4)
years <- sort(unique(lrs_all[,"b_year"]))
kons <- c("male","female")

lrs_summary <- matrix(NA, ncol=11, nrow=length(years))                   # average of LRS             
colnames(lrs_summary) <- c("b_year","n_male","n_female","male_n_child","female_n_child","male_n_gchild","female_n_gchild","male_n_child_max","female_n_child_max","male_n_gchild_max","female_n_gchild_max")

lrs_count_summary <- matrix(NA, ncol=13, nrow=length(years))             # count of LRS
colnames(lrs_count_summary) <- c("b_year","n_male","n_female", paste("n_", rep(kons,each=5), "_", seq(0,4,1), "child", sep=""))


for (year_n in 1:length(years)){
	lrs_summary[year_n,"b_year"] <- lrs_count_summary[year_n,"b_year"] <- years[year_n]
	lrs_yearn <- lrs_all[lrs_all[,"b_year"]==unique(lrs_all[,"b_year"])[year_n], c("SUKUPUOLI","n_child","n_gchild")]
	for (kon_n in 1:length(kons)){
		lrs_summary[year_n, paste("n_",kons[kon_n],sep="")] <- lrs_count_summary[year_n, paste("n_",kons[kon_n],sep="")] <- nrow(lrs_yearn[lrs_yearn[,"SUKUPUOLI"]==kon_n,])	
		for (pop in c("child","gchild")){
			lrs_summary[year_n, paste(kons[kon_n],"n",pop,sep="_")] <- round(mean(lrs_yearn[lrs_yearn[,"SUKUPUOLI"]==kon_n, paste0("n_",pop)]), 3)  
			lrs_summary[year_n, paste(kons[kon_n],"n",pop,"max",sep="_")] <- max(lrs_yearn[lrs_yearn[,"SUKUPUOLI"]==kon_n, paste0("n_",pop)])
		}	
		for (n in 0:4){
			lrs_count_summary[year_n, paste0("n_",kons[kon_n],"_",n,"child")] <- nrow(lrs_yearn[lrs_yearn[,"n_child"]==n,])
		}
		print(paste(year_n, kon_n, sep="_"))
	}
}

write.table(lrs_summary, "index_lrs_summary", append=F, quote=F, sep=" ", row.names=F, col.names=T)
write.table(lrs_count_summary,"index_lrs_count_summary",append=F,quote=F,sep=" ",row.names=F,col.names=T)




####################################
#  Age of having first/last child  #
####################################

## age at first/last delivery  
child$b_year <- substr(child$SUKULAISEN_SYNTYMAPV,1,4)      # birth_year
child_bas <- child[order(child$KANTAHENKILON_TNRO,child$SUKULAISEN_SYNTYMAPV), c("KANTAHENKILON_TNRO","SUKULAISEN_TNRO","b_year")]     # order by id and child's birth_year 


# first child 
child_f_lst <- by(child_bas, child_bas$KANTAHENKILON_TNRO, head, n=1)                         
child_f <- do.call("rbind", child_f_lst)       ## Reduce is 10 time slower than do.call, do.call is 10 times slower than rebindlist


# last child
child_l_lst <- by(child_bas, child_bas$KANTAHENKILON_TNRO, tail, n=1)    
child_l <- do.call("rbind",child_l_lst) 


# combine
child_fl <- merge(child_f, child_l, all=T, by="KANTAHENKILON_TNRO")   
colnames(child_fl) <- c("KANTAHENKILON_TNRO","child_f_TNRO","bf_year","child_l_TNRO","bl_year")


lrs_all$b_year <- substr(lrs_all$SUKULAISEN_SYNTYMAPV,1,4)      # birth_year
ind_bas <- lrs_all[lrs_all[,"n_child"]!=0, c("KANTAHENKILON_TNRO","b_year","SUKUPUOLI")]
ind_del <- merge(ind_bas, child_fl, by="KANTAHENKILON_TNRO")
nrow(ind_del)     # 1,555,839


ind_del$afc <- as.numeric(ind_del$bf_year) - as.numeric(ind_del$b_year)
ind_del$alc <- as.numeric(ind_del$bl_year) - as.numeric(ind_del$b_year)

ind_del <- ind_del[ind_del$afc > 10 & ind_del$alc > 10, ]  # remove age_at first/last child younger than 10
save(ind_del, file=paste0(r_dir,"index_afc_alc.Rdata"))  



#-----------------------------------------------------------------------------------------
## 1.2.2 distribution of age at first/last delivery

# count of age at first/last delivery for each gender
kons <- c("male","female")

k <- 1
for (ac in c("afc","alc")){
	for (kon_n in 1:length(kons)){
		summary(ind_del[ind_del[,"SUKUPUOLI"]==kon_n, ac])	
		age <- as.data.frame(table(ind_del[ind_del[,"SUKUPUOLI"]==kon_n, ac]))
		colnames(age) <- c("age", paste("count",ac,kons[kon_n],sep="_"))		
		if (k == 1){
			age_total <- age
		}else{
			age_total <- merge(age_total, age, by="age", all=T)	
		}	
		k <- k+1					
	}	
}

age_total[is.na(age_total)] <- 0
write.table(age_total, "index_age_at_having_child_count", append=F, quote=F, sep=" ", row.names=F, col.names=T)



# count of age at first/last delivery for each gender for each birth year
years <- sort(unique(ind_del[,"b_year"]))
n_year <- length(years)   # 27 
del_sum <- matrix(NA, ncol=7, nrow=n_year)           
colnames(del_sum) <- c("b_year","male_n","female_n","male_afc","female_afc","male_alc","female_alc")
kons <- c("male","female")

for (year_n in 1:n_year){
	del_sum[year_n,"b_year"] <- years[year_n]
	del_yearn <- ind_del[ind_del[,"b_year"]==years[year_n],c("SUKUPUOLI","afc","alc")]	
	for (kon_n in 1:length(kons)){
		del_sum[year_n,paste(kons[kon_n],"_n",sep="")] <- nrow(del_yearn[del_yearn[,"SUKUPUOLI"]==kon_n,])		
		for (ac in c("afc","alc")){	
			del_sum[year_n, paste(kons[kon_n],ac,sep="_")] <- round(mean(del_yearn[del_yearn$SUKUPUOLI==kon_n,ac],na.rm=T), 3)       	
			print(paste(year_n, kon_n, ac, sep="_"))
		}	
	}	
}

write.table(del_sum, "index_age_at_having_child_summary", append=F, quote=F, sep=" ", row.names=F, col.names=T)




