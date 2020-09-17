
## 3. This script is to count N of children & grandchildren & sib_children for each birth year 


setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
in_dir <- "/homes/aliu/DSGE_LRS/input/"
r_dir <- paste0(in_dir, "r_files/")


#################################################
##   children & grandchildren & sib_children    #
#################################################

for (pop in c("child","grandchild","sibchild")){

	dat <- get(load(paste0(r_dir, pop, ".Rdata")))
	print(paste(length(unique(dat$KANTAHENKILON_TNRO)), "index person have",length(unique(dat$SUKULAISEN_TNRO)), "unique", pop, sep=" "))  
	
	
	# keep unique child/grandchild/sibchild, add birth_year, and calculate sex ratio
	dat[,"b_year"] <- substr(dat$SUKULAISEN_SYNTYMAPV,1,4)
	dat_basic <- dat[,c("SUKULAISEN_TNRO","SUKUPUOLI","b_year")]
	
	dup <- duplicated(dat_basic[,c("SUKULAISEN_TNRO")])
	dat_uniq <- dat_basic[!dup, ]
	print(paste("male:female is ",round(nrow(dat_uniq[dat_uniq$SUKUPUOLI==1,])/nrow(dat_uniq[dat_uniq$SUKUPUOLI==2,]),3),":1 for ",pop,sep=""))


	# population size of child/grandchild/sibchild for each birth_year
	dat_mf <- as.data.frame(table(dat_uniq[dat_uniq$SUKUPUOLI==1,"b_year"]))
	colnames(dat_mf) <- c("b_year",paste("freq",pop,"male",sep="_"))	
	
	dat_ff <- as.data.frame(table(dat_uniq[dat_uniq$SUKUPUOLI==2,"b_year"]))
	colnames(dat_ff) <- c("b_year",paste("freq",pop,"female",sep="_"))
	
	dat_freq <- merge(dat_mf, dat_ff, by="b_year",all=T)
	dat_freq[is.na(dat_freq)] <- 0
	write.table(dat_freq, paste(pop,"birth_year.freq",sep="_"), append=F, quote=F, sep=" ", row.names=F, col.names=T)
	
}


