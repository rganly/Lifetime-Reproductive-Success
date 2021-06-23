## This script is to convert all registry data to .Rdata format and comment on each file

# Input: all original files 
# Output: "Data_comments_FIN.csv"
# Comments: See "Data_comments_FIN.xlsx", which was adapted from "Data_comments_FIN.csv"



#    ############################
#    ##  Upload to FIMM epouta  #
#    ############################
#
#    scp   /Users/aoxliu/Downloads/Files/*  aliu@ssh.fimm.fi:/homes/aliu/DSGE_LRS/input/              # local terminal
#    rsync -P -e ssh /Users/aoxliu/Downloads/Files/*  aliu@ssh.fimm.fi:/homes/aliu/DSGE_LRS/input/    # local terminal, recover the uploading, faster than scp


# system("mkdir /homes/aliu/DSGE_LRS/output/registry_edit/")
# system("mkdir /homes/aliu/DSGE_LRS/input/r_files")


setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
in_dir <- "/homes/aliu/DSGE_LRS/input/"
r_dir <- paste0(in_dir, "r_files/")

library(haven)



###########################
##  Comment on each file  #  
###########################

# system("rm Data_comments_FIN.csv")
wfile <- c(list.files(in_dir, pattern="*.sas7bdat"), "fcr_all_data.csv")

for (k in wfile){
	if (k != "fcr_all_data.csv"){
		d <- read_sas(paste0(in_dir, k))
	#	save(d, file=paste0(r_dir, gsub(".sas7bdat",".Rdata",k)))   # convert to Rdata format
	}else{
		d <- read.table(paste0(in_dir,"fcr_all_data.csv"), sep=";", header=T)
	}
	
	info <- matrix(NA, nrow=ncol(d), ncol=14)
	colnames(info) <- c("FileNumber","File","FileDescription","NoOfRows","NoOfColumns","ColumnNumber","ColumnName","ColumnNameOriginal","ColumnDescription","NumberOfLevels","Min","Max","DataType","FileType")	
	print(paste0("Start for file: ", k))
	
	for (i in 1:ncol(d)){
		print(paste("Column", i ,"of", ncol(d), "columns", sep=" "))
		info[i, c("FileNumber","File")] <- c(which(wfile==k), k)		
		info[i, c("NoOfRows","NoOfColumns")] <- c(nrow(d),ncol(d))		
		info[i, c("ColumnNumber", "ColumnName")] <- c(i,colnames(d)[i])		
		info[i, "NumberOfLevels"] <- length(unlist(unique(d[,i])))
		#info[i, c("Min","Max")] <- c(min(d[d[,i]!="",i]), max(d[d[,i]!="",i]))
		if (is.numeric(d[,i])==T){
			info[i, c("Min","Max")] <- c(min(d[d[,i]!="",i]), max(d[d[,i]!="",i]))
		}
		info[i, "DataType"] <- ifelse(as.numeric(info[i, "NumberOfLevels"])<20, paste(unique(unlist(d[, i])),collapse = ","), paste(head(unique(unlist(d[, i])),5),collapse = ","))		
	}
	rm(d)  # save memory
	
	write.table(info, "Data_comments_FIN.csv", append=T, quote=F, sep=" ", row.names=F, col.names=F)
	print(paste0("Done for file: ", k))	
}



############################
#   what is "roolit" for?  #
############################

# check whether the 1st column of "roolit" are for indexperson and children of indexperson and sibling
tlj <- get(load(paste0(r_dir, "thl2019_804_tljslv.Rdata")))
child_lst <- unique(tlj$KANTAHENKILON_TNRO)
length(child_lst)      # 2,631,986

index <- get(load(paste0(r_dir,"index.Rdata")))                     # 2,365,707 
length(unique(c(child_lst, index$KANTAHENKILON_TNRO)))              # 4,546,684

roolit <- get(load(paste0(r_dir,"thl2019_804_roolit.Rdata")))  
length(setdiff(unique(roolit$KANTAHENKILON_TNRO), unique(c(child_lst, index_lst))))    # 0
length(setdiff(unique(c(child_lst, index_lst)), unique(roolit$KANTAHENKILON_TNRO)))    # 0

child_notindex_lst <- setdiff(child_lst, index$KANTAHENKILON_TNRO)   # 2,180,977
roolit_cni <- roolit[roolit$KANTAHENKILON_TNRO %in%  child_notindex_lst, ]
table(roolit_cni$SUKUL_SUHDE)   #  only 3a and 3j, indicate for children of index and siblings, only parents were provided, therefore the same as "thl2019_804_tljslv.sas7bdat"


# "roolit" was for indexperson and children of indexperson and sibling
# For indexperson, the information provided was exactly the same as "thl2019_804_roolit.sas7bdat"
# For children of indexperson and sibling, the information provided was exactly the same as "thl2019_804_tljslv.sas7bdat"
# Therefore, "roolit"  could be ignored since no extra information was provided



############################
# Extract each population  #
############################

# tuk for all relatives of index person
tuk <- get(load(paste0(r_dir,"thl2019_804_tutkhenk.Rdata")))    
nrow(tuk)     # 33,449,716
table(tuk$SUKUL_SUHDE)   

pops <- c("index", "child", "parent", "sib", "grandchild", "sibchild")
p_abb <- c(0, 2, 3, 4, 7, 9)

for (n in 1:length(pops)){
	pop_dat <- tuk[substr(tuk$SUKUL_SUHDE,1,1) %in% p_abb[n], ]
	save(pop_dat, file=paste0(r_dir, pops[n], ".Rdata"))
}

tuk$b_year <- substr(tuk$SUKULAISEN_SYNTYMAPV, 1, 4)
tuk_bas <- tuk[ ,c("SUKULAISEN_TNRO", "SUKUPUOLI", "b_year")]
tuk_bas <- unique(tuk_bas)
nrow(tuk_bas)


# tlj 
dup <- duplicated(tlj)    # this step need large memory
tlj_uniq <- tlj[!dup, ]   
nrow(tlj_uniq)       # 5,132,871 
save(tlj_uniq, file=paste0(r_dir, "thl2019_804_tljslv_uniq.Rdata"))



# creat a dataset including demographic info for all individuals
demo_extract <- function(dat_name){
	dat <- get(load(paste0(r_dir,"thl2019_804_",dat_name,".Rdata")))[ ,5:15]
	demo <- dat[!duplicated(dat$SUKULAISEN_TNRO), ]
	print(paste0("n_rows=",nrow(dat),"; n_individuals=",nrow(demo)))   	
	return(demo)
}


demo_r <- demo_extract(dat_name="tutkhenk")        # "n_rows=33449716; n_individuals=6,695,121"
demo_p <- demo_extract(dat_name="tljslv_uniq")     # "n_rows=5132871; n_individuals=2285704"

demo <- rbind(demo_r,demo_p) 
demo <- demo[!duplicated(demo$SUKULAISEN_TNRO), ]  
demo$b_year <- substr(demo$SUKULAISEN_SYNTYMAPV,1,4)
nrow(demo)    # 6,752,171
save(demo, file=paste0(r_dir,"demo.Rdata"))




