
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


# tlj for 
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


