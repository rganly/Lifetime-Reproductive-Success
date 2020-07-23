
## 4. This script is to check the marriage registry of index person and parents of indexperson's children and siblings's children

setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
in_dir <- "/homes/aliu/DSGE_LRS/input/"
r_dir <- paste0(in_dir, "r_files/")



##########################################
#    Marriage Registry of index person   #
##########################################

# 4.1.1 For indexperson only?

avio <- get(load(paste0(r_dir,"thl2019_804_avio.Rdata")))
nrow(avio)    # 1,743,787
length(unique(avio$TUTKHENK_TNRO))   # 1,492,115
length(unique(avio$PUOLISON_TNRO))   # 1,491,233


index <- get(load(paste0(r_dir,"index.Rdata")))
index_lst <- index$KANTAHENKILON_TNRO
length(index_lst)        # 2,365,707 indexperson
length(setdiff(unique(avio$TUTKHENK_TNRO), index_lst))     # 0, indicates marriage registry is only for index person 
length(intersect(unique(avio$PUOLISON_TNRO), index_lst))   # 1,277,877 indexperson's spouse are also index person



# 4.1.2 how many registered spouse without records 

# population registry which recorded demographic info (e.g. birth municipality)
tuk <- get(load(paste0(r_dir,"thl2019_804_tutkhenk.Rdata")))        # all relatives of index person
length(unique(tuk$SUKULAISEN_TNRO))                                 # 6,695,121
length(setdiff(unique(avio$PUOLISON_TNRO), tuk$SUKULAISEN_TNRO))    # 51,382 spouse don't have demographic information


# all registry other than population registry
edu <- get(load(paste0(r_dir,"koulutus_ala_aste_u1477_a.Rdata")))   # the list to extract education registry is the same as all kinds of health registry
ei <- get(load(paste0(r_dir,"ei_hnroa_u1477_a.Rdata")))             # not found in Statistic Finland database
length(setdiff(avio$PUOLISON_TNRO, c(ei$TNRO, edu$TNRO)))           # 1, indicates almost all spouse were involved when extracting all registry other than population registry




###################################################
#   parents of children and sibling’s children    #
###################################################

# parents of children and sibling’s children (duplicates)
#tlj <- get(load(paste0(r_dir,"thl2019_804_tljslv.Rdata")))
#nrow(tlj)            # 12,424,122
#length(unique(tlj$KANTAHENKILON_TNRO))  # 2,631,986
#head(tlj)


#dup <- duplicated(tlj)
#tlj_uniq <- tlj[!dup, ]   
#nrow(tlj_uniq)       # 5,132,871 
#save(tlj_uniq, file=paste(r_dir,"thl2019_804_tljslv_uniq.Rdata"))
tlj_uniq <- get(load(paste(r_dir,"thl2019_804_tljslv_uniq.Rdata")))
print("read in tlj_uniq done")


sib_uniq <- get(load(paste0(r_dir,"sib_uniq.Rdata")))   # 2,307,224
sib_lst <- sib_uniq$SUKULAISEN_TNRO
length(sib_lst)      # 2307224


# another parent of index person's children
pops <- c("child","sibchild")


for (pop in c("child","sibchild")){

	# read in list of child or sibchild
	c <- get(load(paste0(r_dir, pop, ".Rdata")))
	c_lst <- unique(c$SUKULAISEN_TNRO)   
	length(c_lst)    # 2,060,722 / 2,392,471
	
		
	# extract parents for child or for sibchild
	tlj_c <- tlj_uniq[tlj_uniq$KANTAHENKILON_TNRO %in% c_lst, c("KANTAHENKILON_TNRO", "SUKUL_SUHDE", "SUKULAISEN_TNRO")]
	nrow(tlj_c)      # 4,011,826 / 4,694,040
	
	tlj_c_a <- tlj_c[tlj_c$SUKUL_SUHDE == "3a", ]
	tlj_c_i <- tlj_c[tlj_c$SUKUL_SUHDE == "3i", ]
	tlj_c_ai <- merge(tlj_c_a, tlj_c_i, all=T ,by="KANTAHENKILON_TNRO")
	nrow(tlj_c_ai)   # 2,060,929 / 2,392,691
	
	
	# list of index person or of sibling
	if (pop=="child"){
		popp_lst <- index_lst
	}else{
		popp_lst <- sib_lst
	}
	
	
	# both parents are indexperson for pop=="child" or are sibling for pop=="sibchild"
	tlj_c_b <- tlj_c_ai[(tlj_c_ai$SUKULAISEN_TNRO.x %in% popp_lst) & (tlj_c_ai$SUKULAISEN_TNRO.y %in% popp_lst), ]
	nrow(tlj_c_b)    # 1,530,974 / 1,295,555
	
	b_lst <- unique(c(tlj_c_b$SUKULAISEN_TNRO.x, tlj_c_b$SUKULAISEN_TNRO.y))
	length(b_lst)    # 1,326,920 / 1,116,904
	length(setdiff(b_lst, popp_lst))   # 0
	
	
	
	# only one parent are indexperson for pop=="child" or are sibling for pop=="sibchild"
	tlj_c_o <- tlj_c_ai[tlj_c_ai$KANTAHENKILON_TNRO %in% setdiff(tlj_c_ai$KANTAHENKILON_TNRO, tlj_c_b$KANTAHENKILON_TNRO), ]
	nrow(tlj_c_o)    # 529,954 / 1,097,131
	
	o_lst <- setdiff(unique(c(tlj_c_o$SUKULAISEN_TNRO.x, tlj_c_o$SUKULAISEN_TNRO.y)), popp_lst)
	length(o_lst)    # 207,527 / 489,280
	
	
	
	# spouse with shared child for index person or sibling
	sp_c_lst <- unique(c(b_lst, o_lst))

	if (pop=="child"){
		print(paste0(length(sp_c_lst)," spouse have common child with index person."))
	}else{
		print(paste0(length(sp_c_lst)," spouse have common child with sibling."))
	}
	
}


# "1,534,447 spouse have common child with index person."
# "1,606,184 spouse have common child with sibling."




###############################################################
#  Spouse in marriage registry but with/without common child  #
###############################################################

length(setdiff(unique(avio$PUOLISON_TNRO), sp_c_lst))     # 489,762 in marriage registry, but don't have common child with any index person)

length(intersect(unique(avio$PUOLISON_TNRO), sp_c_lst))   # 1,001,471 spouse have common child with index person are in marrigae registry







