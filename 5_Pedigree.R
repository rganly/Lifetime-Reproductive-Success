
## 5. This script is to construct pedigree for the full population (in a format "id, father_id, mother_id, "sex", birth_year")


setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"


# function for index and children whose parents were directly provided
ped <- function(dat, cha, pat, sex){
	par <- dat[dat$SUKUL_SUHDE==cha & dat$SUKUPUOLI==sex, ]
	print(paste0(nrow(par)," individuals with ",length(unique(par$SUKULAISEN_TNRO))," ",pat)) 
	par <- par[,c("KANTAHENKILON_TNRO","SUKULAISEN_TNRO")]
	colnames(par) <- c("id", paste(pat,"id",sep="_"))
	return(par)	
}


# function for sibling and grandchildren whose parents were not directly provided, need to generate from relative relationship
ped_2 <- function(dat, sex_v, cha, pat){
	par <- dat[dat[ ,sex_v]==cha, ]
	print(paste0(nrow(par)," individuals with ",length(unique(par$VALIHENKILO1_TNRO))," ",pat)) 
	par <- par[ ,c("SUKULAISEN_TNRO","VALIHENKILO1_TNRO")]
	colnames(par) <- c("id", paste(pat,"id",sep="_"))
	return(par)	
}


'%!in%' <- function(x,y)!('%in%'(x,y))



#####################
##   Index person   #
#####################

parent <- get(load(paste0(r_dir, "parent.Rdata")))
nrow(parent)         # 3,868,435
length(unique(parent$KANTAHENKILON_TNRO))   # 1,991,222



# Extract parent info
index_f <- ped(dat=parent, cha="3i", pat="father", sex=1)   # 1890429 individuals with 964328 father
index_m <- ped(dat=parent, cha="3a", pat="mother", sex=2)   # 1977959 individuals with 1017065 mother
index_fm <- merge(index_f, index_m, by="id", all=T)



# Add birth_year and sex
index <- get(load(paste0(r_dir, "index.Rdata")))
index$b_year <- substr(index$SUKULAISEN_SYNTYMAPV,1,4)
index_bas <- index[ ,c("KANTAHENKILON_TNRO","SUKUPUOLI","b_year")]  # 2,365,707
colnames(index_bas) <- c("id","sex","b_year")


index_ped <- merge(index_fm, index_bas, by="id", all=T)     # 2,365,834
save(index_ped, file=paste0(r_dir,"index_ped.Rdata"))


nrow(index_bas)      # 2365707
nrow(index_fm)       # 1991349
nrow(index_ped)      # 2365834
nrow(index) - nrow(index_fm)  # 374358 index person without any parent info




##################################
##   children and sib_children   #
##################################

tlj_uniq <- get(load(paste(r_dir,"thl2019_804_tljslv_uniq.Rdata")))
nrow(tlj_uniq)       # 5,132,871
length(unique(tlj_uniq$KANTAHENKILON_TNRO))   # 2,631,986



# extract parent info
child_f <- ped(dat=tlj_uniq, cha="3i", pat="father", sex=1)  # 2516655 individuals with 1118832 father
child_m <- ped(dat=tlj_uniq, cha="3a", pat="mother", sex=2)  # 2615994 individuals with 1166755 mother
child_fm <- merge(child_f, child_m, by="id")
nrow(child_fm)       # 2,500,882


# Add birth_year and sex
child <- get(load(paste0(r_dir,"child_uniq.Rdata")))
nrow(child)          # 20,607,22
sibchild <- get(load(paste0(r_dir,"sibchild_uniq.Rdata")))
nrow(sibchild)       # 2,392,471


child_a <- unique(rbind(child, sibchild))
nrow(child_a)        # 2,632,301		
child_bas <- child_a[ ,c("SUKULAISEN_TNRO","SUKUPUOLI","b_year")]  
colnames(child_bas) <- c("id","sex","b_year")


child_ped <- merge(child_fm, child_bas, by="id", all=T)
save(child_ped, file=paste0(r_dir,"child_ped.Rdata"))


nrow(child_bas)      # 2632301
nrow(child_fm)       # 2500672
nrow(child_ped)      # 2632529




###################
##    Sibling     #
###################

sib <- get(load(paste0(r_dir,"sib.Rdata")))
nrow(sib)            # 8,066,956
sib <- sib[,c("SUKULAISEN_TNRO","VALIHENKILO1_TNRO","SUKUL_SUHDE")]

sib <- unique(sib)
nrow(sib)            # 4,330,797



# extract parent info	
sib_f <- ped_2(dat=sib, sex_v="SUKUL_SUHDE", cha="4i", pat="father")  # "2125349 individuals with 782072 father"
sib_m <- ped_2(dat=sib, sex_v="SUKUL_SUHDE", cha="4a", pat="mother")  # "2205448 individuals with 818753 mother"
sib_fm <- merge(sib_f, sib_m, by="id", all=T)



# Add birth_year and sex
sib_uniq <- get(load(paste0(r_dir,"sib_uniq.Rdata")))
nrow(sib_uniq)       # 2307224
sib_uniq[,"b_year"] <- substr(sib_uniq$SUKULAISEN_SYNTYMAPV,1,4)
sib_bas <- sib_uniq[ ,c("SUKULAISEN_TNRO","SUKUPUOLI","b_year")]
colnames(sib_bas) <- c("id","sex","b_year")

sib_ped <- merge(sib_fm, sib_bas, by="id", all=T)
save(sib_ped, file=paste0(r_dir,"sib_ped.Rdata"))

nrow(sib_ped)        # 2307290
nrow(sib_fm)         # 2307290
nrow(sib_bas)        # 2307224




######################
##   grandchildren   # 
######################

grandchild <- get(load(paste0(r_dir,"grandchild.Rdata")))
nrow(grandchild)     # 1070302
grandchild <- grandchild[,c("SUKULAISEN_TNRO", "VALIHENKILO1_TNRO")]

grandchild <- unique(grandchild)
nrow(grandchild)     # 665491

grandchild_s <- merge(grandchild, child_bas[,c("id","sex")],by.x="VALIHENKILO1_TNRO", by.y="id")
nrow(grandchild_s)   # 665591   


# extract parent info
grandchild_f <- ped_2(dat=grandchild_s, sex_v="sex", cha="1", pat="father")  # "279718 individuals with 148122 father"
grandchild_m <- ped_2(dat=grandchild_s, sex_v="sex", cha="2", pat="mother")  # "385873 individuals with 195069 mother"
grandchild_fm <- merge(grandchild_f, grandchild_m, by="id", all=T)



# Add birth_year and sex
grandchild_uniq <- get(load(paste0(r_dir,"grandchild_uniq.Rdata")))
nrow(grandchild_uniq)   # 482777 
grandchild_bas <- grandchild_uniq[ ,c("SUKULAISEN_TNRO","SUKUPUOLI","b_year")]
colnames(grandchild_bas) <- c("id","sex","b_year")

grandchild_ped <- merge(grandchild_fm, grandchild_bas, by="id", all=T)
save(grandchild_ped, file=paste0(r_dir,"grandchild_ped.Rdata"))

nrow(grandchild_ped)    # 482897
nrow(grandchild_fm)     # 482897
nrow(grandchild_bas)    # 482777





######################
##   Combine all     # "# ***" are data need to be combined for the final ped
######################

index_ped <- get(load(paste0(r_dir,"index_ped.Rdata")))
sib_ped <- get(load(paste0(r_dir,"sib_ped.Rdata")))
child_ped <- get(load(paste0(r_dir,"child_ped.Rdata")))
grandchild_ped <- get(load(paste0(r_dir,"grandchild_ped.Rdata")))

all <- rbind(index_ped, sib_ped, child_ped, grandchild_ped)
all[,"n"] <- is.na(all[,"father_id"]) + is.na(all[,"mother_id"])
nrow(all)       # 7788550
all <- unique(all)
nrow(all)       # 5533127



# both parents available
all_b <- all[all$n==0, ]    #***
nrow(all_b)   # 4398836    



# with at least one parent missing
all_o <- all[all$n!=0 & all$id %!in% all_b$id, ]    
nrow(all_o)   #  725085 



# id appear once 
dup_lst <- all_o[duplicated(all_o[,c("id")]), "id"] 
length(dup_lst) # 32367

all_oo <- all_o[all_o$id %!in% dup_lst, ]              #***
nrow(all_oo)   # 660366 



# # id appear more than once but with one as missing 
all_om <- all_o[all_o$id %in% dup_lst & all_o$n!=2, ]  
nrow(all_om)      # 32397


dup2_lst <- all_om[duplicated(all_om[,c("id")]), "id"] 
length(dup2_lst)  # 45

all_omo <- all_om[all_om$id %!in% dup2_lst, ]         #***
nrow(all_omo)     # 32307



# id appear more than once and multiple records for each id are different
all_omm <- all_om[all_om$id %in% dup2_lst, ]          
nrow(all_omm)   # 90


all_omm_f <- all_omm[is.na(all_omm$mother_id),]       # with father but mother missing
nrow(all_omm_f)  # 29


all_omm_m <- all_omm[is.na(all_omm$father_id),]       # with mother but father missing
nrow(all_omm_m)  # 61


all_omm_fm <- merge(all_omm_f[,c("id","father_id","sex","b_year")], all_omm_m[,c("id","mother_id")], by="id")
all_omm_fm[,"n"] <- 2
all_omm_fm <- all_omm_fm[,colnames(all)]                    # ***
all_omm_dup <- all_omm[all_omm$id %!in% all_omm_fm$id, ]    # ***


ped_all <- rbind(all_b, all_oo, all_omo, all_omm_fm, all_omm_dup)  

save(ped_all, file=paste0(r_dir,"ped_all.Rdata"))


# summary
male_lst <- unique(c(ped_all[ped_all$sex==1,"id"], ped_all$father_id))
length(male_lst)     # 3500235
female_lst <- unique(c(ped_all[ped_all$sex==2,"id"], ped_all$mother_id))
length(female_lst)   # 3251939


# count index person's spouse which are not in ped_all
avio <- get(load(paste0(r_dir,"thl2019_804_avio.Rdata")))
avio_add <- avio[avio$PUOLISON_TNRO %!in% c(male_lst,female_lst), c("TUTKHENK_TNRO", "PUOLISON_TNRO")]
nrow(avio_add)       # 19550


index <- get(load(paste0(r_dir,"index.Rdata")))    # extract sex of index person in order to know the sex of spouse
index_sex <- index[,c("KANTAHENKILON_TNRO","SUKUPUOLI")]
index_sex[,"s_sex"] <- ifelse(index_sex[,"SUKUPUOLI"]==1,2,1)

avio_add_sex <- merge(avio_add, index_sex, by.x="TUTKHENK_TNRO", by.y="KANTAHENKILON_TNRO")
nrow(avio_add_sex)  # 19550


length(unique(c(male_lst,avio_add_sex[avio_add_sex$s_sex==1,"PUOLISON_TNRO"])))    # 3,510,434 males in Finnish data
length(unique(c(female_lst,avio_add_sex[avio_add_sex$s_sex==2,"PUOLISON_TNRO"])))  # 3,259,875 females in Finnish data





