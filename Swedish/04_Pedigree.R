## This script is to create full pedigree, including parents (id, father, mother, gender, birth_year) of children, grandchildren, sibling's children, indexperson, and sibling


# Input: "tove_lev_koppl_barn_foraldrar.Rdata", "tove_lev_koppl_index_barn.Rdata", "tove_lev_koppl_index_barnbarn.Rdata", "tove_lev_koppl_sysbarn_foraldr.Rdata", "tove_lev_koppl_index_sysbarn.Rdata", "tove_lev_koppl_index_foraldrar.Rdata", "tove_lev_index.Rdata", "sib_uniq.Rdata","tove_lev_koppl_index_syskon.Rdata"
# Output: "Pedigree.Rdata"
# Comments: This file is for building family tree and doesn't cover all samples in the population registry


setwd("/home/aoxing/DSGE_LRS/out/registry_edit/")
r_dir <- "/home/aoxing/DSGE_LRS/input/r_files/"

library(tidyverse)
'%!in%' <- function(x,y)!('%in%'(x,y))



############################################
#    Read in demo for each population      #
############################################

# parents of indexperson's children--------------------------------------------------------
barn_ped <- data.frame(get(load(paste0(r_dir, "tove_lev_koppl_barn_foraldrar.Rdata"))))    
nrow(barn_ped)                              # 2,900,503 children from indexperson, where 30,130 have father as NA, 1,725 have mother as NA

# add gender, birth_year of indexperson's children
barn <- data.frame(get(load(paste0(r_dir, "tove_lev_koppl_index_barn.Rdata"))))       
barn_basic <- barn[!duplicated(barn$LopNrBarn) ,c("LopNrBarn","BarnKon","BarnFodelseAr")]
nrow(barn_basic)                            # 2,900,503

barn_ped <- merge(barn_ped, barn_basic, by="LopNrBarn")
nrow(barn_ped)                              # 2,900,503
colnames(barn_ped) <- c("LopNr","LopNrFar","LopNrMor","Kon","FodelseAr")



# parents of indexperson's grandchildren, from barn-barn_bb pairs and barn's gender--------------------------------------------------------
barn_bb <- data.frame(get(load(paste0(r_dir, "tove_lev_koppl_index_barnbarn.Rdata"))))  
barn_bb_basic <- barn_bb[!duplicated(barn_bb[,c("LopNrBarn", "LopNrBarnBarn")]), c("LopNrBarn","LopNrBarnBarn","BarnBarnKon","BarnBarnFodelseAr")]

barn_bb_basic_far <- barn_bb_basic[barn_bb_basic$LopNrBarn %in% barn_ped[barn_ped$Kon==1, "LopNr"], ]
nrow(barn_bb_basic_far)                      # 333,407
barn_bb_basic_mor <- barn_bb_basic[barn_bb_basic$LopNrBarn %in% barn_ped[barn_ped$Kon==2, "LopNr"], ]
nrow(barn_bb_basic_mor)                      # 469,333

barn_bb_ped <- merge(barn_bb_basic_far, barn_bb_basic_mor, by="LopNrBarnBarn", all=T)
length(unique(barn_bb[,c("LopNrBarnBarn")])) # 591,843 grandchild

barn_bb_ped[, c("LopNr","LopNrFar","LopNrMor")] <- barn_bb_ped[, c("LopNrBarnBarn","LopNrBarn.x","LopNrBarn.y")]
barn_bb_ped$Kon <- ifelse(is.na(barn_bb_ped$BarnBarnKon.x), barn_bb_ped$BarnBarnKon.y, barn_bb_ped$BarnBarnKon.x)
barn_bb_ped$FodelseAr <- ifelse(is.na(barn_bb_ped$BarnBarnFodelseAr.x), barn_bb_ped$BarnBarnFodelseAr.y, barn_bb_ped$BarnBarnFodelseAr.x)
barn_bb_ped <- barn_bb_ped[ ,c("LopNr","LopNrFar","LopNrMor","Kon","FodelseAr")]
nrow(barn_bb_ped)                            # 591,843, where 258,436 have father as NA, 122,510 have mother as NA      



# parents of sibling's children--------------------------------------------------------
syskonbarn_ped <- data.frame(get(load(paste0(r_dir, "tove_lev_koppl_sysbarn_foraldr.Rdata"))))    
nrow(syskonbarn_ped)                          # 3,658,399 children from sibling, where 37,036 have father as NA, 1,939 mother as NA

syskonbarn <- data.frame(get(load(paste0(r_dir, "tove_lev_koppl_index_sysbarn.Rdata"))))  
syskonbarn_basic <- syskonbarn[!duplicated(syskonbarn$LopNrSyskonBarn), c("LopNrSyskonBarn","SyskonBarnKon","SyskonBarnFodelseAr")]
nrow(syskonbarn_basic)                        # 3,658,399

syskonbarn_ped <- merge(syskonbarn_ped, syskonbarn_basic, by="LopNrSyskonBarn")
nrow(syskonbarn_ped)                          # 3,658,399                         
colnames(syskonbarn_ped) <- c("LopNr","LopNrFar","LopNrMor","Kon","FodelseAr")



# parents of indexperson--------------------------------------------------------
index_ped <- data.frame(get(load(paste0(r_dir, "tove_lev_koppl_index_foraldrar.Rdata"))))    
nrow(index_ped)                               # 2,893,654 indexperson, where 55,961 have father as NA, 16,083 have mother as NA
index_ped <- index_ped[, c("LopNr", "LopNrFar", "LopNrMor")]

index <- data.frame(get(load(paste0(r_dir, "tove_lev_index.Rdata"))))         # add gender, birth_year of indexperson's children
index_basic <- index[, c("LopNr","Kon","FodelseAr")]
nrow(index_basic)                             # 2,893,654

index_ped <- merge(index_ped, index_basic, by="LopNr")
nrow(index_ped)                               # 2,900,503
colnames(index_ped)                           # "LopNr","LopNrFar","LopNrMor","Kon","FodelseAr"



# parents of sibling--------------------------------------------------------
sib_uniq <- data.frame(get(load(paste0(r_dir, "sib_uniq.Rdata"))))
nrow(sib_uniq)   # 3,397,292

syskon_need_ped <- setdiff(sib_uniq$LopNrSyskon , c(index_ped$LopNr, barn_ped$LopNr, syskonbarn_ped$LopNr))     # pedigree only need to be built for siblings which are not index, barn, and syskonbarn
length(syskon_need_ped)                        # 675,390

syskon <- data.frame(get(load(paste0(r_dir, "tove_lev_koppl_index_syskon.Rdata"))))       
index_syskon <- syskon[syskon$LopNrSyskon %in% syskon_need_ped, c("LopNr","LopNrSyskon","SyskonTyp")]
nrow(index_syskon)                             # 1,116,082

parents <- c("Far","Mor")
for (parent_n in 1:2){
	syskon_type <- c(paste0("HalvHelsyskon",parents[parent_n]), paste0("Halvsyskon",parents[parent_n]), "Helsyskon")
	syskon_index <- index_syskon[index_syskon$SyskonTyp %in% syskon_type, c("LopNr","LopNrSyskon")]     
	index_parent <- index_ped[index_ped$LopNr %in% syskon_index$LopNr, c("LopNr",paste0("LopNr",parents[parent_n]))]  
	syskon_parent <- merge(syskon_index, index_parent, by="LopNr")
	syskon_parent <- unique(syskon_parent[ ,c("LopNrSyskon",paste0("LopNr",parents[parent_n]))])
	check <- anyDuplicated(syskon_parent$LopNrSyskon)
	print(paste0(parent_n, " is " ,check))
	if (parent_n==1){syskon_parent_one <- syskon_parent}
}

syskon_ped <- merge(syskon_parent_one, syskon_parent, by="LopNrSyskon", all=T)
nrow(syskon_ped)  # 675,390

syskon_basic <- sib_uniq[sib_uniq$LopNrSyskon %in% syskon_ped$LopNrSyskon, c("LopNrSyskon", "SyskonKon", "SyskonFodelseAr")]

syskon_ped <- merge(syskon_ped, syskon_basic, by="LopNrSyskon")
nrow(syskon_ped)  # 675,390
sum(is.na(syskon_ped$LopNrFar))  # 76,817
sum(is.na(syskon_ped$LopNrMor))  # 74,800
colnames(syskon_ped) <- c("LopNr","LopNrFar","LopNrMor","Kon","FodelseAr")



############################################
#               Combine                    #
############################################

ped_raw <- rbind(barn_ped, barn_bb_ped, syskonbarn_ped, index_ped, syskon_ped)
nrow(ped_raw)  # 10,719,789
length(unique(ped_raw$LopNr))   # 6,830,320 


# Extract the unique ones with as more non-NA as possible
ped <- ped_raw %>% group_by(LopNr) %>% summarize(LopNrFar=LopNrFar[which(is.na(LopNrFar)==F)[1]],
                                                 LopNrMor=LopNrMor[which(is.na(LopNrMor)==F)[1]],
                                                 Kon=Kon[which(is.na(Kon)==F)[1]], 
                                                 FodelseAr=FodelseAr[which(is.na(FodelseAr)==F)[1]])                                  

nrow(ped)  # 9,723,423 
save(ped, file=paste0(r_dir,"Pedigree.Rdata"))


