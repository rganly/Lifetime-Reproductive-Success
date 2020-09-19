## This script is to count N of children & grandchildren & sib_children for each birth year 


# Input: "indexW.Rdata", "tove_lev_koppl_index_barn.Rdata", "child_grandchild_uniq.Rdata", 
# Output: 
# Comments: 


setwd("/home/aoxing/DSGE_LRS/out/registry_edit/")
r_dir <- "/home/aoxing/DSGE_LRS/input/r_files/"

library(tidyverse)
'%!in%' <- function(x,y)!('%in%'(x,y))



############################################
#    Read in demo for each population      #
############################################

## Index person, children, and grandchildren -------------------------------
# index person
index <- data.frame(get(load(paste0(r_dir, "tove_lev_index.Rdata"))))
index <- index[,c("LopNr", "AterPnr", "FodelseAr", "FodelseLandNamn", "FodelseLan", "FodelseKommun", "Kon")] 
nrow(index)   # 2,893,654
save(index, file=paste0(r_dir,"DEMO_index.Rdata"))

indexW <- data.frame(get(load(paste0(r_dir, "indexW.Rdata"))))
nrow(indexW)  # 2,555,541


# Children
child <- data.frame(get(load(paste0(r_dir, "tove_lev_koppl_index_barn.Rdata"))))
nrow(child)
child[,"AterPnr"] <- NA
child_uniq <- child[!duplicated(child$LopNrBarn),c("LopNrBarn","AterPnr","BarnFodelseAr","BarnFodelseLand","BarnFodelseLan","BarnFodelseKommun","BarnKon")]
colnames(child_uniq) <- colnames(index)
nrow(child_uniq)  # 2,900,503
save(child_uniq, file=paste0(r_dir,"DEMO_child.Rdata"))


# Grandchildren
grandchild <- data.frame(get(load(paste0(r_dir, "tove_lev_koppl_index_barnbarn.Rdata"))))  
nrow(grandchild)        # 1,241,918
grandchild_uniq <- grandchild[!duplicated(grandchild[ ,"LopNrBarnBarn"]),c("LopNrBarnBarn","BarnBarnFodelseAr","BarnBarnFodelseLan","BarnBarnFodelseKommun","BarnBarnKon")]
save(grandchild_uniq, file=paste0(r_dir, "grandchild_uniq.Rdata")) 

grandchild_uniq[,c("AterPnr","FodelseLandNamn")] <- NA
grandchild_uniq <- grandchild_uniq[,c("LopNrBarnBarn","AterPnr","BarnBarnFodelseAr","FodelseLandNamn","BarnBarnFodelseLan","BarnBarnFodelseKommun","BarnBarnKon" )]
colnames(grandchild_uniq) <- colnames(index)
nrow(grandchild_uniq)   # 591,843
save(grandchild_uniq, file=paste0(r_dir,"DEMO_grandchild.Rdata"))



## Siblings and siblings' children -------------------------------
# sibling
sib <- data.frame(get(load(paste0(r_dir, "sib_uniq.Rdata"))))     
nrow(sib)      # 3,397,292   
sib[,c("AterPnr","FodelseLandNamn")] <- NA
sib <- sib[,c("LopNrSyskon","AterPnr","SyskonFodelseAr","FodelseLandNamn","SyskonFodelseLan","SyskonFodelseKommun","SyskonKon")]
colnames(sib) <- colnames(index)
save(sib, file=paste0(r_dir,"DEMO_sib.Rdata"))


# sibling's children
sib_sibchild_uniq <- data.frame(get(load(paste0(r_dir, "sib_sibchild_uniq.Rdata")))) 
nrow(sib_sibchild_uniq)      # 5,540,829                                    
sibchild <- sib_sibchild_uniq[!duplicated(sib_sibchild_uniq[ ,"LopNrSyskonBarn"]), c("LopNrSyskonBarn","SyskonBarnFodelseAr","SyskonBarnFodelseLan","SyskonBarnFodelseKommun","SyskonBarnKon")]
sibchild[,c("AterPnr","FodelseLandNamn")] <- NA
sibchild <- sibchild[,c("LopNrSyskonBarn","AterPnr","SyskonBarnFodelseAr","FodelseLandNamn","SyskonBarnFodelseLan","SyskonBarnFodelseKommun","SyskonBarnKon")]
colnames(sibchild) <- colnames(index)
nrow(sibchild)   # 3,658,399
save(sibchild, file=paste0(r_dir,"DEMO_sibchild.Rdata"))



## parents of index person, children, and siblings -------------------------------
# parents
index_parent <- data.frame(get(load(paste0(r_dir, "tove_lev_koppl_index_foraldrar.Rdata"))))
nrow(index_parent)    # 2,893,654

father <- index_parent[is.na(index_parent$LopNrFar)==F, c("LopNrFar","FodelseLandNamnFar")]
father[,"Kon"] <- 1
colnames(father) <- c("LopNr","FodelseLandNamn","Kon")
nrow(father)   # 2837693

mother <- index_parent[is.na(index_parent$LopNrMor)==F, c("LopNrMor","FodelseLandNamnMor")]
mother[,"Kon"] <- 2
colnames(mother) <- colnames(father)
nrow(mother)   # 2,877,571

parent_index <- rbind(father, mother)
parent_index[,c("AterPnr","FodelseAr","FodelseLan", "FodelseKommun")] <- NA
parent_index <- parent_index[,c("LopNr", "AterPnr", "FodelseAr", "FodelseLandNamn", "FodelseLan", "FodelseKommun", "Kon")]
nrow(parent_index)  # 5,715,264
save(parent_index, file=paste0(r_dir,"DEMO_parent_index.Rdata"))


# index person children' parents
child_parent <- data.frame(get(load(paste0(r_dir,"tove_lev_koppl_barn_foraldrar.Rdata")))) 
nrow(child_parent)  # 2,900,503
child_parent[,"Kon"] <- NA

child_father <- child_parent[is.na(child_parent$LopNrFar)==F, c("LopNrFar","Kon")]
child_father[,"Kon"] <- 1
colnames(child_father) <- c("LopNr","Kon")
nrow(child_father)   # 2,870,373

child_mother <- child_parent[is.na(child_parent$LopNrMor)==F, c("LopNrMor","Kon")]
child_mother[,"Kon"] <- 2
colnames(child_mother) <- c("LopNr","Kon")
nrow(child_mother)   # 2,898,778

parent_child <- rbind(child_father, child_mother)
parent_child[,c("AterPnr","FodelseAr","FodelseLandNamn","FodelseLan","FodelseKommun")] <- NA
parent_child <- parent_child[,c("LopNr", "AterPnr", "FodelseAr", "FodelseLandNamn", "FodelseLan", "FodelseKommun", "Kon")]
nrow(parent_child)  # 5,769,151
save(parent_child, file=paste0(r_dir,"DEMO_parent_child.Rdata"))


# siblings' parents
sib_parent <- data.frame(get(load(paste0(r_dir,"tove_lev_koppl_sysbarn_foraldr.Rdata")))) 
nrow(sib_parent)  # 3,658,399
sib_parent[,"Kon"] <- NA

sib_father <- sib_parent[is.na(sib_parent$LopNrFar)==F, c("LopNrFar","Kon")]
sib_father[,"Kon"] <- 1
colnames(sib_father) <- c("LopNr","Kon")
nrow(sib_father)   # 3,621,363

sib_mother <- sib_parent[is.na(sib_parent$LopNrMor)==F, c("LopNrMor","Kon")]
sib_mother[,"Kon"] <- 2
colnames(sib_mother) <- c("LopNr","Kon")
nrow(sib_mother)   # 3,656,460

parent_sib <- rbind(sib_father, sib_mother)
parent_sib[,c("AterPnr","FodelseAr","FodelseLandNamn","FodelseLan","FodelseKommun")] <- NA
parent_sib <- parent_sib[,c("LopNr", "AterPnr", "FodelseAr", "FodelseLandNamn", "FodelseLan", "FodelseKommun", "Kon")]
nrow(parent_sib)  # 7,277,823
save(parent_sib, file=paste0(r_dir,"DEMO_parent_sib.Rdata"))


