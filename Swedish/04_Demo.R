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


## Spouses of index person and siblings -------------------------------
spouse <- data.frame(get(load(paste0(r_dir,"Spouse_1977_2017.Rdata"))))
nrow(spouse)  # 37,767,025
spouse <- spouse[!duplicated(spouse$LopNrSamh),c("LopNrSamh","FodelseLanSamh","FodelseKommunSamh","KonSamh")]
nrow(spouse)  # 3,332,240
spouse[,c("AterPnr","FodelseAr","FodelseLandNamn")] <- NA
spouse <- spouse[,c("LopNrSamh", "AterPnr", "FodelseAr", "FodelseLandNamn", "FodelseLanSamh", "FodelseKommunSamh", "KonSamh")]
colnames(spouse) <- colnames(index)
save(spouse, file=paste0(r_dir,"DEMO_spouse.Rdata"))




############################################
#      Combine and format demo             #
############################################
system("ls -alrt /home/aoxing/DSGE_LRS/input/r_files/DEMO*.Rdata")

# Combine all sources
demo_raw <- rbind(index,child_uniq,grandchild_uniq, sib,sibchild, parent_index,parent_child,parent_sib, spouse)
nrow(demo_raw)  # 35,536,169
length(unique(demo_raw$LopNr))   # 9,723,423
demo_raw[demo_raw==""] <- NA
save(demo_raw, file=paste0(r_dir,"demo_raw.Rdata"))


# Extract the unique ones with as more non-NA as possible
demo <- demo_raw %>% group_by(LopNr) %>% summarize(AterPnr=AterPnr[which(is.na(AterPnr)==F)[1]],
                                                   FodelseAr=FodelseAr[which(is.na(FodelseAr)==F)[1]],
                                                   FodelseLandNamn=FodelseLandNamn[which(is.na(FodelseLandNamn)==F)[1]], 
                                                   FodelseLan=FodelseLan[which(is.na(FodelseLan)==F)[1]],
                                                   FodelseKommun=FodelseKommun[which(is.na(FodelseKommun)==F)[1]],
                                                   Kon=Kon[which(is.na(Kon)==F)[1]])                                  

nrow(demo)  # 9,723,423 
demo <- data.frame(demo)
save(demo, file=paste0(r_dir,"demo.Rdata"))



############################################
#      Which population?                   #
############################################

demo[,"is_index"] <- as.numeric(demo[,"LopNr"] %in% index[,"LopNr"])
demo[,"is_indexW"] <- as.numeric(demo[,"LopNr"] %in% indexW[,"LopNr"])
demo[,"is_child"] <- as.numeric(demo[,"LopNr"] %in% child_uniq[,"LopNr"])
demo[,"is_grandchild"] <- as.numeric(demo[,"LopNr"] %in% grandchild_uniq[,"LopNr"])
demo[,"is_sib"] <- as.numeric(demo[,"LopNr"] %in% sib[,"LopNr"])
demo[,"is_sibchild"] <- as.numeric(demo[,"LopNr"] %in% sibchild[,"LopNr"])
demo[,"is_parent_index"] <- as.numeric(demo[,"LopNr"] %in% parent_index[,"LopNr"])
demo[,"is_parent_child"] <- as.numeric(demo[,"LopNr"] %in% parent_child[,"LopNr"])
demo[,"is_parent_sib"] <- as.numeric(demo[,"LopNr"] %in% parent_sib[,"LopNr"])
demo[,"is_spouse_index"] <- as.numeric(demo[,"LopNr"] %in% spouse_index[,"LopNr"])
demo[,"is_spouse_sib"] <- as.numeric(demo[,"LopNr"] %in% spouse_sib[,"LopNr"])



############################################
#      Add Death date                      #
############################################

death <- data.frame(get(load(paste0(r_dir,"tove_lev_doddatum.Rdata"))))
nrow(death)  # 1,301,514
colnames(death) <- c("LopNr","death_date")

demo <- merge(demo, death, by="LopNr", all.x=T)
nrow(demo)



############################################
#      Add Migration                       #
############################################

migra <- data.frame(get(load(paste0(r_dir,"tove_lev_migrationer.Rdata"))))
nrow(migra)  # 2,066,867

demo[,"immigration"] <- ifelse(demo$LopNr %in% migra[migra$Posttyp=="Inv","LopNr"], 1, 0)
demo[,"emigration"] <- ifelse(demo$LopNr %in% migra[migra$Posttyp=="Utv","LopNr"], 1, 0)



############################################
#      Add father and mother               #
############################################

ped <- data.frame(get(load(paste0(r_dir, "ped_all.RData"))))[,c("LopNr","LopNrFar","LopNrMor")]
nrow(ped)   # 6830320

demo <- merge(demo, ped, by="LopNr", all.x=T)
nrow(demo)



############################################
#      Add education                       #
############################################

edu_high <- get(load(paste0(r_dir, "edu_high.Rdata")))
nrow(edu_high)
head(edu_high)

demo <- merge(demo, edu_high, by="LopNr", all.x=T)
nrow(demo)



############################################
#      Add income                          #
############################################


# demo <- data.frame(get(load(paste0(r_dir, "demo.Rdata"))))

demo <- demo[,c("LopNr","LopNrFar","LopNrMor","AterPnr","FodelseAr","FodelseLandNamn","FodelseLan","FodelseKommun","Kon","immigration","emigration","death_date",
                "is_index","is_indexW","is_child","is_grandchild","is_sib","is_sibchild","is_parent_index","is_parent_child","is_parent_sib","is_spouse_index","is_spouse_sib",
                "income_Age2535_max","income_Age2535_mean","income_Age5060_max","income_Age5060_mean","EduYears","ISCED97")]



save(demo, file=paste0(r_dir,"Demographic.Rdata"))







