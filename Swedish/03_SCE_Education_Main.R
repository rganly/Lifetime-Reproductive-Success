## This script is to summarize education registry and extract the highest education level


# Input: "tove_lev_hog_gymn_1989_1977.Rdata", "tove_lev_lisa_{1990..2017}.Rdata", "tove_lev_koppl_index_foraldrar.Rdata"
# Output: "Education_1977_2017.Rdata", "edu_high.Rdata", "index_edu.Rdata"
# Comments: 


setwd("/home/aoxing/DSGE_LRS/out/registry_edit/")
r_dir <- "/home/aoxing/DSGE_LRS/input/r_files/"

require(gridExtra)
library(tidyverse)

'%!in%' <- function(x,y)!('%in%'(x,y))



#######################################
#   2 batches of education registry   #
#######################################

# 1977-1989
lisa_1977_1989 <- as.data.frame(get(load(paste0(r_dir, "tove_lev_hog_gymn_1989_1977.Rdata"))))
dim(lisa_1977_1989)    # 1955027       5


# 1990-2017
lisa_1990_2017 <- NULL
for (i in 1990:2017){
	d <- as.data.frame(get(load(paste0(r_dir, "tove_lev_lisa_", i, ".Rdata"))))
	d[ ,"Ar"] <- i
	colnames(d) <- tolower(colnames(d))
	d <- d[ ,c("lopnr", "ar", "sun2000niva", "sun2000inr", "sun2000niva_old")]
	print(paste0("Year ", i, " have ", nrow(d), " LISA records."))	
	lisa_1990_2017 <- rbind(lisa_1990_2017, d)
}
colnames(lisa_1990_2017) <- colnames(lisa_1977_1989)


# Combine: 1977-2017
edu <- rbind(lisa_1977_1989, lisa_1990_2017)
edu[ ,"ISCED97"] <- sub(9, 0, substr(edu[,"SUN2000Niva"],1,1))
nrow(edu)   #  
save(edu, file=paste0(r_dir, "Education_1977_2017.Rdata"))




#######################################
#     Hihgest education achieved      #
#######################################

# higest education level for individuals with education info
edu_e <- edu[,c("LopNr","ISCED97")]
edu_e <- edu_e[order(edu_e$LopNr, edu_e$ISCED97), ]     # order by id and education level, "aggregate" could be faster
edu_h <- by(edu_e, edu_e["LopNr"], tail, n=1)                           
edu_high <- do.call("rbind", edu_h)                    
nrow(edu_high)     # 7,834,277  


# add ISCED97 & EduYears
Edu_lst <- matrix(c(0,"Pre-primary education",1,
                    1,"Primary education or first stage of basic education",7,
                    2,"Lower secondary or second stage of basic education",10,
                    3,"(Upper) secondary education",13,
                    4,"Post-secondary non-tertiary education",15,
                    5,"First stage of tertiary education (not leading directly to an advanced research qualification)",19,
                    6,"Second stage of tertiary education (leading to an advanced research qualification, e.g. a Ph.D.)",22), byrow=T, ncol=3, nrow=7)
                    
colnames(Edu_lst) <- c("ISCED97","Definition","EduYears")
Edu_lst <- Edu_lst[,c("ISCED97","EduYears")]
Edu_lst


edu_high <- merge(edu_high, Edu_lst, by="ISCED97")
nrow(edu_high)      # 7,834,277
edu_high <- edu_high[ ,c("LopNr","ISCED97","EduYears")]
save(edu_high, file=paste0(r_dir, "edu_high.Rdata"))




###########################################
#   ISCED97 & EduYears for index person   #
###########################################

# index person's parents
index_ped <- as.data.frame(get(load(paste0(r_dir, "tove_lev_koppl_index_foraldrar.Rdata"))))   
nrow(index_ped)                               # 2,893,654 indexperson, where 55,961 have father as NA, 16,083 have mother as NA
index_ped <- index_ped[, c("LopNr", "LopNrFar", "LopNrMor")]


index_edu <- merge(index_ped, edu_high, by="LopNr", all.x=T)
nrow(index_edu)
colnames(index_ped) <- c("LopNrMor", "LopNrFar", "LopNr", "FarEdu", "MorEdu", "ISCED97.Far", "EduYears.Far", "ISCED97.Mor", "EduYears.Mor")
index_ped[is.na(index_ped)] <- NA   
index_ped[,"ISCED97.Far"] <- ifelse(is.na(index_ped$ISCED97.Far), "-1", index_ped$ISCED97.Far)
index_ped[,"ISCED97.Mor"] <- ifelse(is.na(index_ped$ISCED97.Mor), "-1", index_ped$ISCED97.Mor)

table(is.na(index_ped$ISCED97.Far) + is.na(index_ped$ISCED97.Mor))
#       0       1       2 
# 2501458  311661   80535


index_edu[] <- lapply(index_edu, as.character)
index_edu[,"ISCED97"] <- ifelse(is.na(index_edu$ISCED97), "-1", index_edu$ISCED97)
index_edu[,"EduYears"] <- ifelse(is.na(index_edu$EduYears), "-1", index_edu$EduYears)
index_edu[,"EduYears.Far"] <- ifelse(is.na(index_edu$EduYears.Far), "-1", index_edu$EduYears.Far)
index_edu[,"EduYears.Mor"] <- ifelse(is.na(index_edu$EduYears.Mor), "-1", index_edu$EduYears.Mor)


# Father and Mother
cor(as.numeric(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97.Mor>0, "ISCED97.Far"]), as.numeric(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97.Mor>0, "ISCED97.Mor"]), use="pairwise.complete.obs")    # 0.4782035
cor(as.numeric(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97.Mor>0, "EduYears.Far"]), as.numeric(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97.Mor>0, "EduYears.Mor"]), use="pairwise.complete.obs")  # 0.4770058

# Father and index
cor(as.numeric(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97>0, "ISCED97.Far"]), as.numeric(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97>0, "ISCED97"]), use="pairwise.complete.obs")    # 0.3179813
cor(as.numeric(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97>0, "EduYears.Far"]), as.numeric(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97>0, "EduYears"]), use="pairwise.complete.obs")  # 0.3145961

# Mother and index
cor(as.numeric(index_edu[index_edu$ISCED97.Mor>0 & index_edu$ISCED97>0, "ISCED97.Mor"]), as.numeric(index_edu[index_edu$ISCED97.Mor>0 & index_edu$ISCED97>0, "ISCED97"]), use="pairwise.complete.obs")    # 0.3220415
cor(as.numeric(index_edu[index_edu$ISCED97.Mor>0 & index_edu$ISCED97>0, "EduYears.Mor"]), as.numeric(index_edu[index_edu$ISCED97.Mor>0 & index_edu$ISCED97>0, "EduYears"]), use="pairwise.complete.obs")  # 0.319645

index_edu[,"ISCED97.Parent"] <- max(index_edu[,"ISCED97.Far"], index_edu[,"ISCED97.Mor"])
index_edu[,"EduYears.Parent"] <- max(index_edu[,"EduYears.Far"], index_edu[,"EduYears.Mor"])
index_edu$ISCED97.Parent <- ifelse(as.numeric(index_edu$ISCED97.Far) > as.numeric(index_edu$ISCED97.Mor), index_edu$ISCED97.Far, index_edu$ISCED97.Mor)
index_edu$EduYears.Parent <- ifelse(as.numeric(index_edu$EduYears.Far) > as.numeric(index_edu$EduYears.Mor), index_edu$EduYears.Far, index_edu$EduYears.Mor)

index_edu <- index_edu[,c("LopNr","LopNrMor","LopNrFar",
                          "ISCED97","ISCED97.Far","ISCED97.Mor","ISCED97.Parent",
                          "EduYears","EduYears.Far","EduYears.Mor","EduYears.Parent")]

save(index_edu, file=paste0(r_dir, "index_edu.Rdata"))





############################################################
#    For which population we have education information    #
############################################################

parent <- unique(c(index_ped$LopNrFar, index_ped$LopNrMor))
length(parent)  # 3007499

index <- index_ped[, c("LopNr")]
length(index)   # 2893654

length(intersect(index, edu_high$LopNr))   # 2,779,402
length(intersect(parent, edu_high$LopNr))  # 2,756,003

length(intersect(unique(index_ped$LopNrFar), edu_high$LopNr))  # 1,329,471
length(intersect(unique(index_ped$LopNrMor), edu_high$LopNr))  # 1,426,533

index_ped[,"LopNrFar"] <- ifelse(is.na(index_ped$LopNrFar), "999999999", index_ped$LopNrFar)
index_ped[,"LopNrMor"] <- ifelse(is.na(index_ped$LopNrMor), "999999999", index_ped$LopNrMor)




#----------------------------------------------
index_edu <- merge(index_ped, edu_high, by="LopNr", all.x=T)
dim(index_edu)


index_edu[] <- lapply(index_edu, as.character)
index_edu[,"ISCED97"] <- ifelse(is.na(index_edu$ISCED97), "-1", index_edu$ISCED97)
index_edu[,"EduYears"] <- ifelse(is.na(index_edu$EduYears), "-1", index_edu$EduYears)
index_edu[,"EduYears.Far"] <- ifelse(is.na(index_edu$EduYears.Far), "-1", index_edu$EduYears.Far)
index_edu[,"EduYears.Mor"] <- ifelse(is.na(index_edu$EduYears.Mor), "-1", index_edu$EduYears.Mor)



index_edu$ISCED97.Parent <- ifelse(as.numeric(index_edu$ISCED97.Far) > as.numeric(index_edu$ISCED97.Mor), index_edu$ISCED97.Far, index_edu$ISCED97.Mor)
index_edu$EduYears.Parent <- ifelse(as.numeric(index_edu$EduYears.Far) > as.numeric(index_edu$EduYears.Mor), index_edu$EduYears.Far, index_edu$EduYears.Mor)


index_edu <- index_edu[,c("LopNr","LopNrMor","LopNrFar",
                          "ISCED97","ISCED97.Far","ISCED97.Mor","ISCED97.Parent",
                          "EduYears","EduYears.Far","EduYears.Mor","EduYears.Parent")]

save(index_edu, file=paste0(r_dir, "index_edu.Rdata"))



##============================
# Father and Mother
cor(as.numeric(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97.Mor>0, "ISCED97.Far"]), as.numeric(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97.Mor>0, "ISCED97.Mor"]), use="pairwise.complete.obs")    # 0.4782035
cor(as.numeric(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97.Mor>0, "EduYears.Far"]), as.numeric(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97.Mor>0, "EduYears.Mor"]), use="pairwise.complete.obs")  # 0.4770058

# Father and index
cor(as.numeric(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97>0, "ISCED97.Far"]), as.numeric(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97>0, "ISCED97"]), use="pairwise.complete.obs")    # 0.3179813
cor(as.numeric(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97>0, "EduYears.Far"]), as.numeric(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97>0, "EduYears"]), use="pairwise.complete.obs")  # 0.3145961

# Mother and index
cor(as.numeric(index_edu[index_edu$ISCED97.Mor>0 & index_edu$ISCED97>0, "ISCED97.Mor"]), as.numeric(index_edu[index_edu$ISCED97.Mor>0 & index_edu$ISCED97>0, "ISCED97"]), use="pairwise.complete.obs")    # 0.3220415
cor(as.numeric(index_edu[index_edu$ISCED97.Mor>0 & index_edu$ISCED97>0, "EduYears.Mor"]), as.numeric(index_edu[index_edu$ISCED97.Mor>0 & index_edu$ISCED97>0, "EduYears"]), use="pairwise.complete.obs")  # 0.319645


table(is.na(index_edu$ISCED97.Far) + is.na(index_edu$ISCED97.Mor))
#       0       1       2 
# 2501458  311661   80535


nrow(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97.Mor>0, ])   # 2,479,253
nrow(index_edu[index_edu$ISCED97.Far>0 & index_edu$ISCED97.Mor>0, ])/nrow(index_edu)   # 0.8567897
	




