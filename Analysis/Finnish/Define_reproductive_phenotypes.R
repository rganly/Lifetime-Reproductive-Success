## Define reproductive phenotypes, in order to use the same definitions and QC rules across diferent analyses
# Data dictionary: https://docs.google.com/spreadsheets/d/16E40mY9Avffnq6u5DhtQbisohG3ctzmU/edit#gid=947231392

setwd("/homes/aliu/DSGE_LRS/output/registry_edit/REGRESSION/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"

library(dplyr)
'%!in%' <- function(x,y)!('%in%'(x,y))




#----------------------------
# outcomeName   # c("childless", "parity", "NEB", "has_spouse", "age at first birth")
# recurrent     # 1 for recurrent-event and time-varing coxph model, 0 for non-recurrent time-varing coxph model.
# includeZero   # whether only keep individuals with 0 event of outcome or not.
#----------------------------




################################################
#              read in data                    # 
################################################

indexW <- data.frame(get(load(paste0(r_dir, "indexW_5682_everyone.Rdata")))) %>% 
      select(index_id, index_sex, parent_id, index_birth_date, endfollowup4550_date, endfollowup4550_age)
nrow(indexW)  # 1,723,014

indexW_5682_everyone_child <- data.frame(get(load(paste0(r_dir, "indexW_5682_everyone_child.Rdata"))))
indexW_5682_everyone_endpoint <- data.frame(get(load(paste0(r_dir, "indexW_5682_everyone_endpoint.Rdata"))))
indexW_5682_everyone_marriage <- data.frame(get(load(paste0(r_dir, "indexW_5682_everyone_marriage.Rdata"))))




################################################
#                Giving birth                  #
################################################

## NofChildren, parity, childless, and AgeFirstBirth ------------------------
# NofChildren and parity is different at includeZero or not
# childless and AgeFirstBirth is different at includeZero or not

# ------------------------
for (outcomeName in c("parity","childless","AgeFirstBirth")) {
	outcome_age_dat <- data.frame(get(load(paste0(r_dir, "indexW_5682_everyone_child.Rdata")))) %>% rename(outcome_age="index_birth_age") 
	if (outcomeName %in% c("childless","AgeFirstBirth")) { outcome_age_dat <- outcome_age_dat %>% group_by(index_id) %>% slice_min(outcome_age,1) %>% distinct(index_id, outcome_age,.keep_all=F)}
	print(paste0(outcomeName, ": ", nrow(outcome_age_dat)))  # 1,546,175
	save(outcome_age_dat, file=paste0(r_dir, "indexW_5682_everyone_",outcomeName,".Rdata"))
}
# [1] "parity: 3011597"
# [1] "childless: 1295105"
# [1] "AgeFirstBirth: 1295105"


# ------------------------
# (???how to deal with is.na(child_ART)), which are children not registered in medical birth registry) --------------------
for (outcomeName in c("parity_NoART","childless_NoART","AgeFirstBirth_NoART")) {
	outcome_age_dat <- data.frame(get(load(paste0(r_dir, "indexW_5682_everyone_child.Rdata")))) %>% rename(outcome_age="index_birth_age") %>% 
	      filter(child_ART==0|is.na(child_ART)) 
	if (outcomeName %in% c("childless_NoART","AgeFirstBirth_NoART")) { outcome_age_dat <- outcome_age_dat %>% group_by(index_id) %>% slice_min(outcome_age,1) %>% distinct(index_id, outcome_age,.keep_all=F)}
	print(paste0(outcomeName, ": ", nrow(outcome_age_dat)))  # 1,546,175
	save(outcome_age_dat, file=paste0(r_dir, "indexW_5682_everyone_",outcomeName,".Rdata"))
}
# "parity_NoART: 2980345" (31252 were removed)
# "childless_NoART: 1282391" (12714 were removed)
# "AgeFirstBirth_NoART: 1282391" (12714 were removed)


# ------------------------
for (outcomeName in c("parity_NoDeath28","childless_NoDeath28","AgeFirstBirth_NoDeath28")) {
	outcome_age_dat <- data.frame(get(load(paste0(r_dir, "indexW_5682_everyone_child.Rdata")))) %>% rename(outcome_age="index_birth_age") %>% 
	      filter(child_death_before28days==0) 
	if (outcomeName %in% c("childless_NoDeath28","AgeFirstBirth_NoDeath28")) { outcome_age_dat <- outcome_age_dat %>% group_by(index_id) %>% slice_min(outcome_age,1) %>% distinct(index_id, outcome_age,.keep_all=F)}
	print(paste0(outcomeName, ": ", nrow(outcome_age_dat)))  # 1,546,175
	save(outcome_age_dat, file=paste0(r_dir, "indexW_5682_everyone_",outcomeName,".Rdata"))
}
# "parity_NoDeath28: 3004345" (7252 were removed)
# "childless_NoDeath28: 1294790" (12399 were removed)
# "AgeFirstBirth_NoDeath28: 1294790" (12399 were removed)


# ------------------------
for (outcomeName in c("parityNoARTNoDeath28","childlessNoARTNoDeath28","AgeFirstBirthNoARTNoDeath28")) {
	outcome_age_dat <- data.frame(get(load(paste0(r_dir, "indexW_5682_everyone_child.Rdata")))) %>% rename(outcome_age="index_birth_age") %>% 
	      filter((child_ART==0|is.na(child_ART)) & child_death_before28days==0) 
	if (outcomeName %in% c("childlessNoARTNoDeath28","AgeFirstBirthNoARTNoDeath28")) { outcome_age_dat <- outcome_age_dat %>% group_by(index_id) %>% slice_min(outcome_age,1) %>% distinct(index_id, outcome_age,.keep_all=F)}
	print(paste0(outcomeName, ": ", nrow(outcome_age_dat)))  # 1,546,175
	save(outcome_age_dat, file=paste0(r_dir, "indexW_5682_everyone_",outcomeName,".Rdata"))
}
# "parityNoARTNoDeath28: 2973174" (38423 were removed)
# "childlessNoARTNoDeath28: 1282076" (13029 were removed)
# "AgeFirstBirthNoARTNoDeath28: 1282076" (13029 were removed)




################################################
#                Mate choice                   #
################################################

## NofChildren, parity, childless, and AgeFirstBirth ------------------------
# spouseless and AgeFirstSpouse is different at includeZero or not

for (outcomeName in c("spouseless","AgeFirstSpouse")) {

	outcome_age_dat <- data.frame(get(load(paste0(r_dir, "indexW_5682_everyone_marriage.Rdata")))) %>% rename(outcome_age="index_marry_age") %>% 
	      distinct(index_id, outcome_age,.keep_all=F) %>% 
	      group_by(index_id) %>% slice_min(outcome_age,1)
	
	print(paste0(outcomeName, ": ", nrow(outcome_age_dat)))  # 1,546,175
	print(head(outcome_age_dat))
	save(outcome_age_dat, file=paste0(r_dir, "indexW_5682_everyone_",outcomeName,".Rdata"))
	
}
# "spouseless: 1190827"
# "AgeFirstSpouse: 1190827"






