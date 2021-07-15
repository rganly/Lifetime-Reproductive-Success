
## Data preparation-------------------- 
# Data dictionary: https://docs.google.com/spreadsheets/d/16E40mY9Avffnq6u5DhtQbisohG3ctzmU/edit#gid=947231392

# Index person: birth year 1956-1982, born in Finland, not emigrated from Finland, alive until age 16, have same-sex full sib

# QC criteria
# Start of follow-up is age 16: Index person dead before 16 were removed. Children born before age 16 were considered as born at age 16.
# End of follow-up is age 45/50: For female/male index person alive until 45/50, the age at end of follow-up is set to 45/50. Children born after 45/50 were removed.
# Index person with death_date prior to child_birth_date were removed


setwd("/homes/aliu/DSGE_LRS/output/registry_edit/REGRESSION/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"

library(dplyr)
'%!in%' <- function(x,y)!('%in%'(x,y))



############################################
#             Read in data                 #
############################################

# Index person (born 1956-1982)  
index <- data.frame(get(load(paste0(r_dir, "index.Rdata"))))    # 2,365,707 indexperson
table(index$SUKUPUOLI)        # 1,260,350 males and 1,105,357 females


# Children of index person  
child <- data.frame(get(load(paste0(r_dir, "child.Rdata")))) 
length(unique(child$KANTAHENKILON_TNRO))    # 1,555,839 indexperson with children
length(unique(child$SUKULAISEN_TNRO))       # 2,060,722 children


# Death registry
death <- data.frame(get(load(paste0(r_dir, "kuolemansyyt_u1477_a.Rdata")))) %>% mutate(death_year=substr(kuolpvm,1,4)) %>% select(TNRO, death_year) 
dim(death) # 


# Marriage registry
marriage <- data.frame(get(load(paste0(r_dir, "thl2019_804_avio.Rdata")))) 
dim(marriage)  # 1,743,787  8


# Endpoint first diagenose
ry_first <- data.frame(get(load(paste0(r_dir, "ry_first_COMPLETE.Rdata"))))
dim(ry_first)  # 55360647 4


# Independent endpoints
res_ep_info <- read.table("/homes/aliu/DSGE_LRS/input/IndependentEndpoints_Info.tsv", sep="\t", header=T)
dim(res_ep_info)   # 1742    8
save(res_ep_info, file=paste0(r_dir, "Endpoint_Info.Rdata"))


# demographic
demo <- data.frame(get(load(paste0(r_dir, "demo.Rdata"))))
dim(demo)


# pedgree
ped <- data.frame(get(load(paste0(r_dir, "ped_all_bdate.Rdata"))))
dim(ped)  # 5091592       5
dup <- ped[duplicated(ped$id),"id"]
ped <- ped %>% filter(id %!in% dup) #  5,091,284       5


# children's medical birth registry
mbr_c <- get(load(paste0(r_dir, "mbr_children_1987_2018.Rdata"))) %>% mutate(TNRO=as.character(TNRO))
dim(mbr_c)           # 1867462     137


# Mother's medical birth registry
mbr_m <- get(load(paste0(r_dir, "mbr_mothers_1987_2018.Rdata"))) %>% mutate(TNRO=as.character(TNRO))
dim(mbr_m)           # 1867462     137





###############################################
#            QC for index person              #
###############################################

# QC criteria
# Start of follow-up is age 16: Index person dead before 16 were removed.
# End of follow-up is age 45/50: For female/male index person alive until 45/50, the age at end of follow-up is set to 45/50. 

# birth year 1956-1982, born in Finland, not imigrated/emigrated from Finland, and alive until age 16
index %>% filter(SYNTYMAKOTIKUNTA==200) %>% nrow()   # 453,861 born outside of Finland
index %>% filter(SYNTYMAKOTIKUNTA==200 & AIDINKIELI=="fi") %>% nrow()   # 60,890 born outside of Finland but with Finnish as mother tongue 

indexW <- index %>% filter(SYNTYMAKOTIKUNTA!=200 & is.na(ULKOMAILLE_MUUTON_PV) & ULKOM_ASUINVALTIO=="" & ULKOM_ASUINVALTION_NIMI=="") %>%     # born in Finland and no emigration
      filter(substr(SUKULAISEN_SYNTYMAPV,1,4)>=1956 & substr(SUKULAISEN_SYNTYMAPV,1,4)<=1982) %>%                              # birth year 1956-1982
      filter(SUKULAISEN_KUOLINPV - SUKULAISEN_SYNTYMAPV>160000|is.na(SUKULAISEN_KUOLINPV)|is.na(SUKULAISEN_SYNTYMAPV)) %>%     # alive until 16
      select(-SUKUL_SUHDE, -VALIHENKILO1_TNRO, -VALIHENKILO2_TNRO, -SUKULAISEN_TNRO, -ULKOM_ASUINVALTIO, -ULKOM_ASUINVALTION_NIMI, -ULKOMAILLE_MUUTON_PV) %>% 
      rename(index_id="KANTAHENKILON_TNRO", index_sex="SUKUPUOLI", index_birth_date="SUKULAISEN_SYNTYMAPV", index_death_date="SUKULAISEN_KUOLINPV",
             mother_tongue="AIDINKIELI", birth_town="KOTI_KUNTA", birth_town_name="KOTIKUNNAN_NIMI", birth_municipality="SYNTYMAKOTIKUNTA", birth_municipality_name="SYNTYMKOTIKUNNAN_NIMI") %>% 
      inner_join(ped[,c("id","father_id","mother_id")], by=c("index_id"="id")) %>% 
      select(index_id, index_sex, father_id, mother_id, index_birth_date, index_death_date, mother_tongue, birth_town, birth_town_name, birth_municipality, birth_municipality_name)



# birth year 1956-1982; with both father_id and mother_id available 
indexW_5682_everyone <- indexW %>% filter(!is.na(father_id) & !is.na(mother_id)) %>% 
      mutate(parent_id=paste0(father_id,"_",mother_id,"_",index_sex)) %>% 
      mutate(index_death_date=ifelse(index_death_date %in% c(19951000,20120400, 19840400, 19790300), index_death_date+1, index_death_date)) %>% 
      mutate(endfollowup_date=ifelse(!is.na(index_death_date) & index_death_date<=20181231, index_death_date, 20181231)) %>% 
      select(index_id, index_sex, father_id, mother_id, parent_id, index_birth_date, index_death_date, endfollowup_date, mother_tongue, birth_town, birth_town_name, birth_municipality, birth_municipality_name)

indexW_5682_everyone <- indexW_5682_everyone %>% mutate(age_at_endfollowup=as.numeric(as.Date(as.character(endfollowup_date),"%Y%m%d") - as.Date(as.character(index_birth_date),"%Y%m%d"))/365.25) %>% 
      mutate(endfollowup4550_date=endfollowup_date) %>% 
      mutate(endfollowup4550_date=ifelse(age_at_endfollowup>=45 & index_sex==2, index_birth_date + 450000, endfollowup4550_date)) %>%  # wrong format, could be due to add or minus 15 days
      mutate(endfollowup4550_date=ifelse(age_at_endfollowup>=50 & index_sex==1, index_birth_date + 500000, endfollowup4550_date)) %>%  # wrong format, could be due to add or minus 15 days
      mutate(endfollowup4550_date=ifelse(endfollowup4550_date %in% c(20170229, 20130229, 20180229, 20090229, 20140229, 20100229, 20050229, 20060229, 20010229), endfollowup4550_date-1, endfollowup4550_date)) %>% 
      mutate(endfollowup4550_age=as.numeric(as.Date(as.character(endfollowup4550_date),"%Y%m%d") - as.Date(as.character(index_birth_date),"%Y%m%d"))/365.25) %>% 
      mutate(endfollowup4550_age=round(endfollowup4550_age,3), endfollowup4550_age=ifelse(endfollowup4550_age==50.001,50,endfollowup4550_age)) %>% 
      select(index_id, index_sex, father_id, mother_id, parent_id, index_birth_date, index_death_date, endfollowup_date, endfollowup4550_date, endfollowup4550_age, mother_tongue, birth_town, birth_town_name, birth_municipality, birth_municipality_name)





###############################################
#       Longitudinal data for child           #
###############################################

## Remove IDs where death date < child date----------------
# QC criteria
# Start of follow-up is age 16: Children born before age 16 were considered as born at age 16.
# End of follow-up is age 45/50: Children born after 45/50 were removed.
# Index person with death_date prior to child_birth_date were removed (men could dead 1 year before the birth of the child)

# Children of index person
indexW_5682_everyone_child <- child %>% rename(index_id="KANTAHENKILON_TNRO", child_id="SUKULAISEN_TNRO") %>%
      select(index_id, child_id) %>% unique() %>% 
      inner_join(indexW_5682_everyone[,c("index_id","index_sex","parent_id","index_birth_date","index_death_date","endfollowup4550_date")], by="index_id") %>% 
      inner_join(demo[,c("SUKULAISEN_TNRO","SUKUPUOLI","SUKULAISEN_SYNTYMAPV","SUKULAISEN_KUOLINPV")], by=c("child_id"="SUKULAISEN_TNRO")) %>%
      rename(child_sex="SUKUPUOLI", child_birth_date="SUKULAISEN_SYNTYMAPV", child_death_date="SUKULAISEN_KUOLINPV") %>% 
      mutate(age_at_birth=as.numeric(as.character(child_birth_date))-as.numeric(as.character(index_birth_date))) %>% 
      mutate(child_birth_date=ifelse(age_at_birth<160000 & age_at_birth>120000, index_birth_date+160000, child_birth_date), age_at_birth=ifelse(age_at_birth<160000 & age_at_birth>120000, 160000, age_at_birth))    # set age_at_birth to 160000 if children born before age 16

# remove index person with children born before birth date and with children born after death date
indexW_to_remove <- indexW_5682_everyone_child %>% filter((child_birth_date<index_birth_date)|(index_sex==2 & child_birth_date>index_death_date & !is.na(index_death_date))|(index_sex==1 & child_birth_date>(index_death_date+10000) & !is.na(index_death_date))) %>% select(index_id)
indexW_5682_everyone <- indexW_5682_everyone %>% filter(index_id %!in% indexW_to_remove$index_id)
# save(indexW_5682_everyone, file=paste0(r_dir, "indexW_5682_everyone.Rdata"))


indexW_5682_everyone_child <- indexW_5682_everyone_child %>% filter(index_id %!in% indexW_to_remove$index_id) %>% 
                        filter(child_birth_date<=endfollowup4550_date) %>% 
                        mutate(index_birth_age=round(as.numeric(as.Date(as.character(child_birth_date),"%Y%m%d") - as.Date(as.character(index_birth_date),"%Y%m%d"))/365.25,3)) %>% 
                        group_by(index_id) %>% arrange(desc(-child_birth_date)) %>% mutate(child_order=row_number()) %>% ungroup() %>%
                        mutate(child_death_before28days=ifelse(!is.na(child_death_date) & as.Date(as.character(child_death_date),"%Y%m%d")-as.Date(as.character(child_birth_date),"%Y%m%d")<28, 1, 0)) %>%
                        select(index_id, index_birth_date, index_birth_age, child_id, child_sex, child_order, child_birth_date, child_death_date, child_death_before28days)


# Add ART, preterm, and death before 28days (Neonatal mortality: https://www.stat.fi/til/ksyyt/2017/ksyyt_2017_2018-12-17_laa_001_en.html)
indexW_5682_everyone_child <- indexW_5682_everyone_child %>% mutate(child_id=as.character(child_id)) %>% 
                              left_join(mbr_c[,c("TNRO","ALKIONSIIRTO","KESTOVKPV")], by=c("child_id"="TNRO")) %>% 
                              rename(ART="ALKIONSIIRTO") %>% mutate(child_ART=ifelse(is.na(ART),NA,ART)) %>% 
                              mutate(child_preterm=ifelse(is.na(KESTOVKPV), NA, as.numeric(substr(as.character(KESTOVKPV),1,2)<37))) %>% 
                              select(index_id, index_birth_date, index_birth_age, child_id, child_sex, child_order, child_birth_date, child_death_date, child_death_before28days, child_ART, child_preterm)

dim(indexW_5682_everyone_child)  # 3,011,597       11
# save(indexW_5682_everyone_child, file=paste0(r_dir, "indexW_5682_everyone_child.Rdata"))

table(indexW_5682_everyone_child$child_death_before28days)
#       0       1 
# 3004345    7249 

table(indexW_5682_everyone_child$child_ART)
#       0       1 
# 2576324   31252 

table(indexW_5682_everyone_child$child_preterm)
#       0       1 
# 2453899  153677 






###############################################
#    Longitudinal data for spouse registry    #
###############################################

# remove spouse registry after the end of 2018
indexW_5682_everyone_marriage <- marriage %>% filter(TUTKHENK_TNRO %in% indexW_5682_everyone$index_id) %>% 
                           select (-PUOLISO_ULKOHENKILO, -JARJ_NRO) %>% 
                           rename(index_id="TUTKHENK_TNRO", spouse_id="PUOLISON_TNRO", 
                                  start_date="ALKUPAIVA", end_date="PAATTYMISPAIVA", 
                                  marital_status="TUTKHENK_NYKYINEN_SIVIILISAATY", end_reason="PAATT_TAPA") %>% 
                           filter(start_date<=20181231)  # 6,602 records after 2018 were removed
nrow(indexW_5682_everyone_marriage)   # 1,408,264


indexW_5682_everyone_marriage <- indexW_5682_everyone_marriage %>% inner_join(indexW_5682_everyone[,c("index_id","index_birth_date","endfollowup4550_date")], by="index_id") %>% 
	      mutate(index_marry_age=as.numeric(as.Date(as.character(start_date),"%Y%m%d") - as.Date(as.character(index_birth_date),"%Y%m%d"))/365.25) %>% 
	      mutate(index_marry_age=round(index_marry_age,3)) %>% 
	      filter(start_date<endfollowup4550_date & index_marry_age>0) %>% 
	      mutate(index_marry_age=ifelse(index_marry_age<16, 16, index_marry_age)) %>% 
	      select(index_id, marital_status, spouse_id, start_date, index_marry_age, end_date, end_reason)

nrow(indexW_5682_everyone)  # 1,723,014
nrow(indexW_5682_everyone_marriage)   # 1,347,887
unique(indexW_5682_everyone_marriage$index_id) %>% length()  # 1,190,827
save(indexW_5682_everyone_marriage, file=paste0(r_dir, "indexW_5682_everyone_marriage.Rdata"))





################################################
#       Longitudinal (first) endpoints         #
################################################

# remove records with age onset before birth_date and after end of follow-up!!!!
indexW_5682_everyone_endpoint <- ry_first %>% mutate(ID=as.numeric(as.character(ID))) %>% inner_join(indexW_5682_everyone, by=c("ID"="index_id")) %>% filter(ENDPOINT %in% res_ep_info$Endpoint) %>%
                                        mutate(EVENT_F_DATE=as.numeric(as.character(EVENT_F_DATE))) %>% filter(EVENT_F_DATE>=19560101 & EVENT_F_DATE<=endfollowup4550_date) %>% 
                                        select(ID, ENDPOINT, EVENT_F_DATE, EVENT_N)

nrow(indexW_5682_everyone_endpoint)   # 7,946,440
save(indexW_5682_everyone_endpoint, file=paste0(r_dir, "indexW_5682_everyone_endpoint.Rdata"))





###########################
##  Comment on each file  #  
###########################

wfile <- c("indexW_5682_everyone.Rdata", "indexW_5682_everyone_marriage.Rdata", "indexW_5682_everyone_child.Rdata", "indexW_5682_everyone_endpoint.Rdata")
for (k in wfile){
	d <- data.frame(get(load(paste0(r_dir, k))))
	info <- matrix(NA, nrow=ncol(d), ncol=14)
	colnames(info) <- c("FileNumber","File","FileDescription","NoOfRows","NoOfColumns","ColumnNumber","ColumnName","ColumnNameOriginal","ColumnDescription","NumberOfLevels","Min","Max","DataType","FileType")	
	print(paste0("Start for file: ", k))
	
	for (i in 1:ncol(d)){
		print(paste("Column", i ,"of", ncol(d), "columns",sep=" "))
		info[i, c("FileNumber","File")] <- c(which(wfile==k), k)		
		info[i, c("NoOfRows","NoOfColumns")] <- c(nrow(d),ncol(d))		
		info[i, c("ColumnNumber", "ColumnName")] <- c(i,colnames(d)[i])		
		info[i, "NumberOfLevels"] <- length(unique(d[,i]))
		info[i, c("Min","Max")] <- c(min(as.numeric(as.character(d[d[,i]!="",i])),na.rm=T), max(as.numeric(as.character(d[d[,i]!="",i])), na.rm=T))
		
		if (is.numeric(d[,i])==T){
			info[i, c("Min","Max")] <- c(min(d[d[,i]!="",i]), max(d[d[,i]!="",i]))
		}
			info[i, "DataType"] <- ifelse(as.numeric(info[i, "NumberOfLevels"])<20, paste(unique(d[, i]),collapse = ","), paste(head(unique(d[, i]),5),collapse = ","))		
	}
	
	rm(d)  # save memory
	write.table(info, paste0("Data_comments_Newly_Generated_Data_LRS.csv"), append=T, quote=F, sep=" ", row.names=F, col.names=F)		
	print(paste0("Done for file: ", k))
}





###########################################################
#   index person: 1956-1982_fullsib & Age45-50_fullsib    #
###########################################################


# birth year 1956-1982; with both father_id and mother_id available; with same-sex full-siblings
indexW_5682_fullsib <- indexW_5682_everyone %>% group_by(parent_id) %>% count() %>% filter(n>1) %>% inner_join(indexW_5682_everyone, by="parent_id")


# birth year: men in 1956-1968, women in 1956-1973; with both father_id and mother_id available; with same-sex full-siblings
indexW_4550_everyone <- indexW_5682_everyone %>% filter((index_sex==1 & index_birth_date<=19681231)|(index_sex==2 & index_birth_date<=19731231)) 


# birth year: men in 1956-1968, women in 1956-1973; with both father_id and mother_id available; with same-sex full-siblings
indexW_4550_fullsib <- indexW_4550_everyone %>% group_by(parent_id) %>% count() %>% filter(n>1) %>% inner_join(indexW_4550_everyone, by="parent_id")


table(indexW$index_sex)  # 928,875 males and 873,157 females
table(indexW_5682_everyone$index_sex)  # 888,258 males and 834,756 females     
table(indexW_5682_fullsib$index_sex)   # 444,031 males and 405,589 females
table(indexW_4550_everyone$index_sex)  # 463,410 males and 572,518 females     
table(indexW_4550_fullsib$index_sex)   # 223,196 males and 271,561 females
  
indexW_5682_everyone %>% select(index_sex, parent_id) %>% unique() %>% group_by(index_sex) %>% count() %>% rename(N_family="n")  # 639,680 male full-sib family, and 607,679 male full-sib family
indexW_5682_fullsib %>% select(index_sex, parent_id) %>% unique() %>% group_by(index_sex) %>% count() %>% rename(N_family="n")   # 195,453 male full-sib family, and 178,512 male full-sib family
indexW_4550_everyone %>% select(index_sex, parent_id) %>% unique() %>% group_by(index_sex) %>% count() %>% rename(N_family="n")  # 338,063  male full-sib family, and 419,935 male full-sib family
indexW_4550_fullsib %>% select(index_sex, parent_id) %>% unique() %>% group_by(index_sex) %>% count() %>% rename(N_family="n")   #  97,849 male full-sib family, and 118,978 male full-sib family


save(indexW_5682_everyone, file=paste0(r_dir, "indexW_5682_everyone.Rdata"))   
save(indexW_5682_fullsib, file=paste0(r_dir, "indexW_5682_fullsib.Rdata"))   ## also named as "indexW_fullsib.Rdata"
save(indexW_4550_everyone, file=paste0(r_dir, "indexW_4550_everyone.Rdata")) 
save(indexW_4550_fullsib, file=paste0(r_dir, "indexW_4550_fullsib.Rdata"))





