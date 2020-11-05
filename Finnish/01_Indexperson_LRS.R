## This script is to calculate LRS (N of children and N of grandchildren), childless, and age of having the first/last child for each index person.
 
setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"

library(tidyverse)

'%!in%' <- function(x,y)!('%in%'(x,y))


############################################
#             Read in data                 #
############################################

# Index person (born 1956-1982)  
index <- get(load(paste0(r_dir, "index.Rdata")))                # 2,365,707 indexperson
table(index$SUKUPUOLI)                                          # 1,260,350 males and 1,105,357 females


# Children of index person  
child <- get(load(paste0(r_dir, "child.Rdata")))  
length(unique(child$KANTAHENKILON_TNRO))                        # 1,555,839 indexperson with children
length(unique(child$SUKULAISEN_TNRO))                           # 2,060,722 children


# Grandchildren of index person  
grandchild <- get(load(paste0(r_dir, "grandchild.Rdata")))  
length(unique(grandchild$KANTAHENKILON_TNRO))                   # 371,109 indexperson with grandchildren
length(unique(grandchild$SUKULAISEN_TNRO))                      # 482,777 grandchildren



############################################
#         QC for index person              #
############################################

# birth year 1956-1982, born in Finland, not imigrated/emigrated from Sweden, and alive until age 15
index %>% filter(SYNTYMAKOTIKUNTA==200 & AIDINKIELI=="fi") %>% nrow()   # 60,890  born outside of Finland but with Finnish as mother tongue 

indexW <- index %>% filter(SYNTYMAKOTIKUNTA!=200 & is.na(ULKOMAILLE_MUUTON_PV) & ULKOM_ASUINVALTIO=="" & ULKOM_ASUINVALTION_NIMI=="") %>%     # born in Finland and no emigration
                    filter(substr(SUKULAISEN_SYNTYMAPV,1,4)>=1956 & substr(SUKULAISEN_SYNTYMAPV,1,4)<=1982) %>%                               # birth year 1956-1982
                    filter(SUKULAISEN_KUOLINPV - SUKULAISEN_SYNTYMAPV>=150000|is.na(SUKULAISEN_KUOLINPV)|is.na(SUKULAISEN_SYNTYMAPV))         # avlive until 15

nrow(indexW)   # 1,802,891
table(indexW$SUKUPUOLI)  # 929,448 males and 873,443 females


# sex ratio for raw data and QCed data
sum(index$SUKUPUOLI==1)/sum(index$SUKUPUOLI==2)     # 1.14022
sum(indexW$SUKUPUOLI==1)/sum(indexW$SUKUPUOLI==2)   # 1.06412, sex ratio become normal after removing people born and moving out of Finland



############################################
#   LRS (N of children or grandchildren)   #
############################################

# n_child
index_lrs <- child %>% select(KANTAHENKILON_TNRO,SUKULAISEN_TNRO,SUKULAISEN_SYNTYMAPV) %>% 
                           inner_join(indexW[,c("KANTAHENKILON_TNRO","SUKULAISEN_SYNTYMAPV","SUKUPUOLI")], by="KANTAHENKILON_TNRO") %>% 
                           mutate(age_of_child=as.numeric(SUKULAISEN_SYNTYMAPV.x) - as.numeric(SUKULAISEN_SYNTYMAPV.y)) %>%
                           filter(age_of_child>=100000) %>% 
                           group_by(KANTAHENKILON_TNRO) %>% count() %>% rename(n_child=n) %>% data.frame()

nrow(indexW) - nrow(index_lrs)    # 444,691 indexperson are childless
mean(index_lrs[,"n_child"])       # 2.332206 for indexperson with children
range(index_lrs[,"n_child"])      # from 1 to 19


# n_child_Age4550 
index_lrs4550 <- child %>% select(KANTAHENKILON_TNRO,SUKULAISEN_TNRO,SUKULAISEN_SYNTYMAPV) %>% 
                           inner_join(indexW[,c("KANTAHENKILON_TNRO","SUKULAISEN_SYNTYMAPV","SUKUPUOLI")], by="KANTAHENKILON_TNRO") %>% 
                           mutate(age_of_child=as.numeric(SUKULAISEN_SYNTYMAPV.x) - as.numeric(SUKULAISEN_SYNTYMAPV.y)) %>%
                           filter(age_of_child>=100000) %>% 
                           filter((age_of_child<=500000 & SUKUPUOLI==1)|(age_of_child<=450000 & SUKUPUOLI==2)) %>% 
                           group_by(KANTAHENKILON_TNRO) %>% count() %>% rename(n_child_Age4550=n) %>% data.frame()
              
nrow(indexW) - nrow(index_lrs4550)    # 445,842 indexperson are childless
mean(index_lrs4550[,"n_child_Age4550"])       # 2.32883 for indexperson with children
range(index_lrs4550[,"n_child_Age4550"])      # from 1 to 19


# n_grandchild
index_glrs <- grandchild %>% filter(KANTAHENKILON_TNRO %in% indexW$KANTAHENKILON_TNRO) %>% group_by(KANTAHENKILON_TNRO) %>% count() %>% rename(n_gchild=n) %>% data.frame()  
nrow(indexW) - nrow(index_glrs)     # 1,454,059 indexperson without grandchildren
mean(index_glrs[,"n_gchild"])       # 2.908214 for indexperson with grandchildren 
range(index_glrs[,"n_gchild"])      # from 1 to 89   ## the maximum is much higher than Swedish


# combine
lrs_all <- index_lrs %>% full_join(index_lrs4550, by="KANTAHENKILON_TNRO") %>%
                         full_join(index_glrs, by="KANTAHENKILON_TNRO") %>%
                         full_join(indexW[,c("KANTAHENKILON_TNRO","SUKULAISEN_SYNTYMAPV","SUKUPUOLI")], by="KANTAHENKILON_TNRO") %>%
                         mutate(n_child=ifelse(is.na(n_child),0,n_child), n_gchild=ifelse(is.na(n_gchild),0,n_gchild)) %>%
                         mutate(n_child_Age4550=ifelse((20200127-SUKULAISEN_SYNTYMAPV<=500000 & SUKUPUOLI==1)|(20200127-SUKULAISEN_SYNTYMAPV<=450000 & SUKUPUOLI==2), NA, ifelse(!is.na(n_child_Age4550),n_child_Age4550, 0))) %>% 
                         mutate(childless=ifelse(n_child==0,1,0), childless_Age4550=ifelse(is.na(n_child_Age4550),NA,ifelse(n_child_Age4550==0,1,0))) %>%
                         select(KANTAHENKILON_TNRO, SUKULAISEN_SYNTYMAPV, SUKUPUOLI, n_child, n_child_Age4550, n_gchild, childless, childless_Age4550)

range(lrs_all[is.na(lrs_all$n_child_Age4550),"SUKULAISEN_SYNTYMAPV"])  # should after 1970
range(lrs_all[!is.na(lrs_all$n_child_Age4550),"SUKULAISEN_SYNTYMAPV"]) # should before 1975

summary(lrs_all[lrs_all[,"SUKUPUOLI"]==1, c("n_child","n_child_Age4550","n_gchild")])  #   male: mean(n_child/n_child_Age4550/n_grandchild)=1.636/1.7/0.4476
summary(lrs_all[lrs_all[,"SUKUPUOLI"]==2, c("n_child","n_child_Age4550","n_gchild")])  # female: mean(n_child/n_child_Age4550/n_grandchild)=1.886/1.92/0.6852



############################################
#     Age of having first/last child       #
############################################

indexW_LRS <- child %>% mutate(b_year=substr(SUKULAISEN_SYNTYMAPV,1,4)) %>% select(c("KANTAHENKILON_TNRO","SUKULAISEN_TNRO","b_year")) %>% 	
                            group_by(KANTAHENKILON_TNRO) %>% 
                            summarize(KANTAHENKILON_TNRO=KANTAHENKILON_TNRO[1], bf_year=b_year[which(b_year==min(b_year,na.rm=T))[1]], bl_year=b_year[which(b_year==max(b_year,na.rm=T))[1]]) %>% 
                            inner_join(lrs_all[,c("KANTAHENKILON_TNRO","SUKULAISEN_SYNTYMAPV","SUKUPUOLI")], by="KANTAHENKILON_TNRO") %>%
                            mutate(afc=as.numeric(as.character(bf_year))-as.numeric(substr(SUKULAISEN_SYNTYMAPV,1,4)), alc=as.numeric(as.character(bl_year))-as.numeric(substr(SUKULAISEN_SYNTYMAPV,1,4))) %>% 
                            filter(afc>10 & alc>10) %>% select(KANTAHENKILON_TNRO, afc, alc) %>% 
                            right_join(lrs_all, by="KANTAHENKILON_TNRO") %>% 
                            select(KANTAHENKILON_TNRO, SUKULAISEN_SYNTYMAPV, SUKUPUOLI, n_child,n_child_Age4550, n_gchild, childless, childless_Age4550, afc, alc)

save(indexW_LRS, file=paste0(r_dir, "indexW_LRS.Rdata"))  



############################################
#      Distribution of LRS                 #
############################################

# LRS-related phenotypes by birth year and sex
Summary_LRS <- indexW_LRS %>% mutate(b_year=substr(SUKULAISEN_SYNTYMAPV,1,4)) %>% group_by(b_year,SUKUPUOLI) %>% 
                              summarize(birth_year=b_year[1], sex=SUKUPUOLI[1], N=sum(!is.na(n_child)),
                                  n_child_mean=mean(n_child,na.rm=T), n_child_Age4550_mean=mean(n_child_Age4550,na.rm=T), n_gchild_mean=mean(n_gchild,na.rm=T),
                                  n_child_max=max(n_child), n_child_Age4550_max=max(n_child_Age4550,na.rm=T), n_gchild_max=max(n_gchild,na.rm=T),
                                  N_0child_Age4550=sum(n_child_Age4550==0), N_1child_Age4550=sum(n_child_Age4550==1), N_2child_Age4550=sum(n_child_Age4550==2), N_3child_Age4550=sum(n_child_Age4550==3), N_4child_Age4550=sum(n_child_Age4550==4)，
                                  afc_mean=mean(afc,na.rm=T), alc_mean=mean(alc,na.rm=T)) 

write.table(Summary_LRS, "Summary_LRS.txt", append=F, quote=F, sep=" ", row.names=F, col.names=T)


# count of AFC and ALC
Summary_afc <- indexW_LRS %>% mutate(b_year=substr(SUKULAISEN_SYNTYMAPV,1,4)) %>% group_by(afc) %>% 
                                   summarize(count_afc_male=sum(SUKUPUOLI==1), count_afc_female=sum(SUKUPUOLI==2)) %>% rename(Age=afc)

Summary_AgeChild <- indexW_LRS %>% mutate(b_year=substr(SUKULAISEN_SYNTYMAPV,1,4)) %>% group_by(alc) %>% 
                                   summarize(count_alc_male=sum(SUKUPUOLI==1), count_alc_female=sum(SUKUPUOLI==2)) %>% rename(Age=alc) %>% 
                                   full_join(Summary_afc, by="Age") %>% arrange(Age)

write.table(Summary_AgeChild, "Summary_AgeChild.txt", append=F, quote=F, sep=" ", row.names=F, col.names=T)


