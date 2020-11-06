## This script is to calculate LRS (N of children) and age of having the first/last child for each sibling.

setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"

library(tidyverse)

'%!in%' <- function(x,y)!('%in%'(x,y))


############################################
#             Read in data                 #
############################################

# QCed index person
indexW <- get(load(paste0(r_dir, "indexW_LRS.Rdata"))) 
nrow(indexW)


# sib
sib <- get(load(paste0(r_dir,"sib.Rdata")))
nrow(sib)  # 8,066,956
length(unique(sib$KANTAHENKILON_TNRO))    # 1,819,317 indexperson with sibling
length(unique(sib$SUKULAISEN_TNRO))       # 2,307,224 siblings 
length(unique(sib$VALIHENKILO1_TNRO))     # 1,600,825, VALIHENKILO1_TNRO is shared father/mother between indexperson-sibling 

sib_uniq <- sib %>% filter(KANTAHENKILON_TNRO %in% indexW$KANTAHENKILON_TNRO) %>% select(-c("KANTAHENKILON_TNRO","SUKUL_SUHDE","VALIHENKILO1_TNRO","VALIHENKILO2_TNRO")) %>% distinct(SUKULAISEN_TNRO, .keep_all=T)
nrow(sib_uniq)  # 2,144,679 
save(sib_uniq, file=paste0(r_dir,"sib_uniq.Rdata"))    


# sib's children
sibchild <- get(load(paste0(r_dir,"sibchild.Rdata")))
nrow(sibchild)   # 14,486,780
length(unique(sibchild$KANTAHENKILON_TNRO))   # 1,521,419 index person have sibling's with children
length(unique(sibchild$VALIHENKILO1_TNRO))    # 1,397,102 VALIHENKILO1_TNRO is shared father/mother between indexperson-sibling
length(unique(sibchild$VALIHENKILO2_TNRO))    # 1,597,982 VALIHENKILO2_TNRO is sibling
length(unique(sibchild$SUKULAISEN_TNRO))      # 2,392,471 sibling's children 

sibchild_uniq <- sibchild %>% filter(KANTAHENKILON_TNRO %in% indexW$KANTAHENKILON_TNRO) %>% select(-c("KANTAHENKILON_TNRO","SUKUL_SUHDE","VALIHENKILO1_TNRO")) %>% distinct(VALIHENKILO2_TNRO, SUKULAISEN_TNRO, .keep_all=T)
nrow(sibchild_uniq)  # 3,549,721
save(sibchild_uniq, file=paste0(r_dir,"sibchild_uniq.Rdata"))      



###########################
#   LRS (N of children)   #
###########################

# n_child for each sibling 
sib_lrs <- sibchild_uniq %>% select(VALIHENKILO2_TNRO,SUKULAISEN_SYNTYMAPV) %>% 
                  inner_join(sib_uniq[,c("SUKULAISEN_TNRO","SUKULAISEN_SYNTYMAPV")], by=c("VALIHENKILO2_TNRO"="SUKULAISEN_TNRO")) %>% 
                  mutate(age_of_child=as.numeric(SUKULAISEN_SYNTYMAPV.x) - as.numeric(SUKULAISEN_SYNTYMAPV.y)) %>% rename(KANTAHENKILON_TNRO=VALIHENKILO2_TNRO) %>%
                  filter(age_of_child>=100000) %>% 
                  group_by(KANTAHENKILON_TNRO) %>% count() %>% rename(n_child=n) %>% data.frame()

nrow(sib_lrs)  # 1,533,434                  
summary(sib_lrs$n_child)  # mean=2.315, max=19, for sibling with children

nrow(sib_uniq) - nrow(sib_lrs)  # 611,245 indexperson are childless
mean(sib_lrs[,"n_child"])       # 2.314882 for indexperson with children
range(sib_lrs[,"n_child"])      # from 1 to 19


# n_child_Age4550 
sib_lrs4550 <- sibchild_uniq %>% select(VALIHENKILO2_TNRO,SUKULAISEN_SYNTYMAPV) %>% 
                  inner_join(sib_uniq[,c("SUKULAISEN_TNRO","SUKUPUOLI","SUKULAISEN_SYNTYMAPV")], by=c("VALIHENKILO2_TNRO"="SUKULAISEN_TNRO")) %>% 
                  mutate(age_of_child=as.numeric(SUKULAISEN_SYNTYMAPV.x) - as.numeric(SUKULAISEN_SYNTYMAPV.y)) %>% rename(KANTAHENKILON_TNRO=VALIHENKILO2_TNRO) %>%
                  filter(age_of_child>=100000) %>% 
                  filter((age_of_child<=500000 & SUKUPUOLI==1)|(age_of_child<=450000 & SUKUPUOLI==2)) %>% 
                  group_by(KANTAHENKILON_TNRO) %>% count() %>% rename(n_child_Age4550=n) %>% data.frame()
     
nrow(sib_uniq) - nrow(sib_lrs4550)  # 612,553 indexperson are childless
mean(sib_lrs4550[,"n_child_Age4550"])  # 2.310996 for indexperson with children
range(sib_lrs4550[,"n_child_Age4550"]) # from 1 to 19


# combine
lrs_all <- sib_lrs %>% full_join(sib_lrs4550, by="KANTAHENKILON_TNRO") %>%
                           full_join(sib_uniq[,c("SUKULAISEN_TNRO","SUKUPUOLI","SUKULAISEN_SYNTYMAPV")], by=c("KANTAHENKILON_TNRO"="SUKULAISEN_TNRO")) %>% 
                           mutate(n_child=ifelse(is.na(n_child),0,n_child)) %>%
                           mutate(n_child_Age4550=ifelse((20200127-SUKULAISEN_SYNTYMAPV<=500000 & SUKUPUOLI==1)|(20200127-SUKULAISEN_SYNTYMAPV<=450000 & SUKUPUOLI==2), NA, ifelse(!is.na(n_child_Age4550),n_child_Age4550, 0))) %>% 
                           mutate(childless=ifelse(n_child==0,1,0), childless_Age4550=ifelse(is.na(n_child_Age4550),NA,ifelse(n_child_Age4550==0,1,0))) %>%
                           select(KANTAHENKILON_TNRO, SUKULAISEN_SYNTYMAPV, SUKUPUOLI, n_child, n_child_Age4550, childless, childless_Age4550)
                                      
range(lrs_all[is.na(lrs_all$n_child_Age4550),"SUKULAISEN_SYNTYMAPV"])  # should after 1970
range(lrs_all[!is.na(lrs_all$n_child_Age4550),"SUKULAISEN_SYNTYMAPV"]) # should before 1975

summary(lrs_all[lrs_all[,"SUKUPUOLI"]==1, c("n_child","n_child_Age4550","childless", "childless_Age4550")])  #   male: mean(n_child/n_child_Age4550/n_grandchild)=1.636/1.7/0.4476
summary(lrs_all[lrs_all[,"SUKUPUOLI"]==2, c("n_child","n_child_Age4550","childless", "childless_Age4550")])  # female: mean(n_child/n_child_Age4550/n_grandchild)=1.886/1.92/0.6852

# save(sibW_LRS, file=paste0(r_dir,"sibW_LRS.Rdata"))



####################################
#  Age of having first/last child  #
####################################

sibW_LRS <- sibchild_uniq %>% mutate(b_year=substr(SUKULAISEN_SYNTYMAPV,1,4)) %>% select(c("VALIHENKILO2_TNRO","b_year")) %>% 	
                              group_by(VALIHENKILO2_TNRO) %>% 
                              summarize(bf_year=b_year[which(b_year==min(b_year,na.rm=T))[1]], bl_year=b_year[which(b_year==max(b_year,na.rm=T))[1]]) %>% 
                              inner_join(lrs_all[,c("KANTAHENKILON_TNRO","SUKULAISEN_SYNTYMAPV")], by=c("VALIHENKILO2_TNRO"="KANTAHENKILON_TNRO")) %>%                            
                              mutate(afc=as.numeric(as.character(bf_year))-as.numeric(substr(SUKULAISEN_SYNTYMAPV,1,4)), alc=as.numeric(as.character(bl_year))-as.numeric(substr(SUKULAISEN_SYNTYMAPV,1,4))) %>% 
                              filter(afc>10 & alc>10) %>% select(VALIHENKILO2_TNRO, afc, alc) %>% 
                              right_join(lrs_all, by=c("VALIHENKILO2_TNRO"="KANTAHENKILON_TNRO")) %>% rename(KANTAHENKILON_TNRO=VALIHENKILO2_TNRO) %>% 
                              select(KANTAHENKILON_TNRO, SUKULAISEN_SYNTYMAPV, SUKUPUOLI, n_child, n_child_Age4550, childless, childless_Age4550, afc, alc)



############################################
#      Distribution of LRS                 #
############################################

# LRS-related phenotypes by birth year and sex
Summary_LRS_sibW <- sibW_LRS %>% mutate(b_year=substr(SUKULAISEN_SYNTYMAPV,1,4)) %>% group_by(b_year,SUKUPUOLI) %>% 
                                 summarize(birth_year=b_year[1], sex=SUKUPUOLI[1], N=sum(!is.na(n_child)),
                                           n_child_mean=mean(n_child,na.rm=T), n_child_Age4550_mean=mean(n_child_Age4550,na.rm=T),
                                           n_child_max=max(n_child), n_child_Age4550_max=max(n_child_Age4550,na.rm=T),
                                           N_0child_Age4550=sum(n_child_Age4550==0), N_1child_Age4550=sum(n_child_Age4550==1),
                                           N_2child_Age4550=sum(n_child_Age4550==2), N_3child_Age4550=sum(n_child_Age4550==3), N_4child_Age4550=sum(n_child_Age4550==4),
                                           afc_mean=mean(afc,na.rm=T), alc_mean=mean(alc,na.rm=T)) 

write.table(Summary_LRS_sibW, "Summary_LRS_sibW.txt", append=F, quote=F, sep=" ", row.names=F, col.names=T)


# count of AFC and ALC
Summary_afc_sibW <- sibW_LRS %>% mutate(b_year=substr(SUKULAISEN_SYNTYMAPV,1,4)) %>% group_by(afc) %>% 
                                   summarize(count_afc_male=sum(SUKUPUOLI==1), count_afc_female=sum(SUKUPUOLI==2)) %>% rename(Age=afc)

Summary_AgeChild_sibW <- sibW_LRS %>% mutate(b_year=substr(SUKULAISEN_SYNTYMAPV,1,4)) %>% group_by(alc) %>% 
                                   summarize(count_alc_male=sum(SUKUPUOLI==1), count_alc_female=sum(SUKUPUOLI==2)) %>% rename(Age=alc) %>% 
                                   full_join(Summary_afc_sibW, by="Age") %>% arrange(Age)

write.table(Summary_AgeChild_sibW, "Summary_AgeChild_sibW.txt", append=F, quote=F, sep=" ", row.names=F, col.names=T)



