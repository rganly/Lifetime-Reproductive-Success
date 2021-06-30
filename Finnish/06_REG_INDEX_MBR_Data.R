## Medical birth registry
# The Register includes data on live births and 
# on stillbirths of foetuses with a birth weight of at least 500 g or with a gestational age of at least 22 weeks, as well as data on the mothers.
# distribution: N, by n of children


setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"

library(ggplot2, lib.loc="/homes/aliu/anaconda3/lib/R/library")
library(gridExtra)
library(dplyr, lib.loc="/homes/aliu/anaconda3/lib/R/library") 

'%!in%' <- function(x,y)!('%in%'(x,y))


# Useful variable -----------------------------
# KESTOVKPV (Gestational age WW + D, best estimate)
# AIEMMATSYNNYTYKSET, Previous births
# SYNNYTYSTAPATUNNUS, Mode of delivery
# KUOLLEISUUS, Perinatal death
# SYNTYMATILATUNNUS, The infant born live/stillborn




################################################
#               Read in data                   #
################################################

# Mother's medical birth registry
mbr_m <- get(load(paste0(r_dir, "mbr_mothers_1987_2018.Rdata")))
mbr_m <- mbr_m %>% mutate(TNRO=as.character(TNRO))
dim(mbr_m)           # 1867462     137


# Mother's malformation registry
mal_m <- data.frame(get(load(paste0(r_dir, "anomalies_mothers_1987_2016.Rdata"))))
dim(mal_m)  # 76,489     6
mal_m_dg <- data.frame(get(load(paste0(r_dir, "anomalies_mothers_1987_2016_dg.Rdata"))))
dim(mal_m_dg)  # 166,960      9
mal_m_dg %>% select(TNRO, RASKAUSNRO, ERNO_ID) %>% unique() %>% nrow()  # 76,489


# Co-variates and endpoints, remove thoese with right censoring (45/50) and only keep females
phe <- data.frame(get(load(paste0(r_dir, "Phe_IndexW_AllEndpointsWith50Cases_NoDiagnoseAfter4550.Rdata"))))
phe <- phe %>% filter(SUKUPUOLI==2 & !is.na(n_child_Age4550)) %>% select(-SUKUPUOLI)
dim(phe)  #  637,894   2038




################################################
#                  Preterm                     #
################################################

## Preterm is defined as babies born alive before 37 weeks of pregnancy are completed. (https://www.who.int/news-room/fact-sheets/detail/preterm-birth)
# There are sub-categories of preterm birth, based on gestational age: extremely preterm (less than 28 weeks) very preterm (28 to 32 weeks) moderate to late preterm (32 to 37 weeks).
# Across 184 countries, the rate of preterm birth ranges from 5% to 18% of babies born.

## First Parity (only keep index person) -------------------------------
Preterm_p1 <- mbr_m %>% mutate(Gestation_week=substr(as.character(KESTOVKPV),1,2)) %>% 
              filter(AIEMMATSYNNYTYKSET==0 & KUOLLEISUUS!=1 & Gestation_week!="") %>%
              mutate(pretermGrp2=ifelse(Gestation_week<37,1,0), pretermGrp4=ifelse(Gestation_week<28,"<28",ifelse(Gestation_week<32,"28-32",ifelse(Gestation_week<37,"32-37",">=37")))) %>%
              select(TNRO, pretermGrp2, pretermGrp4, AITI_IKA) %>% 
              group_by(as.character(TNRO)) %>% filter(row_number()==1) %>%             
              inner_join(phe, by=c("TNRO"="KANTAHENKILON_TNRO")) %>% 
              filter(AITI_IKA<=45 & childless_Age4550==0) 
dim(Preterm_p1)  # 347,475   2042
length(unique(Preterm_p1$TNRO))  # 347475


# check the completement of data
mbr_m %>% mutate(Gestation_week=substr(as.character(KESTOVKPV),1,2)) %>% 
              filter(AIEMMATSYNNYTYKSET==0 & KUOLLEISUUS!=1 & Gestation_week!="") %>%
              mutate(pretermGrp2=ifelse(Gestation_week<37,1,0), pretermGrp4=ifelse(Gestation_week<28,"<28",ifelse(Gestation_week<32,"28-32",ifelse(Gestation_week<37,"32-37",">=37")))) %>%
              select(TNRO, pretermGrp2, pretermGrp4, AITI_IKA, AITI_TULOPVM) %>%
              group_by(TNRO) %>% filter(row_number()==1) %>%             
              inner_join(phe, by=c("TNRO"="KANTAHENKILON_TNRO")) %>% 
              filter(AITI_IKA<=45 & childless_Age4550==0) %>% 
              mutate(year=substr(AITI_TULOPVM,1,4)) %>% group_by(year) %>% summarize(rate=sum(pretermGrp2==0)/length(pretermGrp2)) %>% data.frame()

sum(Preterm_p1$pretermGrp2==1)/nrow(Preterm_p1)  # 0.06216275
Preterm_p1 %>% group_by(pretermGrp2, pretermGrp4) %>% count()
Preterm_p1 %>% group_by(pretermGrp2, childless_Age4550) %>% count()
save(Preterm_p1, file=paste0(r_dir, "PretermParity1_IndexW_AllEndpointsWith50Cases.Rdata")) 



# All Parity (only keep index person) -------------------------------
Preterm_pall <- mbr_m %>% filter(as.character(TNRO) %in% Preterm_p1$TNRO) %>% 
              mutate(Gestation_week=substr(as.character(KESTOVKPV),1,2)) %>% 
              filter(KUOLLEISUUS!=1 & Gestation_week!="") %>%
              mutate(pretermGrp2=ifelse(Gestation_week<37,1,0), pretermGrp4=ifelse(Gestation_week<28,"<28",ifelse(Gestation_week<32,"28-32",ifelse(Gestation_week<37,"32-37",">=37")))) %>%
              select(TNRO, pretermGrp2, pretermGrp4, AITI_IKA) %>% filter(AITI_IKA<=45) 
dim(Preterm_pall)  # 776,553      4              
save(Preterm_pall, file=paste0(r_dir, "PretermByParity_IndexW_AllEndpointsWith50Cases.Rdata")) 

              
Preterm_N <- Preterm_pall %>% select(TNRO, pretermGrp2) %>% group_by(TNRO) %>% summarize(n_preterm=sum(pretermGrp2==1)) %>% 
              inner_join(phe, by=c("TNRO"="KANTAHENKILON_TNRO")) 

dim(Preterm_N)  #  347,475   2039
save(Preterm_N, file=paste0(r_dir, "Preterm_N_IndexW_AllEndpointsWith50Cases.Rdata")) 

PretermN_NEB_freq <- Preterm_N %>% group_by(n_preterm,n_child_Age4550) %>% count() %>% data.frame()
write.table(PretermN_NEB_freq, file="PretermN_NEB_freq.tsv", quote=F, row.names=F, sep="\t")

Preterm_rate_by_parity <- mbr_m %>% filter(as.character(TNRO) %in% Preterm_p1$TNRO) %>% 
              mutate(Gestation_week=substr(as.character(KESTOVKPV),1,2)) %>% 
              filter(KUOLLEISUUS!=1 & Gestation_week!="") %>% mutate(parity=AIEMMATSYNNYTYKSET+1) %>% group_by(parity) %>%
              summarize(N_preterm_births=sum(Gestation_week<37), N_births=sum(!is.na(Gestation_week)), preterm_rate=round(N_preterm_births/N_births,3)) %>% data.frame()
write.table(Preterm_rate_by_parity, file="Preterm_rate_by_parity.tsv", quote=F, row.names=F, sep="\t")




################################################
#               Malformation                   #
################################################

Preterm_N <- data.frame(get(load(paste0(r_dir, "Preterm_N_IndexW_AllEndpointsWith50Cases.Rdata"))))

mal_m_dg %>% select(TNRO, RASKAUSNRO, ERNO_ID) %>% unique() %>% nrow()  # 76,489
mal_m_dg %>% select(TNRO) %>% unique() %>% nrow()  # 71,845

N_malform <- mal_m %>% select(TNRO,YEAR_OF_BIRTH) %>% mutate(TNRO=as.character(TNRO)) %>% 
                       inner_join(Preterm_N[,c("TNRO","SUKULAISEN_SYNTYMAPV")], by="TNRO") %>%  
                       mutate(b_year=as.numeric(substr(SUKULAISEN_SYNTYMAPV,1,4))) %>% 
                       filter(as.numeric(as.character(b_year))-YEAR_OF_BIRTH<=45) %>% 
                       group_by(TNRO) %>% count() %>% rename(n_malform="n") %>%
                       right_join(Preterm_N, by=c("TNRO"="TNRO")) %>%
                       mutate(n_malform=ifelse(is.na(n_malform),0,n_malform)) %>% 
                       mutate(has_malform=ifelse(n_malform==0,0,1))
dim(N_malform)  # 347,475   2041                    
nrow(N_malform[N_malform$has_malform==1,])/nrow(N_malform)  # 0.08172099 with at least 1 malformation children
save(N_malform, file=paste0(r_dir, "MalformALL_N_IndexW_AllEndpointsWith50Cases.Rdata")) 


N_malform <- mal_m %>% filter(MANNER_OF_BIRTH==1) %>% 
                       select(TNRO,YEAR_OF_BIRTH) %>% mutate(TNRO=as.character(TNRO)) %>% 
                       inner_join(Preterm_N[,c("TNRO","SUKULAISEN_SYNTYMAPV")], by="TNRO") %>%  
                       mutate(b_year=as.numeric(substr(SUKULAISEN_SYNTYMAPV,1,4))) %>% 
                       filter(as.numeric(as.character(b_year))-YEAR_OF_BIRTH<=45) %>% 
                       group_by(TNRO) %>% count() %>% rename(n_malform="n") %>%
                       right_join(Preterm_N, by=c("TNRO"="TNRO")) %>%
                       mutate(n_malform=ifelse(is.na(n_malform),0,n_malform)) %>% 
                       mutate(has_malform=ifelse(n_malform==0,0,1))
dim(N_malform)  # 347,475   2041                    
nrow(N_malform[N_malform$has_malform==1,])/nrow(N_malform)  # 0.07414059 with at least 1 malformation children
save(N_malform, file=paste0(r_dir, "Malform_N_IndexW_AllEndpointsWith50Cases.Rdata")) 





n_malform_freq <- mal_m %>% select(TNRO,YEAR_OF_BIRTH, MANNER_OF_BIRTH) %>% 
                       filter(MANNER_OF_BIRTH %in% 1:12) %>%  
                       mutate(TNRO=as.character(TNRO)) %>% 
                       inner_join(Preterm_N[,c("TNRO","SUKULAISEN_SYNTYMAPV")], by="TNRO") %>%  
                       mutate(b_year=as.numeric(substr(SUKULAISEN_SYNTYMAPV,1,4))) %>% 
                       filter(as.numeric(as.character(b_year))-YEAR_OF_BIRTH<=45) %>% 
                       group_by(TNRO) %>% summarize(n_malform=sum(!is.na(TNRO)), n_malform_termination=sum(MANNER_OF_BIRTH %in% 4:12), n_malform_stillbirth=sum(MANNER_OF_BIRTH %in% 2), n_malform_livebirth=sum(MANNER_OF_BIRTH %in% 1)) %>%
                       right_join(Preterm_N, by=c("TNRO"="TNRO")) %>%
                       mutate(n_malform=ifelse(is.na(n_malform),0,n_malform)) %>%
                       mutate(n_malform_termination=ifelse(is.na(n_malform_termination),0,n_malform_termination)) %>%
                       mutate(n_malform_stillbirth=ifelse(is.na(n_malform_stillbirth),0,n_malform_stillbirth)) %>%
                       mutate(n_malform_livebirth=ifelse(is.na(n_malform_livebirth),0,n_malform_livebirth)) %>% 
                       group_by(n_malform, n_malform_termination, n_malform_stillbirth, n_malform_livebirth) %>% count() %>% data.frame()
                       
write.table(n_malform_freq, file="MalformN_freq.tsv", quote=F, row.names=F, sep="\t")







#######################################
#          Mode of delivery           #
#######################################

## Mode of delivery of first child---------------
#1	Spontaneous vaginal delivery
#2	Vaginal breech delivery (since 1996)
#3	Forceps
#4	Vacuum extractor
#5	Planned CS (since 1991)
#6	Urgent CS (since 2004, some hospitals since Autumn 2005)
#7	Emergency CS (since 2004, some hospitals since Autumn 2005)
#8	Other CS
#9	Unknown


deliveryMode_p1 <- mbr_m %>% filter(AIEMMATSYNNYTYKSET==0 & SYNNYTYSTAPATUNNUS %in% 1:8 ) %>%
              mutate(deliveryAssistGrp2=ifelse(SYNNYTYSTAPATUNNUS==1,0,1), deliveryCSGrp2=ifelse(SYNNYTYSTAPATUNNUS %in% 5:8,1,0) ) %>%
              select(TNRO, deliveryAssistGrp2, deliveryCSGrp2, AITI_IKA) %>%
              inner_join(phe, by=c("TNRO"="KANTAHENKILON_TNRO")) %>% filter(AITI_IKA<=45 & childless_Age4550==0)

nrow(deliveryMode_p1) # 355,535

sum(deliveryMode_p1$deliveryAssistGrp2==1)/nrow(deliveryMode_p1)  # 0.3244744
deliveryMode_p1 %>% group_by(deliveryAssistGrp2, childless_Age4550) %>% count()
#   deliveryAssistGrp2 childless_Age4550      n
#                <dbl>             <dbl>  <int>
# 1                  0                 0 240173
# 2                  1                 0 115362


sum(deliveryMode_p1$deliveryCSGrp2==1)/nrow(deliveryMode_p1)      # 0.2102578
deliveryMode_p1 %>% group_by(deliveryCSGrp2, childless_Age4550) %>% count()
#   deliveryCSGrp2 childless_Age4550      n
#            <dbl>             <dbl>  <int>
# 1              0                 0 280781
# 2              1                 0  74754

save(deliveryMode_p1, file=paste0(r_dir, "DeliveryModeParity1_IndexW_AllEndpointsWith50Cases.Rdata")) 


## Misscarriage of first child--------------------
# A miscarriage, or spontaneous abortion, is an event that results in the loss of a fetus before 20 weeks of pregnancy. 
# It typically happens during the first trimester, or first three months, of the pregnancy. 
# Miscarriages can happen for a variety of medical reasons, many of which aren't within a person's control.
# There is one register named "Register of Induced Abortions" (https://thl.fi/en/web/thlfi-en/statistics/information-on-statistics/register-descriptions/register-of-induced-abortions)

# SYNTYMATILATUNNUS

deliveryMode_p1 <- mbr_m %>% filter(AIEMMATSYNNYTYKSET==0 ) %>% group_by(SYNTYMATILATUNNUS) %>% count()
  SYNTYMATILATUNNUS      n
  <chr>              <int>
1 1                 755686
2 2                   2264
3 3                    233
4 4                    358


mbr_m %>% filter(AIEMMATSYNNYTYKSET==0 ) %>% group_by(KUOLLEISUUS, SYNTYMATILATUNNUS) %>% count()  # dead child in SYNTYMATILATUNNUS has been covered by KUOLLEISUUS
mbr_m %>% filter(AIEMMATSYNNYTYKSET==0 ) %>% mutate(Gestation_week=substr(as.character(KESTOVKPV),1,2)) %>% group_by(KUOLLEISUUS, Gestation_week) %>% count() %>% data.frame()

mbr_m %>% mutate(Gestation_week=substr(as.character(KESTOVKPV),1,2)) %>% 
              filter(AIEMMATSYNNYTYKSET==0 & KUOLLEISUUS!=1 & Gestation_week!="") %>%
              mutate(pretermGrp2=ifelse(Gestation_week<37,1,0), pretermGrp4=ifelse(Gestation_week<28,"<28",ifelse(Gestation_week<32,"28-32",ifelse(Gestation_week<37,"32-37",">=37")))) %>%
              select(TNRO, pretermGrp2, pretermGrp4, AITI_IKA, KUOLLEISUUS) %>%
              inner_join(phe, by=c("TNRO"="KANTAHENKILON_TNRO")) %>% 
              filter(AITI_IKA<=45 & childless_Age4550==0) %>% 
              group_by(KUOLLEISUUS, Gestation_week) %>% count() %>% data.frame()





#######################################
#                 Plot                #
#######################################

# Finland
scp aliu@ssh.fimm.fi:/homes/aliu/DSGE_LRS/output/registry_edit/PretermN_NEB_freq.tsv  /Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Main_analysis/MBR/Finland/

setwd("/Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Main_analysis/MBR/Finland/")
PretermN_NEB_freq <- read.table("PretermN_NEB_freq.tsv", header=T)

p <- ggplot(PretermN_NEB_freq %>% group_by(n_preterm) %>% summarize(N=sum(n)), aes(x=n_preterm, y=N)) + 
      xlab("N of preterm births") + ylab("N") +
      geom_bar(stat="identity") + 
      theme(axis.text=element_text(size=12,face="bold",color="black"), axis.title=element_text(size=14,face="bold",color="black")) + theme_classic() +
      geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
ggsave("Preterm_Npreterm.png", width=7, height=4)

p <- ggplot(PretermN_NEB_freq %>% group_by(n_child_Age4550) %>% summarize(N=sum(n)), aes(x=n_child_Age4550, y=N)) + 
      xlab("N of children") + ylab("N") +
      geom_bar(stat="identity") + 
      theme(axis.text=element_text(size=12,face="bold",color="black"), axis.title=element_text(size=14,face="bold",color="black"))  +  theme_classic() + 
      geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
ggsave("Preterm_NEB.png", width=9, height=4)
    
p <- ggplot(PretermN_NEB_freq %>% mutate(n_preterm=as.factor(n_preterm)), aes(x=n_child_Age4550, y=n, fill=n_preterm)) + 
      geom_bar(position = "fill",stat="identity") +
      xlab("N of children") + ylab("Proportion") + scale_y_continuous(labels = scales::percent) +
      theme_classic()  
ggsave("Preterm_NEB_byNpreterm.png", width=9, height=4)





N_of_preterm = age_at_first_child + NEB + disease + birth_year + birth_year^2
N_of_malformation = age_at_first_child + NEB + disease + birth_year + birth_year^2

N_of_misscarriage (Mika)





# check the results------------------
cd  /homes/aliu/DSGE_LRS/output/registry_edit/ 
cat 06_REG_INDEX_GLM_pretermGrp2_female.tsv| grep -v O15| awk '$4<0.05/1500'|awk '$1>0'  # increase risk of preterm
cat 06_REG_INDEX_GLM_pretermGrp2_female.tsv| grep -v O15| awk '$4<0.05/1500'|awk '$1<0'  # decrease risk of preterm
cat 06_REG_INDEX_GLM_pretermGrp2_female.tsv| grep -v O15| awk '$4<0.05/1500'|awk '$1>0{print $NF}'| cut -d'_' -f1|sort|uniq -c|sort -nk1,1 -r
cat 06_REG_INDEX_GLM_pretermGrp2_female.tsv| grep -v O15| awk '$4<0.05/1500'|awk '$1>0{print $NF,$4}'| grep E4
cat 06_REG_INDEX_GLM_pretermGrp2_female.tsv| grep -v O15| awk '$4<0.05/1500'|awk '$1>0{print $NF,$4}'| grep N14
cat 06_REG_INDEX_GLM_pretermGrp2_female.tsv| grep -v O15| awk '$4<0.05/1500'|awk '$1>0{print $NF,$4}'| grep M13


cat 06_REG_INDEX_GLM_deliveryAssistGrp2_female.tsv| grep -v O15| awk '$4<0.05/1500'|awk '$1>0'
cat 06_REG_INDEX_GLM_deliveryAssistGrp2_female.tsv| grep -v O15| awk '$4<0.05/1500'|awk '$1>0{print $NF}' | cut -d'_' -f1|sort|uniq -c|sort -nk1,1 -r

cat 06_REG_INDEX_GLM_deliveryAssistGrp2_female.tsv| grep -v O15| awk '$4<0.05/1500'|awk '$1>0{print $NF,$4}'| grep J10




