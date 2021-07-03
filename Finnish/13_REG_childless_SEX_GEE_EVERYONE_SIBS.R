## Aim: Regression analysis for disease status before age 45/50 using a GEE model in the full population

mod <- "MODELVAR"
NEB <- "TRAITVAR"  
sex_n <- SEXNVAR
sexs <- c("male","female")


setwd("/homes/aliu/DSGE_LRS/output/registry_edit/REGRESSION/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"


require(gridExtra)
library(tidyverse)
library(dplyr)
require(ggplot2)
library(survival, lib.loc="/homes/aliu/anaconda3/lib/R/library")  
require(sandwich)
require(msm, lib.loc="/homes/aliu/anaconda3/lib/R/library")
library(boot)
library(simpleboot, lib="/homes/aliu/anaconda3/lib/R/library")
library(lme4)
library(gee)
library(plotly, lib.loc="/homes/aliu/anaconda3/lib/R/library")
library(gnm, lib.loc="/homes/aliu/anaconda3/lib/R/library")  
   
'%!in%' <- function(x,y)!('%in%'(x,y))
   
   

#   ###########################################
#   #            Prepare input                #
#   ###########################################
#
#    ## Read in data---------------------------
#    LRS <- data.frame(get(load(paste0(r_dir, "indexW_LRS.Rdata"))))
#    nrow(LRS)   # 1,802,891
#     
#    edu <- get(load(paste0(r_dir, "index_edu.Rdata"))) %>% filter(id %in% LRS$KANTAHENKILON_TNRO)
#    nrow(edu)   # 1,802,806
#      
#    phe <- LRS %>% mutate(KANTAHENKILON_TNRO=as.character(KANTAHENKILON_TNRO)) %>% left_join(edu, by=c("KANTAHENKILON_TNRO"="id"))
#    nrow(phe)   # 1,802,891
#    
#    ry_first_index <- data.frame(get(load(paste0(r_dir, "ry_first_indexW_COMPLETE.Rdata")))) %>% filter(ID %in% LRS$KANTAHENKILON_TNRO)
#    nrow(ry_first_index)   # 16,495,332
#    
#    
#    ## Prepare 0/1 phenotype---------------------------
#    ry_first_index_Age4550 <- ry_first_index %>% inner_join(phe[,c("KANTAHENKILON_TNRO","SUKULAISEN_SYNTYMAPV","SUKUPUOLI")], by=c("ID"="KANTAHENKILON_TNRO")) %>%
#                                                 mutate(EVENT_F_DATE=as.numeric(as.character(EVENT_F_DATE)), EVENT_F_AGE=EVENT_F_DATE-SUKULAISEN_SYNTYMAPV) %>% 
#                                                 filter((SUKUPUOLI==1 & EVENT_F_AGE<=500000)|(SUKUPUOLI==2 & EVENT_F_AGE<=450000))
#    nrow(ry_first_index_Age4550)  # 12,692,519
#                                                 
#    ep_frq <- data.frame(table(ry_first_index_Age4550$ENDPOINT))
#    ep_frq <- ep_frq[order(-ep_frq$Freq),]
#    save(ep_frq, file=paste0(r_dir, "Phe_IndexW_ep_frq_NoDiagnoseAfter4550.Rdata"))
#    
#    endpoints <- as.character(ep_frq[ep_frq$Freq>=50,1])
#    for (ep_n in 1:length(endpoints)){
#    	id_i <- ry_first_index_Age4550 %>% filter(ENDPOINT==endpoints[ep_n]) %>% select(ID)
#    	phe[ ,endpoints[ep_n]] <- as.integer(phe$KANTAHENKILON_TNRO %in% id_i$ID)
#    	print(paste0(ep_n,": ",endpoints[ep_n]))	
#    }
#    
#    save(phe, file=paste0(r_dir, "Phe_IndexW_AllEndpointsWith50Cases_NoDiagnoseAfter4550.Rdata"))
#    # rm(ry_first_index)
#



###########################################
#         Format input variables          #
###########################################

phe <- data.frame(get(load(paste0(r_dir, "Phe_IndexW_AllEndpointsWith50Cases_NoDiagnoseAfter4550.Rdata"))))
ep_frq <- data.frame(get(load(paste0(r_dir, "Phe_IndexW_ep_frq.Rdata"))))
endpoints <- as.character(ep_frq[ep_frq$Freq>=50,1])

if (file.exists(paste0("06_REG_INDEX_",toupper(mod),"_",NEB,"_",sexs[sex_n],".tsv"))){
	fi <- read.table(paste0("06_REG_INDEX_",toupper(mod),"_",NEB,"_",sexs[sex_n],".tsv"), header=T)
	endpoints <- endpoints[endpoints %!in% fi$Endpoint]
}


phe <- within(phe, {
	KANTAHENKILON_TNRO <- factor(KANTAHENKILON_TNRO)
	b_year <- factor(substr(SUKULAISEN_SYNTYMAPV,1,4)) 
	n_child <- as.numeric(n_child)
	n_child_Age4550 <- as.numeric(n_child_Age4550)
	n_gchild <- as.numeric(n_gchild)
	n_pchild_Age4550 <- ifelse(n_child_Age4550>0,n_child_Age4550,NA)  # only for people with child  
	
	childless <- as.numeric(childless)
	childless_Age4550 <- as.numeric(childless_Age4550)
	has_child_Age4550 <- as.numeric(ifelse(childless_Age4550==0,1,ifelse(childless_Age4550==1,0,NA)))
	
	afc <- as.numeric(afc)
	afc_Age4550 <- as.numeric(has_child_Age4550==1 & !is.na(has_child_Age4550),afc,NA)
	
	survival_Age4550 <- as.numeric(survival_Age4550)
	has_spouse_Age4550 <- as.numeric(ifelse(survival_Age4550==0 & !is.na(survival_Age4550), NA, has_spouse_Age4550))
	
	EduYears <- as.numeric(as.character(EduYears))
	EduYears.Far <- as.numeric(as.character(EduYears.Far))	
	EduYears.Mor <- as.numeric(as.character(EduYears.Mor))		
	EduYears.Parent <- as.numeric(as.character(EduYears.Parent))	
	EduYears2 <- EduYears^2
	EduYears2.Far <- EduYears.Far^2	
	EduYears2.Mor <- EduYears.Mor^2		
	EduYears2.Parent <- EduYears.Parent^2
	
	ISCED97 <- factor(ISCED97) 
	ISCED97.Parent <- factor(ISCED97.Parent) 
	ISCED97.Far <- factor(ISCED97.Far) 
	ISCED97.Mor <- factor(ISCED97.Mor) 
	
	age <- as.numeric(as.character(b_year))-1956
	age2 <- age^2
	
	LopNrMor_Far_Kon <- paste0(mother_id, "_", father_id, "_",SUKUPUOLI)
		
})



###########################################
#         Regression analysis             #
###########################################

df_NA <- matrix(NA, ncol=16, nrow=1)
colnames(df_NA) <- c("Estimate","SE","Z","P_val","OR","OR_025","OR_975","N_Cases","N_Controls","Prevalence","LRS_Cases","LRS_Controls","model","sex","LRS","Endpoint")
df_gnm_NA <- matrix(NA, ncol=15, nrow=1)
colnames(df_gnm_NA) <- c("Estimate","SE","Z","P_val","OR","OR_025","OR_975","N_Family","N_Case","N_Control","N_Family_Case","model","sex","LRS","Endpoint")


i <- 1
p <- phe[!is.na(phe[,NEB[i]]) & phe$SUKUPUOLI==sex_n, ]
nrow(p)   # 530,215

if (mod=="gee"){   # For the full cohort (GEE)
	p_par <- p[!is.na(p[,"mother_id"]) & !is.na(p[,"father_id"]),]
	p_par <- p_par[order(p_par$LopNrMor_Far_Kon, p_par$b_year),]
	nrow(p_par)  # 496,677
	p_par$LopNrMor_Far_Kon <- as.factor(p_par$LopNrMor_Far_Kon)
	for (ep_n in 1:length(endpoints)){
		if (sum(p_par[,endpoints[ep_n]])>=50 & ((sum(p_par[,NEB]==0 & p_par[,endpoints[ep_n]]==1)>=10 & sum(p_par[,NEB]==0 & p_par[,endpoints[ep_n]]==0)>=10)|(NEB %in% c("n_child_Age4550","n_pchild_Age4550","afc_Age4550")))){	
			if (NEB %in% c("n_child_Age4550","n_pchild_Age4550","afc_Age4550")){
				m_gee <- gee(p_par[,NEB[i]] ~ p_par[,endpoints[ep_n]] + age + age2 + EduYears.Parent + EduYears2.Parent, data=p_par, family="poisson", id=LopNrMor_Far_Kon, corstr="exchangeable") 
			} else if (NEB %in% c("has_child_Age4550","has_spouse_Age4550","survival_Age4550")){
				m_gee <- gee(p_par[,NEB[i]] ~ p_par[,endpoints[ep_n]] + age + age2 + EduYears.Parent + EduYears2.Parent, data=p_par, family="binomial", id=LopNrMor_Far_Kon, corstr="exchangeable") 
			} else {
				print("Check NEB name!")
			}
			df <- as.data.frame(t(as.data.frame(summary(m_gee)$coeff["p_par[, endpoints[ep_n]]",])))
			rownames(df) <- "Endpoint"
			df$P_val <- 2 * pnorm(abs(df[,"Robust z"]), lower.tail = FALSE)
			df <- df[,c("Estimate","Robust S.E.","Robust z", "P_val")]
			colnames(df) <- c("Estimate","SE","Z", "P_val")
			df$OR <- exp(df[1,"Estimate"])
			ci <- exp(df[1,"Estimate"] + c(-1, 1) * df[1,"SE"] * qnorm(0.975))
			df$OR_025 <- ci[1]
			df$OR_975 <- ci[2]
			df$N_Cases <- sum(p_par[,endpoints[ep_n]]==1 & !is.na(p_par[,"age"]) & !is.na(p_par[,NEB[i]]) & !is.na(p_par[,"EduYears.Parent"]))
			df$N_Controls <- sum(p_par[,endpoints[ep_n]]==0 & !is.na(p_par[,"age"]) & !is.na(p_par[,NEB[i]]) & !is.na(p_par[,"EduYears.Parent"]))	
			df$Prevalence <- df$N_Cases/(df$N_Cases+df$N_Controls)
			df$LRS_Cases <- mean(p_par[p_par[,endpoints[ep_n]]==1 & !is.na(p_par[,"age"]) & !is.na(p_par[,NEB[i]]) & !is.na(p_par[,"EduYears.Parent"]), NEB[i]])
			df$LRS_Controls <- mean(p_par[p_par[,endpoints[ep_n]]==0 & !is.na(p_par[,"age"]) & !is.na(p_par[,NEB[i]]) & !is.na(p_par[,"EduYears.Parent"]), NEB[i]])
			df$model <- "gee"
			df$sex <- sexs[sex_n] 
			df$LRS <- NEB[i]
			df$Endpoint <- endpoints[ep_n]
		} else {
			df <- df_NA
			df[1,"model"] <- "gee"
			df[1,"sex"] <- sexs[sex_n]
			df[1,"LRS"] <- NEB[i]
			df[1,"Endpoint"] <- endpoints[ep_n]
		}		
		print(paste0(sexs[sex_n],": ",ep_n))
		print(df)
		if (ep_n==1 & !file.exists(paste0("06_REG_INDEX_GEE_",NEB,"_",sexs[sex_n],".tsv"))){
			write.table(df, paste0("06_REG_INDEX_GEE_",NEB[i],"_",sexs[sex_n],".tsv"), append=F, quote=F, sep=" ", row.names=F, col.names=T)
		} else {
			write.table(df, paste0("06_REG_INDEX_GEE_",NEB[i],"_",sexs[sex_n],".tsv"), append=T, quote=F, sep=" ", row.names=F, col.names=F)
		}
	}
}
