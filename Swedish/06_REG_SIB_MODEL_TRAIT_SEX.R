## Regression analysis for disease status, including sib_of_unsick, sib_of_sick, index_sick, Unsick

mod <- "MODELVAR"
NEB <- "TRAITVAR"  
sex_n <- SEXNVAR
sexs <- c("male","female")

setwd("/home/aoxing/DSGE_LRS/out/registry_edit/REGRESSION/")
r_dir <- "/home/aoxing/DSGE_LRS/input/r_files/"

require(gridExtra)
library(tidyverse)
library(dplyr)
require(ggplot2)
library(survival)  
require(sandwich)
require(msm)
library(boot)
library(simpleboot)
library(lme4)
library(gee)
library(plotly)
library(gnm)  

'%!in%' <- function(x,y)!('%in%'(x,y))



#   ###########################################
#   #            Prepare input                #
#   ###########################################
#   
#   fitness <- data.frame(get(load(paste0(r_dir, "indexW_fitness.Rdata")))) %>% 
#                 select(LopNr,lambda,lambda_image,lambda_5,lambda_image_5,N_spouse,N_year_spouse,Age_first_spouse,has_spouse,N_spouse_Age4550,N_year_spouse_Age4550,Age_first_spouse_Age4550,has_spouse_Age4550,survival_Age4550)
#   
#   
#   phe <- data.frame(get(load(paste0(r_dir, "Phe_IndexW_EndpointSelected.Rdata"))))[,1:37] %>% mutate(LopNr=as.numeric(LopNr)) %>% left_join(fitness,by="LopNr")
#   
#   
#   ry_first_index <- data.frame(get(load(paste0(r_dir, "ry_first_indexW_COMPLETE.Rdata")))) %>% select(LopNr,ENDPOINT) %>% filter(LopNr %in% phe$LopNr)
#   nrow(ry_first_index)   # 14177075 
#   
#    
#   ep_frq <- data.frame(get(load(paste0(r_dir, "Phe_IndexW_ep_frq.Rdata")))) 
#   endpoints <- as.character(ep_frq[ep_frq$Freq>=50,1])
#   
#     
#   parent_multi_child <- phe %>% select(LopNr,LopNrMor,LopNrFar) %>% filter(!is.na(LopNrMor) & !is.na(LopNrFar)) %>% mutate(parent_id=paste0(LopNrMor,"_",LopNrFar)) %>% 
#                                 group_by(parent_id) %>% count() %>% rename(n_sibs=n) %>% filter(n_sibs>1)
#   
#   
#   sibs <- phe %>% mutate(parent_id=paste0(LopNrMor,"_",LopNrFar)) %>% filter(parent_id %in% parent_multi_child$parent_id) %>% select(LopNr) %>% unique()
#   nrow(sibs)  # 1,803,694
#   
#       
#   for (ep_n in 1:length(endpoints)){
#   	index_sick <- ry_first_index %>% filter(ENDPOINT==endpoints[ep_n]) %>% select(LopNr) %>% unique()
#   	parent_of_sick <- phe %>% select(LopNr,LopNrMor,LopNrFar) %>% 
#   		                      filter(LopNr %in% index_sick$LopNr) %>% 
#      	                          filter(!is.na(LopNrMor) & !is.na(LopNrFar)) %>% unique()
#      	                          
#   	sib_of_sick <- phe %>% select(LopNr,LopNrMor,LopNrFar) %>% 
#   	                       filter(LopNr %!in% index_sick$LopNr) %>% 
#    	                       inner_join(parent_of_sick[,c("LopNrMor","LopNrFar")],by=c("LopNrMor"="LopNrMor","LopNrFar"="LopNrFar")) %>% select(LopNr) %>% unique()
#   
#      	sib_of_unsick <- sibs %>% filter(LopNr %!in% sib_of_sick$LopNr) %>% filter(LopNr %!in% index_sick$LopNr) %>% unique() 
#   	
#    	phe[ ,endpoints[ep_n]] <- ifelse(phe$LopNr %in% index_sick$LopNr, "index_sick", 
#     	                          ifelse(phe$LopNr %in% sib_of_sick$LopNr, "sib_of_sick",
#    	                          ifelse(phe$LopNr %in% sib_of_unsick$LopNr, "sib_of_unsick","Unsick")))
#    	                          	                          
#   	print(paste0(ep_n,": ",endpoints[ep_n]))		
#   }
#   
#   save(phe, file=paste0(r_dir, "Phe_Sibstatus_AllEndpointsWith50Cases.Rdata"))
#   # rm(ry_first_index)




###########################################
#         Format input variables          #
###########################################

phe <- data.frame(get(load(paste0(r_dir, "Phe_Sibstatus_AllEndpointsWith50Cases.Rdata"))))
ep_frq <- data.frame(get(load(paste0(r_dir, "Phe_IndexW_ep_frq.Rdata"))))
endpoints <- as.character(ep_frq[ep_frq$Freq>=50,1])

if (file.exists(paste0("06_REG_SIB_",mod,"_",NEB,"_",sexs[sex_n],".tsv"))){
	fi <- read.table(paste0("06_REG_SIB_",mod,"_",NEB,"_",sexs[sex_n],".tsv"), header=T)
	endpoints <- endpoints[endpoints %!in% fi$Endpoint]
}


phe <- within(phe, {
        LopNr <- factor(LopNr)
        FodelseAr <- factor(FodelseAr)
        n_child <- as.numeric(n_child)
        n_child_Age4550 <- as.numeric(n_child_Age4550)
        n_gchild <- as.numeric(n_gchild)
        n_pchild_Age4550 <- ifelse(n_child_Age4550>0,n_child_Age4550,NA)  # only for people with child  

        childless <- as.numeric(childless)
        childless_Age4550 <- as.numeric(childless_Age4550)
        has_child_Age4550 <- as.numeric(ifelse(childless_Age4550==0,1,ifelse(childless_Age4550==1,0,NA)))

        afc <- as.numeric(afc)
        afc_Age4550 <- as.numeric(afc_Age4550)

        survival_Age4550 <- as.numeric(survival_Age4550)
        has_spouse_Age4550 <- as.numeric(ifelse(is.na(survival_Age4550)|(survival_Age4550==0 & !is.na(survival_Age4550)), NA, has_spouse_Age4550))

        lambda_5qc <- as.numeric(as.character(ifelse(lambda_5>0 & lambda_image_5==0,lambda_5,NA)))
        N_spouse_Age4550 <- as.numeric(as.character(N_spouse_Age4550))
        N_year_spouse_Age4550 <- as.numeric(as.character(N_year_spouse_Age4550))
        Age_first_spouse_Age4550 <- as.numeric(as.character(Age_first_spouse_Age4550))
        
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

        income_Age2535_max <- as.numeric(as.character(income_Age2535_max))
        income_Age2535_mean <- as.numeric(as.character(income_Age2535_mean))
        income_Age5060_max.Mor <- as.numeric(as.character(income_Age5060_max.Mor))
        income_Age5060_mean.Mor <- as.numeric(as.character(income_Age5060_mean.Mor))
        income_Age5060_max.Far <- as.numeric(as.character(income_Age5060_max.Far))
        income_Age5060_mean.Far <- as.numeric(as.character(income_Age5060_mean.Far))
        income_Age5060_max.Parent <- as.numeric(as.character(income_Age5060_max.Parent))
        income_Age5060_mean.Parent <- as.numeric(as.character(income_Age5060_mean.Parent))
        income_Age2535_max2 <- income_Age2535_max^2
        income_Age2535_mean2 <- income_Age2535_mean^2
        income_Age5060_max2.Mor <- income_Age5060_max.Mor^2
        income_Age5060_mean2.Mor <- income_Age5060_mean.Mor^2
        income_Age5060_max2.Far <- income_Age5060_max.Far^2
        income_Age5060_mean2.Far <- income_Age5060_mean.Far^2
        income_Age5060_max2.Parent <- income_Age5060_max.Parent^2
        income_Age5060_mean2.Parent <- income_Age5060_mean.Parent^2

        age <- as.numeric(as.character(FodelseAr))-1956
        age2 <- age^2

        LopNrMor_Far_Kon <- paste0(LopNrMor, "_", LopNrFar, "_",Kon)
})
nrow(phe)  # 2,555,541



###########################################
#         Regression analysis             #
###########################################

df_NA <- matrix(NA, ncol=21, nrow=1)
colnames(df_NA) <- c("Estimate","SE","Z","P_val","OR","OR_025","OR_975","N_sib_of_unsick","N_sib_of_sick","N_index_sick","N_Unsick","Prevalence","Prevalence_Sib","LRS_sib_of_unsick","LRS_sib_of_sick","LRS_index_sick","LRS_Unsick" ,"model","sex","LRS","Endpoint")       

i <- 1
p <- phe[!is.na(phe[,NEB[i]]) & phe$Kon==sex_n, ]
nrow(p)   # 530,215   


if (mod=="glm"){     # For the full cohort (regular glm)                    
	for (ep_n in 1:length(endpoints)){
		p[,endpoints[ep_n]] <- factor(p[,endpoints[ep_n]], levels=c("sib_of_unsick","sib_of_sick","index_sick","Unsick"))
		if (sum(p[,endpoints[ep_n]]=="index_sick")>=50 & sum(p[,endpoints[ep_n]]=="index_sick" & p[,NEB]==0)>10 & sum(p[,endpoints[ep_n]]=="sib_of_sick" & p[,NEB]==0)>10){
			if (NEB %in% c("n_child_Age4550","n_pchild_Age4550","afc_Age4550")){
				m_glm <- glm(p[,NEB[i]] ~ p[,endpoints[ep_n]] + age + age2 + EduYears.Parent + EduYears2.Parent, data=p, family="poisson") 
			} else if (NEB %in% c("has_child_Age4550","has_spouse_Age4550","survival_Age4550")){
				m_glm <- glm(p[,NEB[i]] ~ p[,endpoints[ep_n]] + age + age2 + EduYears.Parent + EduYears2.Parent, data=p, family="binomial") 
			} else {
				print("Check NEB name!")
			}
			df <- as.data.frame(t(as.data.frame(summary(m_glm)$coeff["p[, endpoints[ep_n]]sib_of_sick",])))
			rownames(df) <- "Endpoint"	
			df <- df[,c("Estimate","Std. Error","z value","Pr(>|z|)")]
			colnames(df) <- c("Estimate","SE","Z", "P_val")
			df$OR <- exp(df[1,"Estimate"])
			ci <- exp(confint(m_glm,"p[, endpoints[ep_n]]sib_of_sick"))
			df$OR_025 <- ci[1]
			df$OR_975 <- ci[2]				
			df$N_sib_of_unsick <- sum(p[,endpoints[ep_n]]=="sib_of_unsick" & !is.na(p[,"age"]) & !is.na(p[,"EduYears.Parent"]))
			df$N_sib_of_sick <- sum(p[,endpoints[ep_n]]=="sib_of_sick" & !is.na(p[,"age"]) & !is.na(p[,"EduYears.Parent"]))	
			df$N_index_sick <- sum(p[,endpoints[ep_n]]=="index_sick" & !is.na(p[,"age"]) & !is.na(p[,"EduYears.Parent"]))
			df$N_Unsick <- sum(p[,endpoints[ep_n]]=="Unsick" & !is.na(p[,"age"]) & !is.na(p[,"EduYears.Parent"]))	
			df$Prevalence <- df$N_index_sick/(df$N_sib_of_unsick+df$N_sib_of_sick+df$N_index_sick+df$N_Unsick)
			df$Prevalence_Sib <- df$N_sib_of_sick/(df$N_sib_of_unsick+df$N_sib_of_sick)
			df$LRS_sib_of_unsick <- mean(p[p[,endpoints[ep_n]]=="sib_of_unsick" & !is.na(p[,"age"]) & !is.na(p[,"EduYears.Parent"]), NEB[i]])
			df$LRS_sib_of_sick <- mean(p[p[,endpoints[ep_n]]=="sib_of_sick" & !is.na(p[,"age"]) & !is.na(p[,"EduYears.Parent"]), NEB[i]])
			df$LRS_index_sick <- mean(p[p[,endpoints[ep_n]]=="index_sick" & !is.na(p[,"age"]) & !is.na(p[,"EduYears.Parent"]), NEB[i]])
			df$LRS_Unsick <- mean(p[p[,endpoints[ep_n]]=="Unsick" & !is.na(p[,"age"]) & !is.na(p[,"EduYears.Parent"]), NEB[i]])
			df$model <- "glm"
			df$sex <- sexs[sex_n] 
			df$LRS <- NEB[i]	
			df$Endpoint <- endpoints[ep_n]	
		} else {
			df <- df_NA 
			df[1,"model"] <- "glm"
			df[1,"sex"] <- sexs[sex_n]
			df[1,"LRS"] <- NEB[i]
			df[1,"Endpoint"] <- endpoints[ep_n]
		}
		print(paste0(sexs[sex_n],": ",ep_n))
		print(df)
		if (ep_n==1 & !file.exists(paste0("06_REG_SIB_",mod,"_",NEB,"_",sexs[sex_n],".tsv"))){
			write.table(df, paste0("06_REG_SIB_glm_",NEB[i],"_",sexs[sex_n],".tsv"), append=F, quote=F, sep=" ", row.names=F, col.names=T)
		} else {
			write.table(df, paste0("06_REG_SIB_glm_",NEB[i],"_",sexs[sex_n],".tsv"), append=T, quote=F, sep=" ", row.names=F, col.names=F)
		}
	}

} else if (mod=="gee"){   # For the full cohort (GEE)
	p_par <- p[!is.na(p[,"LopNrMor"]) & !is.na(p[,"LopNrFar"]),]	
	p_par <- p_par[order(p_par$LopNrMor_Far_Kon, p_par$FodelseAr),]
	nrow(p_par)  # 496,677
	p_par$LopNrMor_Far_Kon <- as.factor(p_par$LopNrMor_Far_Kon)
	for (ep_n in 1:length(endpoints)){
		p_par[,endpoints[ep_n]] <- factor(p_par[,endpoints[ep_n]], levels=c("sib_of_unsick","sib_of_sick","index_sick","Unsick"))
		if (sum(p_par[,endpoints[ep_n]]=="index_sick")>=50 & sum(p_par[,endpoints[ep_n]]=="index_sick" & p_par[,NEB]==0)>10 & sum(p_par[,endpoints[ep_n]]=="sib_of_sick" & p_par[,NEB]==0)>10){
			if (NEB %in% c("n_child_Age4550","n_pchild_Age4550","afc_Age4550")){
				m_gee <- gee(p_par[,NEB[i]] ~ p_par[,endpoints[ep_n]] + age + age2 + EduYears.Parent + EduYears2.Parent, data=p_par, family="poisson", id=LopNrMor_Far_Kon, corstr="exchangeable") 
			} else if (NEB %in% c("has_child_Age4550","has_spouse_Age4550","survival_Age4550")){
				m_gee <- gee(p_par[,NEB[i]] ~ p_par[,endpoints[ep_n]] + age + age2 + EduYears.Parent + EduYears2.Parent, data=p_par, family="binomial", id=LopNrMor_Far_Kon, corstr="exchangeable") 
			} else {
				print("Check NEB name!")
			}
			df <- as.data.frame(t(as.data.frame(summary(m_gee)$coeff["p_par[, endpoints[ep_n]]sib_of_sick",])))		
			rownames(df) <- "Endpoint"			
			df$P_val <- 2 * pnorm(abs(df[,"Robust z"]), lower.tail = FALSE)
			df <- df[,c("Estimate","Robust S.E.","Robust z", "P_val")]
			colnames(df) <- c("Estimate","SE","Z", "P_val")		
			df$OR <- exp(df[1,"Estimate"])
			ci <- exp(df[1,"Estimate"] + c(-1, 1) * df[1,"SE"] * qnorm(0.975))
			df$OR_025 <- ci[1]
			df$OR_975 <- ci[2]								
			df$N_sib_of_unsick <- sum(p_par[,endpoints[ep_n]]=="sib_of_unsick" & !is.na(p_par[,"age"]) & !is.na(p_par[,"EduYears.Parent"]))
			df$N_sib_of_sick <- sum(p_par[,endpoints[ep_n]]=="sib_of_sick" & !is.na(p_par[,"age"]) & !is.na(p_par[,"EduYears.Parent"]))	
			df$N_index_sick <- sum(p_par[,endpoints[ep_n]]=="index_sick" & !is.na(p_par[,"age"]) & !is.na(p_par[,"EduYears.Parent"]))
			df$N_Unsick <- sum(p_par[,endpoints[ep_n]]=="Unsick" & !is.na(p_par[,"age"]) & !is.na(p_par[,"EduYears.Parent"]))	
			df$Prevalence <- df$N_index_sick/(df$N_sib_of_unsick+df$N_sib_of_sick+df$N_index_sick+df$N_Unsick)
			df$Prevalence_Sib <- df$N_sib_of_sick/(df$N_sib_of_unsick+df$N_sib_of_sick)
			df$LRS_sib_of_unsick <- mean(p_par[p_par[,endpoints[ep_n]]=="sib_of_unsick" & !is.na(p_par[,"age"]) & !is.na(p_par[,"EduYears.Parent"]), NEB[i]])
			df$LRS_sib_of_sick <- mean(p_par[p_par[,endpoints[ep_n]]=="sib_of_sick" & !is.na(p_par[,"age"]) & !is.na(p_par[,"EduYears.Parent"]), NEB[i]])
			df$LRS_index_sick <- mean(p_par[p_par[,endpoints[ep_n]]=="index_sick" & !is.na(p_par[,"age"]) & !is.na(p_par[,"EduYears.Parent"]), NEB[i]])
			df$LRS_Unsick <- mean(p_par[p_par[,endpoints[ep_n]]=="Unsick" & !is.na(p_par[,"age"]) & !is.na(p_par[,"EduYears.Parent"]), NEB[i]])
			df$model <- "gee"
			df$sex <- sexs[sex_n] 
			df$LRS <- NEB[i]
			df$Endpoint <- endpoints[ep_n]
		}else{
			df <- df_NA 
			df[1,"model"] <- "gee"
			df[1,"sex"] <- sexs[sex_n]
			df[1,"LRS"] <- NEB[i]   
			df[1,"Endpoint"] <- endpoints[ep_n]
		}		
		print(paste0(sexs[sex_n],": ",ep_n))	
		print(df)
		if (ep_n==1 & !file.exists(paste0("06_REG_SIB_",mod,"_",NEB,"_",sexs[sex_n],".tsv"))){		
			write.table(df, paste0("06_REG_SIB_gee_",NEB[i],"_",sexs[sex_n],".tsv"), append=F, quote=F, sep=" ", row.names=F, col.names=T)
		} else {
			write.table(df, paste0("06_REG_SIB_gee_",NEB[i],"_",sexs[sex_n],".tsv"), append=T, quote=F, sep=" ", row.names=F, col.names=F)
		}		
	}
	
} else {
	print("Check model name!")
}





