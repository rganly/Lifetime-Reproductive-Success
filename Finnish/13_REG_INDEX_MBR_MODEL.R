## Quality of children using MBR

setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"

library(ggplot2, lib.loc="/homes/aliu/anaconda3/lib/R/library")
library(gridExtra)
library(dplyr, lib.loc="/homes/aliu/anaconda3/lib/R/library") 
library(gee)
library(gnm, lib.loc="/homes/aliu/anaconda3/lib/R/library")  
library(survival, lib.loc="/homes/aliu/anaconda3/lib/R/library")  

'%!in%' <- function(x,y)!('%in%'(x,y))




#############################################
#              Prepare data                 #
#############################################

i <- 1
sexs <- "female"
sex_n <- 1

df_NA <- matrix(NA, ncol=16, nrow=1)
colnames(df_NA) <- c("Estimate","SE","Z","P_val","OR","OR_025","OR_975","N_Cases","N_Controls","Prevalence","LRS_Cases","LRS_Controls","model","sex","LRS","Endpoint")
df_gnm_NA <- matrix(NA, ncol=15, nrow=1)
colnames(df_gnm_NA) <- c("Estimate","SE","Z","P_val","OR","OR_025","OR_975","N_Family","N_Case","N_Control","N_Family_Case","model","sex","LRS","Endpoint")


# N of preterm births -----------------------------------
NEB <- "n_preterm"
p <- data.frame(get(load(paste0(r_dir, "Preterm_N_IndexW_AllEndpointsWith50Cases.Rdata"))))
p <- within(p, {
	TNRO <- factor(TNRO)
	n_preterm <- as.numeric(n_preterm)
	afc_Age4550 <- as.numeric(ifelse(childless_Age4550==0, afc, NA))
	afc2_Age4550 <- afc_Age4550^2
	n_child_Age4550 <- as.numeric(n_child_Age4550)
	n_child2_Age4550 <- n_child_Age4550^2
	b_year <- factor(substr(SUKULAISEN_SYNTYMAPV,1,4)) 
	age <- as.numeric(as.character(b_year))-1956	
	age2 <- age^2	
	EduYears.Parent <- as.numeric(as.character(EduYears.Parent))	
	EduYears2.Parent <- EduYears.Parent^2
	LopNrMor_Far <- paste0(mother_id, "_", father_id)	
})

# N of malform births -----------------------------------
NEB <- "n_malform"
# NEB <- "has_malform"
p <- data.frame(get(load(paste0(r_dir, "Malform_N_IndexW_AllEndpointsWith50Cases.Rdata"))))
p <- within(p, {
	TNRO <- factor(TNRO)
	n_preterm <- as.numeric(n_preterm)
	n_malform <- as.numeric(n_malform)
	has_malform <- as.numeric(has_malform)
	afc_Age4550 <- as.numeric(ifelse(childless_Age4550==0, afc, NA))
	afc2_Age4550 <- afc_Age4550^2
	n_child_Age4550 <- as.numeric(n_child_Age4550)
	n_child2_Age4550 <- n_child_Age4550^2
	b_year <- factor(substr(SUKULAISEN_SYNTYMAPV,1,4)) 
	age <- as.numeric(as.character(b_year))-1956	
	age2 <- age^2	
	EduYears.Parent <- as.numeric(as.character(EduYears.Parent))	
	EduYears2.Parent <- EduYears.Parent^2
	LopNrMor_Far <- paste0(mother_id, "_", father_id)	
})


# set parameter and read in data -----------------------------------
eps <- colnames(p)[which(colnames(p)=="O15_DELIV_SPONT"):which(colnames(p)=="M13_CHONDROLYSIS")]
endpoints <- NULL
for (k in eps) {
	print(k)
	if (sum(p[,k])>=50){
		endpoints <- c(endpoints,k)
	}
}
length(endpoints)  # 1,379

# if (file.exists(paste0("06_REG_INDEX_",toupper(mod),"_",NEB,"_",sexs[sex_n],".tsv"))){
# 	fi <- read.table(paste0("06_REG_INDEX_",toupper(mod),"_",NEB,"_",sexs[sex_n],".tsv"), header=T)
# 	endpoints <- endpoints[endpoints %!in% fi$Endpoint]
# }




#############################################
#            Regression analysis            #
#############################################

mod <- "glm"
mod <- "gee"
mod <- "cond"

# N_preterm_birth = disease + AFC + NEB + age + age^2 + EduYears.Parent + EduYears.Parent^2

if (mod=="glm"){     # For the full cohort (regular glm) 
	# for (ep_n in 1:length(endpoints)){
	for (ep_n in 1361:length(endpoints)){
		if (sum(p[,endpoints[ep_n]])>=50 & ((sum(p[,NEB]==0 & p[,endpoints[ep_n]]==1)>=10 & sum(p[,NEB[i]]==0 & p[,endpoints[ep_n]]==0)>=10)|(NEB %in% c("n_malform","n_preterm","n_child_Age4550","n_pchild_Age4550","afc_Age4550")))){	
			if (NEB %in% c("n_malform","n_preterm","n_child_Age4550","n_pchild_Age4550","afc_Age4550")){
				m_glm <- glm(p[,NEB[i]] ~ p[,endpoints[ep_n]] + afc_Age4550 + afc2_Age4550 + n_child_Age4550 + n_child2_Age4550 + age + age2 + EduYears.Parent + EduYears2.Parent, data=p, family="poisson") 
			} else if (NEB %in% c("has_malform","deliveryAssistGrp2","deliveryCSGrp2","pretermGrp2","has_child_Age4550","has_spouse_Age4550","survival_Age4550")){
				m_glm <- glm(p[,NEB[i]] ~ p[,endpoints[ep_n]] + afc_Age4550 + afc2_Age4550 + n_child_Age4550 + n_child2_Age4550 + age + age2 + EduYears.Parent + EduYears2.Parent, data=p, family="binomial") 
			} else {
				print("Check NEB name!")
			}
			df <- as.data.frame(t(as.data.frame(summary(m_glm)$coeff["p[, endpoints[ep_n]]",])))
			rownames(df) <- "Endpoint"	
			df <- df[,c("Estimate","Std. Error","z value","Pr(>|z|)")]
			colnames(df) <- c("Estimate","SE","Z", "P_val")
			df$OR <- exp(df[1,"Estimate"])
			ci <- exp(confint(m_glm,"p[, endpoints[ep_n]]"))
			df$OR_025 <- ci[1]
			df$OR_975 <- ci[2]
			df$N_Cases <- sum(p[,endpoints[ep_n]]==1 & !is.na(p[,"age"]) & !is.na(p[,"EduYears.Parent"]))
			df$N_Controls <- sum(p[,endpoints[ep_n]]==0 & !is.na(p[,"age"]) & !is.na(p[,"EduYears.Parent"]))	
			df$Prevalence <- df$N_Cases/(df$N_Cases+df$N_Controls)
			df$LRS_Cases <- mean(p[p[,endpoints[ep_n]]==1 & !is.na(p[,"age"]) & !is.na(p[,"EduYears.Parent"]), NEB[i]])
			df$LRS_Controls <- mean(p[p[,endpoints[ep_n]]==0 & !is.na(p[,"age"]) & !is.na(p[,"EduYears.Parent"]), NEB[i]])
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
		if (!file.exists(paste0("06_REG_INDEX_GLM_",NEB,"_",sexs[sex_n],".tsv"))){
			write.table(df, paste0("06_REG_INDEX_GLM_",NEB[i],"_",sexs[sex_n],".tsv"), append=F, quote=F, sep=" ", row.names=F, col.names=T)
		} else {
			write.table(df, paste0("06_REG_INDEX_GLM_",NEB[i],"_",sexs[sex_n],".tsv"), append=T, quote=F, sep=" ", row.names=F, col.names=F)
		}
	}

} else if (mod=="gee"){   # For the full cohort (GEE)
	p_par <- p[!is.na(p[,"mother_id"]) & !is.na(p[,"father_id"]),]
	p_par <- p_par[order(p_par$LopNrMor_Far, p_par$b_year),]
	nrow(p_par)  # 496,677
	p_par$LopNrMor_Far <- as.factor(p_par$LopNrMor_Far)
	for (ep_n in 1:length(endpoints)){
		if (sum(p_par[,endpoints[ep_n]])>=50 & ((sum(p_par[,NEB]==0 & p_par[,endpoints[ep_n]]==1)>=10 & sum(p_par[,NEB]==0 & p_par[,endpoints[ep_n]]==0)>=10)|(NEB %in% c("n_malform","n_preterm","n_child_Age4550","n_pchild_Age4550","afc_Age4550")))){	
			if (NEB %in% c("n_malform","n_preterm","n_child_Age4550","n_pchild_Age4550","afc_Age4550")){
				m_gee <- gee(p_par[,NEB[i]] ~ p_par[,endpoints[ep_n]] + afc_Age4550 + afc2_Age4550 + n_child_Age4550 + n_child2_Age4550 + age + age2 + EduYears.Parent + EduYears2.Parent, data=p_par, family="poisson", id=LopNrMor_Far, corstr="exchangeable") 
			} else if (NEB %in% c("has_malform","has_child_Age4550","has_spouse_Age4550","survival_Age4550")){
				m_gee <- gee(p_par[,NEB[i]] ~ p_par[,endpoints[ep_n]] + afc_Age4550 + afc2_Age4550 + n_child_Age4550 + n_child2_Age4550 + age + age2 + EduYears.Parent + EduYears2.Parent, data=p_par, family="binomial", id=LopNrMor_Far, corstr="exchangeable") 
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
	
} else if (mod=="cond"){   # For the sibling cohort (conditional poisson/logistic regression)
	dup <- duplicated(p[,"LopNrMor_Far"])
	sum(dup)  # 135,469
	pp <- p[!is.na(p$mother_id) & !is.na(p$father_id) & p$LopNrMor_Far %in% p[dup,"LopNrMor_Far"],]
	nrow(pp)  # 248,255
	pp <- pp[order(pp$LopNrMor_Far,as.integer(pp$b_year)),]
	length(unique(pp$LopNrMor_Far))  # 115,087
	pp$LopNrMor_Far <- factor(pp$LopNrMor_Far)
	# for (ep_n in 1:length(endpoints)){
	for (ep_n in 1202:length(endpoints)){
		if (sum(pp[,endpoints[ep_n]])>=50 & ((sum(pp[,NEB]==0 & pp[,endpoints[ep_n]]==1)>=10 & sum(pp[,NEB]==0 & pp[,endpoints[ep_n]]==0)>=10)|(NEB %in% c("n_malform","n_preterm","n_child_Age4550","n_pchild_Age4550","afc_Age4550")))){	
			if (NEB %in% c("n_malform","n_preterm","n_child_Age4550","n_pchild_Age4550","afc_Age4550")){
				m_cond <- gnm(pp[,NEB[i]] ~ pp[,endpoints[ep_n]] + afc_Age4550 + afc2_Age4550 + n_child_Age4550 + n_child2_Age4550 + age + age2 + EduYears.Parent + EduYears2.Parent, family=poisson, eliminate=factor(LopNrMor_Far), data=pp)		
				df <- as.data.frame(t(as.data.frame(summary(m_cond)$coeff["pp[, endpoints[ep_n]]",])))			
				rownames(df) <- "Endpoint"
				df <- df[,c("Estimate","Std. Error","z value","Pr(>|z|)")] 
				colnames(df) <- c("Estimate","SE","Z", "P_val")
				df$OR <- exp(df[1,"Estimate"])
				conf_095 <- confint(m_cond, parm=1, level=0.95,)
				df$OR_025 <- exp(conf_095[1])
				df$OR_975 <- exp(conf_095[2])
				df$N_Family <- length(unique(pp[,c("LopNrMor_Far")]))
				df$N_Sample <- nrow(pp[,c("LopNrMor_Far")])
				df$N_Case <- sum(pp[,endpoints[ep_n]]==1)
				df$N_Control<- sum(pp[,endpoints[ep_n]]==0)
				df$N_Family_Case <- length(unique(intersect(pp[pp[,endpoints[ep_n]]==1,"LopNrMor_Far"], pp[pp[,endpoints[ep_n]]==0,"LopNrMor_Far"])))
				df$model <- "cond"
				df$sex <- sexs[sex_n] 
				df$LRS <- NEB[i]
				df$Endpoint <- endpoints[ep_n]
			} else if (NEB %in% c("has_malform","has_child_Age4550","has_spouse_Age4550","survival_Age4550")){
				ppp <- pp[pp$LopNrMor_Far %in% intersect(pp[pp[,NEB[i]]==0,"LopNrMor_Far"],pp[pp[,NEB[i]]==1,"LopNrMor_Far"]),]
				m_cond <- clogit(ppp[,NEB[i]] ~ ppp[,endpoints[ep_n]] + afc_Age4550 + afc2_Age4550 + n_child_Age4550 + n_child2_Age4550 + age + age2 + strata(LopNrMor_Far), data=ppp)
				df <- as.data.frame(t(as.data.frame(summary(m_cond)$coeff["ppp[, endpoints[ep_n]]",])))	
				rownames(df) <- "Endpoint"
				df <- df[,c("coef","se(coef)","z","Pr(>|z|)")]
				colnames(df) <- c("Estimate","SE","Z", "P_val")
				df$OR <- summary(m_cond)$conf.int["ppp[, endpoints[ep_n]]","exp(coef)"]
				df$OR_25 <- summary(m_cond)$conf.int["ppp[, endpoints[ep_n]]",3]
				df$OR_975 <- summary(m_cond)$conf.int["ppp[, endpoints[ep_n]]",4]
				df$N_Family <- length(unique(ppp[,c("LopNrMor_Far")]))
				df$N_Sample <- nrow(ppp[,c("LopNrMor_Far")])
				df$N_Case <- sum(ppp[,endpoints[ep_n]]==1)
				df$N_Control<- sum(ppp[,endpoints[ep_n]]==0)
				df$N_Family_Case <- length(unique(c(intersect(ppp[ppp[,endpoints[ep_n]]==1 & ppp[,NEB[i]]==1,"LopNrMor_Far"], ppp[ppp[,endpoints[ep_n]]==0 & ppp[,NEB[i]]==0,"LopNrMor_Far"]),
	                                       intersect(ppp[ppp[,endpoints[ep_n]]==1 & ppp[,NEB[i]]==0,"LopNrMor_Far"], ppp[ppp[,endpoints[ep_n]]==0 & ppp[,NEB[i]]==1,"LopNrMor_Far"]))))
				df$model <- "cond"
				df$sex <- sexs[sex_n] 
				df$LRS <- NEB[i]
				df$Endpoint <- endpoints[ep_n]
			} else {
				print("Check NEB name!")
			}
		} else {
			df <- df_gnm_NA 
			df[1,"model"] <- "cond"
			df[1,"sex"] <- sexs[sex_n]
			df[1,"LRS"] <- NEB[i]
			df[1,"Endpoint"] <- endpoints[ep_n]
		}
		print(paste0(sexs[sex_n],": ",ep_n))
		print(df)
		if (ep_n==1 & !file.exists(paste0("06_REG_INDEX_COND_",NEB,"_",sexs[sex_n],".tsv"))){
			write.table(df, paste0("06_REG_INDEX_COND_",NEB[i],"_",sexs[sex_n],".tsv"), append=F, quote=F, sep=" ", row.names=F, col.names=T)
		} else {
			write.table(df, paste0("06_REG_INDEX_COND_",NEB[i],"_",sexs[sex_n],".tsv"), append=T, quote=F, sep=" ", row.names=F, col.names=F)
		}
	}		

} else {
	print("Check model name!")
}


