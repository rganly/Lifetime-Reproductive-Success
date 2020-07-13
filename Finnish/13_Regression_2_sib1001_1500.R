## 13.2 Sibling-based Regression analysis


setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"


# require(gridExtra)
# require(ggplot2)
# install.packages("sandwich", lib="/homes/aliu/anaconda3/lib/R/library")
require(sandwich, lib.loc="/homes/aliu/anaconda3/lib/R/library")
#install.packages("msm", lib="/homes/aliu/anaconda3/lib/R/library")
require(msm, lib.loc="/homes/aliu/anaconda3/lib/R/library")
# library(tidyverse)
# install.packages("plotly", lib="/homes/aliu/anaconda3/lib/R/library")
# library(plotly, lib.loc="/homes/aliu/anaconda3/lib/R/library")


'%!in%' <- function(x,y)!('%in%'(x,y))


ep <- as.data.frame(get(load(paste0(r_dir, "INDEX_LRS_EDU_ENDPOINT_FIRST_012_1_2339.Rdata"))))


endpoints <- colnames(ep)[11:2349]

ep <- ep[ ,c("KANTAHENKILON_TNRO","n_child","n_gchild","SUKULAISEN_SYNTYMAPV","SUKUPUOLI","SYNTYMAKOTIKUNTA","ISCED97","ISCED97.Parent","EduYears","EduYears.Parent",endpoints[1001:1500])]


ep <- within(ep, {

	KANTAHENKILON_TNRO <- factor(KANTAHENKILON_TNRO)
	FodelseAr <- factor(substr(SUKULAISEN_SYNTYMAPV,1,4)) 
	n_child <- as.numeric(n_child)
	
	EduYears <- as.numeric(EduYears)
	EduYears.Parent <- as.numeric(EduYears.Parent)	
	ISCED97 <- factor(ISCED97) 
	ISCED97.Parent <- factor(ISCED97.Parent) 
		
	age <- as.numeric(as.character(FodelseAr))-1956
	age2 <- age^2
	EduYears.Parent2 <- as.numeric(EduYears.Parent)^2

})


sexs <- c("male","female")

#sink(file="popsib_reg_selected_endpoint_specific.output", append=F, type="output", split=T)
sink(file="popsib_reg_selected_endpoint_1001_1500.output", append=F, type="output", split=T)

for (ep_n in 1001:1500){
	print(" ")
	print("----------------------------------------------------------------------------------------")
	print(paste0(ep_n,": ",endpoints[ep_n]))	
		
	for (sex_n in 1:2){	
		ep_s <- ep[ep$SUKUPUOLI==sex_n, ]
		
		ep_s[,endpoints[ep_n]] <- as.numeric(as.character(ep_s[,endpoints[ep_n]]))		
		p_1 <- round(100*nrow(ep_s[ep_s[,endpoints[ep_n]]==1, ])/nrow(ep_s),2)
		p_2 <- round(100*nrow(ep_s[ep_s[,endpoints[ep_n]]==2, ])/nrow(ep_s),2)
		
		m_lrs_0 <- round(mean(ep_s[ep_s[,endpoints[ep_n]]==0,"n_child"]),3)
		m_lrs_1 <- round(mean(ep_s[ep_s[,endpoints[ep_n]]==1,"n_child"]),3)
		m_lrs_2 <- round(mean(ep_s[ep_s[,endpoints[ep_n]]==2,"n_child"]),3)
			
		print(sexs[sex_n])
		print(paste0("N of individuals is ",nrow(ep_s)))
		print(paste0("Prevalence in ",sexs[sex_n], " is ", p_1, " for cases, and ", p_2, " for case's sibling."))
		print(paste0("Mean n_child is ", m_lrs_1, " for cases, ", m_lrs_2, " for case's sibling, and ", m_lrs_0, " for control."))

		ep_s[,endpoints[ep_n]] <- factor(ep_s[,endpoints[ep_n]])	
		
		ep_s_1 <- ep_s[as.numeric(as.character(ep_s$ISCED97.Parent))>0, ]		
		#m_1 <- glm(ep_s_1$n_child ~ ep_s_1$FodelseAr + ep_s_1[,endpoints[ep_n]] + ep_s_1$ISCED97.Parent, family="poisson")
		#m_2 <- glm(ep_s_1$n_child ~ ep_s_1$FodelseAr + ep_s_1[,endpoints[ep_n]] + ep_s_1$EduYears.Parent + ep_s_1$EduYears.Parent2, family="poisson")
		m_3 <- glm(ep_s_1$n_child ~ ep_s_1$age + ep_s_1$age2 + ep_s_1[,endpoints[ep_n]] + ep_s_1$ISCED97.Parent, family="poisson")  ##
		#m_4 <- glm(ep_s_1$n_child ~ ep_s_1$age + ep_s_1$age2 + ep_s_1[,endpoints[ep_n]] + ep_s_1$EduYears.Parent + ep_s_1$EduYears.Parent2, family="poisson")
		
		cov.m_3 <- vcovHC(m_3, type="HC0")
		std.err <- sqrt(diag(cov.m_3))
		
		print(" ")
		print("########################################################################################")
		print(summary(m_3))
		print(" ")
		
		res <- cbind("Endpoint" = endpoints[ep_n],
						"sex" = sexs[sex_n],
						"Prevalence.case" = p_1,
						"Prevalence.sib_of_case" = p_2,
						"N.control" = nrow(ep_s[ep_s[,endpoints[ep_n]]==0,]),
						"N.case" = nrow(ep_s[ep_s[,endpoints[ep_n]]==1,]),	
						"N.sib_of_case" = nrow(ep_s[ep_s[,endpoints[ep_n]]==2,]),							
						"Avg.NEB.control" = m_lrs_0,
						"Avg.NEB.case" = m_lrs_1,
						"Avg.NEB.sib_of_case" = m_lrs_2,
						"Variable" = c("Intercept","Age","Age2","ENDPOINT.case","ENDPOINT.sib_of_case",paste0("ISCED97.Parent",3:6)),
						"Estimate" = coef(m_3),
						"Estimate.SE" = std.err,
						"Estimate.Pval" = 2 * pnorm(abs(coef(m_3)/std.err), lower.tail=FALSE),
						"Estimate.LowerCI" = coef(m_3) - 1.96 * std.err,
						"Estimate.UpperCI" = coef(m_3) + 1.96 * std.err
				)
		if (ep_n==1 & sex_n==1)	{		
			write.table(res, "popsib_reg_selected_endpoint_1001_1500_qsub.tsv", append=F, quote=F, sep=" ", row.names=F, col.names=T)
		} else {
			write.table(res, "popsib_reg_selected_endpoint_1001_1500_qsub.tsv", append=T, quote=F, sep=" ", row.names=F, col.names=F)		
		}
	}

}

sink()



