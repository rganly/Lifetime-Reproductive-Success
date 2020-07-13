## 13.1 Population-based regression analysis


setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"


# require(gridExtra)
# require(ggplot2)
# install.packages("sandwich", lib="/homes/aliu/anaconda3/lib/R/library")
require(sandwich, lib.loc="/homes/aliu/anaconda3/lib/R/library")
# install.packages("msm", lib="/homes/aliu/anaconda3/lib/R/library")
require(msm, lib.loc="/homes/aliu/anaconda3/lib/R/library")
# library(tidyverse)
# install.packages("plotly", lib="/homes/aliu/anaconda3/lib/R/library")
# library(plotly, lib.loc="/homes/aliu/anaconda3/lib/R/library")


'%!in%' <- function(x,y)!('%in%'(x,y))
  
    
    
    
    
    
#    ###########################################
#    #            Prepare input                #
#    ###########################################
#    
#    ## Read in data 
#    
#    lrs_all <- get(load(paste0(r_dir, "index_lrs_all.Rdata")))                             
#    nrow(lrs_all)       # 2,365,707 
#    
#    index_edu <- get(load(paste0(r_dir, "index_edu.Rdata"))) 
#    nrow(index_edu)     # 2,365,584
#    
#    index_lrs_edu <- merge(lrs_all, index_edu, by.x="KANTAHENKILON_TNRO", by.y="id")
#    dim(index_lrs_edu)  # 2,365,584      28
#    
#    
#    ry_first_index <- get(load(paste0(r_dir, "ry_first_index.Rdata")))[,c("ID","ENDPOINT")]
#    nrow(ry_first_index)   # 14747135 
#     
#     
#     
#    ## Extract specific endpoints                        
#    
#    ep_f <- as.data.frame(table(ry_first_index[,"ENDPOINT"]))
#    colnames(ep_f) <- c("ENDPOINT","count")
#    ep_f$ENDPOINT <- as.character(ep_f$ENDPOINT)
#    ep_f <- ep_f[order(-ep_f$count),]
#    nrow(ep_f)      # 2,339
#    
#    
#    for (i in 1:2339){
#    	ep_i <- ep_f[i,"ENDPOINT"]
#    	rfi_i <- ry_first_index[ry_first_index[,"ENDPOINT"]==ep_i,"ID"]
#    	index_lrs_edu[ ,ep_i] <- as.integer(index_lrs_edu[,"KANTAHENKILON_TNRO"] %in% rfi_i)
#    	print(paste0(i,": ",ep_i))	
#    }
#    
#    save(index_lrs_edu, file=paste0(r_dir, "INDEX_LRS_EDU_ENDPOINT_FIRST_1_2339.Rdata"))
#    
#    
#    
#    
#
#
#    ###########################################
#    #  Population-based Regression analysis   #
#    ###########################################
#
#
#    # endpoints <- c("F5_SCHZPHR","F5_BIPO","F5_DEPRESSIO","F5_AUTISM","F5_ANOREX","F5_ALCOHOL",
#                     "J10_ASTHMA","J10_PNEUMONAS","G6_SLEEPAPNO","E4_DM1","E4_DM2",
#                     "N14_MALEINFERT","N14_FIOTHNAS","N14_FITUB","M13_ARTHROSIS_KNEE")  
#
#    endpoints <- ep_f$ENDPOINT
#
#
#    ep <- index_lrs_edu[ ,c("KANTAHENKILON_TNRO","n_child","n_gchild","SUKULAISEN_SYNTYMAPV","SUKUPUOLI","SYNTYMAKOTIKUNTA","ISCED97","ISCED97.Parent","EduYears","EduYears.Parent",endpoints)]
#    rm(index_lrs_edu)
#
#

ep <- as.data.frame(get(load(paste0(r_dir, "INDEX_LRS_EDU_ENDPOINT_FIRST_1_2339.Rdata"))))
endpoints <- colnames(ep)[29:2367]

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

sink(file="pop_reg_selected_endpoint_1001_1500_qsub.output", append=F, type="output", split=T)

#for (ep_n in 1:length(endpoints)){
for (ep_n in 1001:1500){

	print(" ")
	print("----------------------------------------------------------------------------------------")
	print(paste0(ep_n,": ",endpoints[ep_n]))	
		
	for (sex_n in 1:2){	
		ep_s <- ep[ep$SUKUPUOLI==sex_n, ]
		ep_s[,endpoints[ep_n]] <- as.numeric(as.character(ep_s[,endpoints[ep_n]]))	
			
		p <- round(100*nrow(ep_s[ep_s[,endpoints[ep_n]]==1, ])/nrow(ep_s),2)
		m_lrs_0 <- round(mean(ep_s[ep_s[,endpoints[ep_n]]==0,"n_child"]),3)
		m_lrs_1 <- round(mean(ep_s[ep_s[,endpoints[ep_n]]==1,"n_child"]),3)
		
		print(sexs[sex_n])
		print(paste0("N of individuals is ",nrow(ep_s)))
		print(paste0("Prevalence in ",sexs[sex_n], " is ", p))
		print(paste0("Mean n_child is ", m_lrs_1, " for cases and ", m_lrs_0," for control."))
		
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
						"Prevalence" = p,
						"N.control" = nrow(ep_s[ep_s[,endpoints[ep_n]]==0,]),
						"N.case" = nrow(ep_s[ep_s[,endpoints[ep_n]]==1,]),						
						"Avg.NEB.control" = m_lrs_0,
						"Avg.NEB.case" = m_lrs_1,
						"Variable" = c("Intercept","Age","Age2","ENDPOINT",paste0("ISCED97.Parent",2:6)),
						"Estimate" = coef(m_3),
						"Estimate.SE" = std.err,
						"Estimate.Pval" = 2 * pnorm(abs(coef(m_3)/std.err), lower.tail=FALSE),
						"Estimate.LowerCI" = coef(m_3) - 1.96 * std.err,
						"Estimate.UpperCI" = coef(m_3) + 1.96 * std.err
				)
		if (ep_n==1 & sex_n==1)	{		
			write.table(res, "Population_based_regression_endpoint_1001_1500_qsub.tsv", append=F, quote=F, sep=" ", row.names=F, col.names=T)
		} else {
			write.table(res, "Population_based_regression_endpoint_1001_1500_qsub.tsv", append=T, quote=F, sep=" ", row.names=F, col.names=F)		
		}
	}

}

sink()






###########################################
#      Plot the results (interactive)     #
###########################################

system("less Population_based_regression_endpoint_1001_1500_qsub.tsv|head -n 1|awk '{print $1,$2,$3,$5,$6,$7,$8,$9,$11,$12,$13}' ")
system("less Population_based_regression_endpoint_1001_1500_qsub.tsv|grep ENDPOINT|awk '{print $1,$2,$3,$5,$6,$7,$8,$9,$11,$12,$13}' > pop_reg_results_1001_1500_FIN.txt")

#     # scp aliu@atlas.fimm.fi:/homes/aliu/DSGE_LRS/output/registry_edit/pop_reg_results_FIN.txt  /Users/aoxliu/Downloads/
#     # setwd("/Users/aoxliu/Downloads/")
#     
#     res_plot <- function(country){
#     
#     	res <- read.table(paste0("pop_reg_results_", country ,".txt"),header=F)[,1:9]
#     	
#     	# colnames(res) <- c("Endpoint","sex","Prevalence","N.case","Avg.NEB.control","Avg.NEB.case","Variable","Estimate","Estimate.Pval","Estimate.LowerCI","Estimate.UpperCI")
#     	colnames(res) <- c("Endpoint","sex","Prevalence","N.case","Avg.NEB.control","Avg.NEB.case","Variable","Estimate","Estimate.Pval")
#     	
#     	res <- res[order(res$Endpoint),]
#     	res$rank <- as.numeric(as.factor(res$Endpoint))
#     	res$group <- substr(res$Endpoint,1,1)
#     	hlines <- aggregate(rank ~ group, res, max)[,"rank"] + 0.5
#     	
#     	pp <- ggplot(data = res, aes(x = rank, y = Estimate, color = sex,
#     	                            text = paste("Country: ", country, "\n",
#     	                            			 "Endpoint: ", Endpoint, "\n",
#     	                                         "sex: ", sex, "\n",                            
#     	                                         "Prevalence: ", Prevalence, "\n",
#     	                                         "N.case: ", N.case, "\n", 
#     	                                         # "N.control: ", N.control, "\n",    
#     	                                         "Avg.NEB.case: ", Avg.NEB.case, "\n", 
#     	                                         "Avg.NEB.control: ", Avg.NEB.control, "\n",  
#     	                                         "Estimate: ", Estimate, "\n",
#     	                                         "Estimate.Pval: ", Estimate.Pval, "\n", sep = ""))) +
#     	      geom_point(alpha=1, shape=1) + 
#     	      #labs(x = "FinnGen samples (born 1956-1982)", y = "Index person (born 1956-1982)", title="Prevalence (%) of endpoint") +
#     	      labs(y=paste0("Estimates in ", country)) +
#     	      geom_hline(yintercept = 0, linetype="dotted", color = "grey", size=0.6) +
#     	      geom_vline(xintercept = hlines, linetype="dotted", color = "grey", size=0.6) + 
#     	     # xlim(0, 40) + ylim(0, 40) + 
#     	      theme_classic()
#     	pp <- ggplotly(pp, tooltip="text")
#     	
#     	return(pp)
#           
#     }
#     
#     p_FIN <- res_plot(country="FIN")
#     p_SWE <- res_plot(country="SWE")
#     
#     subplot(p_FIN, style(p_SWE, showlegend=F), nrows=2)




