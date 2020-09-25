## This script is to perform regression analysis


setwd("/home/aoxing/DSGE_LRS/out/registry_edit/")
r_dir <- "/home/aoxing/DSGE_LRS/input/r_files/"


require(gridExtra)
library(tidyverse)
require(ggplot2)
require(sandwich)
require(msm)


'%!in%' <- function(x,y)!('%in%'(x,y))



###########################################
#            Prepare input                #
###########################################

## Read in data 
lrs_all <- data.frame(get(load(paste0(r_dir, "indexW_LRS.Rdata"))))                             
nrow(lrs_all)  # 2,555,541

index_edu <- data.frame(get(load(paste0(r_dir, "index_edu.Rdata"))))[,c("LopNr", "ISCED97.Far", "ISCED97.Mor", "ISCED97.Parent", "EduYears.Far", "EduYears.Mor", "EduYears.Parent")]
nrow(index_edu)  # 2,893,654

demo <- data.frame(get(load(paste0(r_dir, "Demographic.Rdata"))))
demo <- demo[,c("LopNr", "LopNrFar", "LopNrMor", "FodelseAr", "Kon", "ISCED97", "EduYears", "income_Age2535_max", "income_Age2535_mean", "income_Age5060_max", "income_Age5060_mean")]
nrow(demo)   # 9,723,423

ry_first_index <- data.frame(get(load(paste0(r_dir, "ry_first_indexW_COMPLETE.Rdata"))))[,c("LopNr","ENDPOINT")]
nrow(ry_first_index)   # 14747135 


## add income of parents 
demo_index <- demo[demo$LopNr %in% lrs_all$LopNr,]
nrow(demo_index)  # 2,555,541

demo_mother <- merge(demo_index[,c("LopNr","LopNrMor")], demo[,c("LopNr","FodelseAr","Kon","income_Age5060_max","income_Age5060_mean")], by.x="LopNrMor", by.y="LopNr", all.x=T)
nrow(demo_mother)  # 2,555,541
demo_mother <- demo_mother[,c("LopNr","LopNrMor", "FodelseAr", "Kon", "income_Age5060_max","income_Age5060_mean")]
colnames(demo_mother) <- c("LopNr", "LopNrMor", "FodelseAr.Mor", "Kon.Mor", "income_Age5060_max.Mor","income_Age5060_mean.Mor")

demo_father <- merge(demo_index[,c("LopNr","LopNrFar")], demo[,c("LopNr","FodelseAr","Kon","income_Age5060_max","income_Age5060_mean")], by.x="LopNrFar", by.y="LopNr", all.x=T)
nrow(demo_father)  # 2,555,541
demo_father <- demo_father[,c("LopNr", "LopNrFar", "FodelseAr", "Kon", "income_Age5060_max","income_Age5060_mean")]
colnames(demo_father) <- c("LopNr", "LopNrFar", "FodelseAr.Far", "Kon.Far", "income_Age5060_max.Far","income_Age5060_mean.Far")

demo_parents <- merge(demo_mother, demo_father, by="LopNr", all.x=T, all.y=T)
nrow(demo_parents)  # 2,555,541
demo_parents[,"income_Age5060_max.Parent"] <- apply(demo_parents[,c("income_Age5060_max.Far","income_Age5060_max.Mor")], 1, max, na.rm=T)
demo_parents[,"income_Age5060_max.Parent"] <- ifelse(demo_parents[,"income_Age5060_max.Parent"]=="-Inf", NA, demo_parents[,"income_Age5060_max.Parent"])
demo_parents[,"income_Age5060_mean.Parent"] <- apply(demo_parents[,c("income_Age5060_mean.Far","income_Age5060_mean.Mor")], 1, max, na.rm=T)
demo_parents[,"income_Age5060_mean.Parent"] <- ifelse(demo_parents[,"income_Age5060_mean.Parent"]=="-Inf", NA, demo_parents[,"income_Age5060_mean.Parent"])


## Combine
covar_parents <- merge(index_edu, demo_parents, by="LopNr")
nrow(covar_parents)  # 2,555,541

covar <- merge(demo[,c("LopNr","ISCED97","EduYears","income_Age2535_max","income_Age2535_mean","income_Age5060_max","income_Age5060_mean")], covar_parents, by="LopNr")
nrow(covar)  # 2,555,541
phe <- merge(lrs_all, covar)
nrow(phe)  # 2,555,541
phe <- phe[, c("LopNr", "FodelseAr", "Kon", "n_child", "n_child_Age4550" ,"n_gchild", "childless", "childless_Age4550", "infertility", "lamda", "afc","afc_Age4550", "alc", 
               "ISCED97","EduYears","income_Age2535_max","income_Age2535_mean","income_Age5060_max","income_Age5060_mean",
               "LopNrFar", "Kon.Far", "FodelseAr.Far", "ISCED97.Far", "EduYears.Far", "income_Age5060_max.Far", "income_Age5060_mean.Far",             
               "LopNrMor", "Kon.Mor", "FodelseAr.Mor", "ISCED97.Mor", "EduYears.Mor", "income_Age5060_max.Mor", "income_Age5060_mean.Mor",                    
               "ISCED97.Parent", "EduYears.Parent", "income_Age5060_max.Parent", "income_Age5060_mean.Parent")]                          
phe$EduYears <- as.numeric(as.character(phe$EduYears))
phe$EduYears.Far <- as.numeric(as.character(phe$EduYears.Far))
phe$EduYears.Mor <- as.numeric(as.character(phe$EduYears.Mor))

## Correlations between variables
# between individual himself
cor(phe$income_Age2535_mean, phe$income_Age2535_max, method="pearson", use="complete.obs")  # 0.806548
cor(phe$income_Age5060_mean, phe$income_Age5060_max, method="pearson", use="complete.obs")  # 0.8230433
cor(phe$income_Age2535_max, phe$income_Age5060_max, method="pearson", use="complete.obs")   # 0.1353279
cor(phe$income_Age2535_mean, phe$income_Age5060_mean, method="pearson", use="complete.obs") # 0.2637808

# 
cor(phe$EduYears, phe$income_Age2535_mean, method="pearson", use="complete.obs")  # 0.07095664
cor(phe$EduYears, phe$income_Age2535_max, method="pearson", use="complete.obs")   # 0.03995085
 
cor(phe$EduYears, phe$income_Age5060_mean, method="pearson", use="complete.obs")  # 0.07095664
cor(phe$EduYears, phe$income_Age5060_max, method="pearson", use="complete.obs")   # 0.03995085

#
cor(phe$EduYears.Far, phe$EduYears.Mor, method="pearson", use="complete.obs")  # 0.398446
cor(phe$income_Age5060_max.Far, phe$income_Age5060_max.Mor, method="pearson", use="complete.obs")  # 0.07217279
cor(phe$income_Age5060_mean.Far, phe$income_Age5060_mean.Mor, method="pearson", use="complete.obs")  # 0.08050527
 

## Extract specific endpoints                        
ep_f <- data.frame(table(ry_first_index$ENDPOINT))
colnames(ep_f) <- c("ENDPOINT","count")
ep_f$ENDPOINT <- as.character(ep_f$ENDPOINT)
ep_f <- ep_f[order(-ep_f$count),]
nrow(ep_f)      # 1,959


endpoints <- c("F5_SCHZPHR","F5_BIPO","F5_DEPRESSIO","F5_AUTISM","F5_ANOREX","F5_ALCOHOL",
               "J10_ASTHMA","J10_PNEUMONAS","G6_SLEEPAPNO","E4_DM1","E4_DM2",
               "N14_MALEINFERT","N14_FIOTHNAS","N14_FITUB","M13_ARTHROSIS_KNEE")  

# endpoints <- unique(c("F5_SCHZPHR","F5_BIPO","F5_DEPRESSIO","F5_AUTISM","F5_ANOREX","F5_ALCOHOL",
               "J10_ASTHMA","J10_PNEUMONAS","G6_SLEEPAPNO","E4_DM1","E4_DM2",
               "N14_MALEINFERT","N14_FIOTHNAS","N14_FITUB","M13_ARTHROSIS_KNEE",colnames(index_lrs_edu)[25:325]))

# endpoints <- colnames(index_lrs_edu)[25:325]


for (ep_n in 1:length(endpoints)){
 	id_i <- ry_first_index[ry_first_index$ENDPOINT==endpoints[ep_n],"LopNr"]
 	phe[ ,endpoints[ep_n]] <- as.integer(phe$LopNr %in% id_i)
 	print(paste0(i,": ",endpoints[i]))	
}
 
save(phe, file=paste0(r_dir, "Phe_IndexW_EndpointSelected.Rdata"))
rm(lrs_all, index_edu, demo, demo_father, demo_mother, demo_index, covar, covar_parents, demo_parents, ry_first_index)





###########################################
#  Population-based Regression analysis   #
###########################################

phe <- within(phe, {
	LopNr <- factor(LopNr)
	FodelseAr <- factor(FodelseAr) 
	n_child <- as.numeric(n_child)
	n_child_Age4550 <- as.numeric(n_child_Age4550)
	n_gchild <- as.numeric(n_gchild)
	childless <- as.numeric(childless)
	childless_Age4550 <- as.numeric(childless_Age4550)
	afc <- as.numeric(afc)
	afc_Age4550 <- as.numeric(afc_Age4550)
	
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
})


sink(file="Pop_IndexW_EndpointSelected.output", append=F, type="output", split=T)
sexs <- c("male","female")
NEB <- c("n_child","n_child_Age4550")
models <- c("no","edu","eduParent","inc","edu_inc")	
res <- NULL

for (ep_n in 1:length(endpoints)){		
	for (sex_n in 1:2){
		for (i in 1:length(NEB)){
			p <- phe[phe$Kon==sex_n & is.na(NEB[i])==F, ]
			p[,"Endpoint"] <- as.numeric(as.character(p[,endpoints[ep_n]]))	
					
			for (mod_n in 1:length(models)){
				print(" ")
				print("----------------------------------------------------------------------------------------")
				print(paste0(endpoints[ep_n], ", ", sexs[sex_n], ", ", NEB[i], ", ", models[mod_n]))	
		
				if (models[mod_n]=="no"){
					# no correction for education and income
					# m_1 <- glm(p[,NEB[i]] ~ age + age2 + Endpoint, data=p, family="poisson")
					
					m <- glm(p[,NEB[i]] ~ age + age2 + Endpoint, data=p, family="poisson") 
					df <- as.data.frame(t(as.data.frame(summary(m)$coeff["Endpoint",])))	
					rownames(df) <- "Endpoint"			
					df$N_Cases <- sum(p[,"Endpoint"]==1 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]))
					df$N_Controls <- sum(p[,"Endpoint"]==0 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]))	
					df$Prevalence <- df$N_Cases/(df$N_Cases+df$N_Controls)
					df$LRS_Cases <- mean(p[p[,"Endpoint"]==1 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]), NEB[i]])
					df$LRS_Controls <- mean(p[p[,"Endpoint"]==0 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]), NEB[i]])
					
				} else if (models[mod_n]=="edu"){
					# correct for education
					# m_2_1 <- glm(p[,NEB[i]] ~ age + age2 + Endpoint + ISCED97, data=p, family="poisson") 
					# m_2_2 <- glm(p[,NEB[i]] ~ age + age2 + Endpoint + EduYears, data=p, family="poisson") 
					# m_2_3 <- glm(p[,NEB[i]] ~ age + age2 + Endpoint + EduYears + EduYears2, data=p, family="poisson") 				
					
					m <- glm(p[,NEB[i]] ~ age + age2 + Endpoint + EduYears, data=p, family="poisson") 	
					df <- as.data.frame(t(as.data.frame(summary(m)$coeff["Endpoint",])))	
					rownames(df) <- "Endpoint"			
					df$N_Cases <- sum(p[,"Endpoint"]==1 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]) & !is.na(p[,"EduYears"]))
					df$N_Controls <- sum(p[,"Endpoint"]==0 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]) & !is.na(p[,"EduYears"]))	
					df$Prevalence <- df$N_Cases/(df$N_Cases+df$N_Controls)
					df$LRS_Cases <- mean(p[p[,"Endpoint"]==1 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]) & !is.na(p[,"EduYears"]), NEB[i]])
					df$LRS_Controls <- mean(p[p[,"Endpoint"]==0 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]) & !is.na(p[,"EduYears"]), NEB[i]])

				} else if (models[mod_n]=="eduParent"){
					# m_2_4 <- glm(p[,NEB[i]] ~ age + age2 + Endpoint + ISCED97.Parent, data=p, family="poisson") 		
					# m_2_5 <- glm(p[,NEB[i]] ~ age + age2 + Endpoint + EduYears.Parent, data=p, family="poisson") 
					# m_2_6 <- glm(p[,NEB[i]] ~ age + age2 + Endpoint + EduYears.Parent + EduYears2.Parent, data=p, family="poisson") 
					# m_2_7 <- glm(p[,NEB[i]] ~ age + age2 + Endpoint + ISCED97.Far + ISCED97.Mor, data=p, family="poisson") 		
					# m_2_8 <- glm(p[,NEB[i]] ~ age + age2 + Endpoint + EduYears.Far + EduYears.Mor, data=p, family="poisson") 
					# m_2_9 <- glm(p[,NEB[i]] ~ age + age2 + Endpoint + EduYears.Far + EduYears2.Far + EduYears.Mor + EduYears2.Mor, data=p, family="poisson") 
					
					m <- glm(p[,NEB[i]] ~ age + age2 + Endpoint + EduYears.Parent, data=p, family="poisson")
					df <- as.data.frame(t(as.data.frame(summary(m)$coeff["Endpoint",])))	
					rownames(df) <- "Endpoint"			
					df$N_Cases <- sum(p[,"Endpoint"]==1 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]) & !is.na(p[,"EduYears.Parent"]))
					df$N_Controls <- sum(p[,"Endpoint"]==0 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]) & !is.na(p[,"EduYears.Parent"]))	
					df$Prevalence <- df$N_Cases/(df$N_Cases+df$N_Controls)
					df$LRS_Cases <- mean(p[p[,"Endpoint"]==1 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]) & !is.na(p[,"EduYears.Parent"]), NEB[i]])
					df$LRS_Controls <- mean(p[p[,"Endpoint"]==0 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]) & !is.na(p[,"EduYears.Parent"]), NEB[i]])

				} else if (models[mod_n]=="inc"){
					# correction for income				
					# m_3_1 <- glm(p[,NEB[i]] ~ age + age2 + Endpoint + income_Age2535_mean, data=p, family="poisson") 
					# m_3_2 <- glm(p[,NEB[i]] ~ age + age2 + Endpoint + income_Age2535_max, data=p, family="poisson")
					 
					m <- glm(p[,NEB[i]] ~ age + age2 + Endpoint + income_Age2535_mean, data=p, family="poisson") 
					df <- as.data.frame(t(as.data.frame(summary(m)$coeff["Endpoint",])))	
					rownames(df) <- "Endpoint"			
					df$N_Cases <- sum(p[,"Endpoint"]==1 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]) & !is.na(p[,"income_Age2535_mean"]))
					df$N_Controls <- sum(p[,"Endpoint"]==0 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]) & !is.na(p[,"income_Age2535_mean"]))	
					df$Prevalence <- df$N_Cases/(df$N_Cases+df$N_Controls)
					df$LRS_Cases <- mean(p[p[,"Endpoint"]==1 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]) & !is.na(p[,"income_Age2535_mean"]), NEB[i]])
					df$LRS_Controls <- mean(p[p[,"Endpoint"]==0 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]) & !is.na(p[,"income_Age2535_mean"]), NEB[i]])

				} else if (models[mod_n]=="edu_inc"){
					# correction for both education and income
					# m_4_1 <- glm(p[,NEB[i]] ~ age + age2 + Endpoint + EduYears + income_Age2535_mean, data=p, family="poisson") 
					# m_4_2 <- glm(p[,NEB[i]] ~ age + age2 + Endpoint + EduYears + EduYears^2 + income_Age2535_mean + income_Age2535_mean^2, data=p, family="poisson") 
					
					m <- glm(p[,NEB[i]] ~ age + age2 + Endpoint + EduYears + income_Age2535_mean, data=p, family="poisson") 
					df <- as.data.frame(t(as.data.frame(summary(m)$coeff["Endpoint",])))	
					rownames(df) <- "Endpoint"			
					df$N_Cases <- sum(p[,"Endpoint"]==1 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]) & !is.na(p[,"EduYears"]) & !is.na(p[,"income_Age2535_mean"]))
					df$N_Controls <- sum(p[,"Endpoint"]==0 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]) & !is.na(p[,"EduYears"]) & !is.na(p[,"income_Age2535_mean"]))	
					df$Prevalence <- df$N_Cases/(df$N_Cases+df$N_Controls)
					df$LRS_Cases <- mean(p[p[,"Endpoint"]==1 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]) & !is.na(p[,"EduYears"]) & !is.na(p[,"income_Age2535_mean"]), NEB[i]])
					df$LRS_Controls <- mean(p[p[,"Endpoint"]==0 & !is.na(p[,"age"]) & !is.na(p[,NEB[i]]) & !is.na(p[,"EduYears"]) & !is.na(p[,"income_Age2535_mean"]), NEB[i]])
				}

				df$model <- models[mod_n]
				df$sex <- sexs[sex_n]  # this was wrong in previous running, since mode_n is used
				df$LRS <- NEB[i]
				df$Endpoint <- endpoints[ep_n]
				
				print(df)
				write.table(df, "Reg_Pop_IndexW_EndpointSelected_20200925.tsv", append=T, quote=F, sep=" ", row.names=F, col.names=T)

				res <- rbind(res, df)
			}				
		}
	}
}
write.table(res, "Reg_Pop_IndexW_EndpointSelected.tsv", append=F, quote=F, sep=" ", row.names=F, col.names=T)

sink()




###########################################
#      Plot the results (interactive)     #
###########################################

system("less Population_based_regression_endpoint_1_1000.tsv|head -n 1|awk '{print $1,$2,$3,$5,$6,$7,$8,$9,$11}' ")
system("less Population_based_regression_endpoint_1_1000.tsv|grep ENDPOINT|awk '{print $1,$2,$3,$5,$6,$7,$8,$9,$11}' > pop_reg_results.txt")


res <- read.table("pop_reg_results.txt",header=F)
colnames(res) <- c("Endpoint","sex","Prevalence","N.case","Avg.NEB.control","Avg.NEB.case","Variable","Estimate","Estimate.Pval")

res <- res[order(res$Endpoint),]
res$rank <- as.numeric(as.factor(res$Endpoint))
res$group <- substr(res$Endpoint,1,1)
hlines <- aggregate(rank ~ group, res, max)[,"rank"] + 0.5

p <- ggplot(data = res, aes(x = rank, y = Estimate, color = sex,
                            text = paste("Endpoint: ", Endpoint, "\n",
                                         "sex: ", sex, "\n",                            
                                         "Prevalence: ", Prevalence, "\n",
                                         "N.case: ", N.case, "\n",    
                                         "Avg.NEB.control: ", Avg.NEB.control, "\n",  
                                         "Avg.NEB.case: ", Avg.NEB.control, "\n", 
                                         "Estimate: ", Estimate, "\n",
                                         "Estimate.Pval", Estimate.Pval, "\n", sep = ""))) +
      geom_point(alpha=1, shape=1) + 
      #labs(x = "FinnGen samples (born 1956-1982)", y = "Index person (born 1956-1982)", title="Prevalence (%) of endpoint") +
      geom_hline(yintercept = 0, linetype="dotted", color = "grey", size=0.6) +
      geom_vline(xintercept = hlines, linetype="dotted", color = "grey", size=0.6) + 
     # xlim(0, 40) + ylim(0, 40) + 
      theme_classic()

ggplotly(p, tooltip="text")




