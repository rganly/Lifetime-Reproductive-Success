
## Aim: conditional logitic regression model for childless in sibling pairs disconcordant on outcomes, using disease diagnoses before age 45/50

sex_n <- 1
sexs <- c("male","female")

outcomeName <- "childless"  

mod_pattern <- "AFB4550"    # 1_case : 1_control regarding disease outcome


df_NA <- matrix(NA, ncol=18, nrow=1)
colnames(df_NA) <- c("Estimate","HR","SE","Z","P_val","HR_025","HR_975","N_Family","N_Sample","N_Record","N_00","N_11","N_01","N_10","model","sex","LRS","Endpoint")


# work directory and function
setwd("/homes/aliu/DSGE_LRS/output/registry_edit/REGRESSION/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"

library(data.table)
library(dplyr)
options(tibble.width = Inf)
library(survival)

'%!in%' <- function(x,y)!('%in%'(x,y))




#########################################################
#       Read in data and extract for a specific sex     #
#########################################################

## Demographic info, the same for all analyses, born from 1956 to 1982
indexW <- data.frame(get(load(paste0(r_dir, "indexW_fullsib.Rdata")))) %>% filter(index_sex==sex_n) %>% 
      select(index_id, index_sex, parent_id, index_birth_date, endfollowup4550_date, endfollowup4550_age)
nrow(indexW)  # 443,695 for male


outcome_age_dat <- data.frame(get(load(paste0(r_dir, "indexW_fullsib_",outcomeName,".Rdata")))) %>% filter(index_id %in% indexW$index_id) %>%
	                     mutate(outcome_age=outcome_age-1)   # change from age at giving birth to age at pregnancy
nrow(outcome_age_dat)  # 317,772


## Date at diagnosis
ry_first_index <- data.frame(get(load(paste0(r_dir,"indexW_fullsib_endpoint.Rdata")))) %>% filter(ID %in% indexW$index_id) 
nrow(ry_first_index)  # 2,086,640


## Disease endpoint
disease30_lst <- ry_first_index %>% mutate(ENDPOINT=as.character(ENDPOINT)) %>% group_by(ENDPOINT) %>% count() %>% filter(n>=30)  # 1,253
if (file.exists(paste0("logit_",mod_pattern, "_", outcomeName ,"_", sexs[sex_n], "_30.tsv"))){
	done <- read.table(paste0("logit_",mod_pattern, "_", outcomeName ,"_", sexs[sex_n], "_50.tsv"), header=T)
	disease30_lst <- disease30_lst %>% filter(ENDPOINT %!in% done$Endpoint)
}




###################################################################################
#        stop/status for outcome events only (ignore disease exposure)            #
###################################################################################

outcome_window <- indexW %>% left_join(outcome_age_dat,by="index_id") %>% 
      mutate(stop=ifelse(is.na(outcome_age), endfollowup4550_age, outcome_age), status=ifelse(is.na(outcome_age), 0, 1)) %>% 
      select(index_id, index_sex, index_birth_date, endfollowup4550_age, parent_id, stop, status) %>% 
      filter(stop!=16)   # remove children pregnant/born at/before the enter age (16)



for (disease in disease30_lst$ENDPOINT ) {
	# disease <- "F5_SCHIZPER"
	# disease <- "N14_MALEINFERT"
	print(paste0(sexs[sex_n],": ",which(disease==disease30_lst$ENDPOINT),": ", disease))



	###########################################################################
	#             Add time-varing disease exposure variable                   #
	###########################################################################

	## Select disease endpoint of interest and merge with outcome_window
	outcome_window_endpoint <- ry_first_index %>% mutate(index_id=as.numeric(as.character(ID))) %>% filter(ENDPOINT==disease) %>% select(index_id, EVENT_F_DATE) %>% 
	      right_join(outcome_window, by="index_id") %>% 
	      mutate(age_onset=round(as.numeric((as.Date(as.character(EVENT_F_DATE),"%Y%m%d") - as.Date(as.character(index_birth_date),"%Y%m%d"))/365.25),3)) %>% 
	      select(-EVENT_F_DATE)




	###########################################################################
	#         Sibling-pairs disconcordant on both disease and outcome         #
	###########################################################################

	## Keep only families having full-siblings discordant on outcome status, that is at least one sibling with children and one being childless; 
	parent_outcome1 <- outcome_window_endpoint %>% filter(status==1) %>% select(parent_id) %>% unique()  # 177,327
	parent_outcome0 <- outcome_window_endpoint %>% filter(status==0) %>% select(parent_id) %>% unique()  # 97,080
	p <- outcome_window_endpoint %>% filter(parent_id %in% parent_outcome0$parent_id) %>% filter(parent_id %in% parent_outcome1$parent_id)
	dim(p)  # 188911      9


	## Within each family, randomly select one sibling with children as control, and the childless siblings with closest birth year with the control as case;
	p_outcome1 <- p %>% filter(status==1) %>% group_by(parent_id) %>% sample_n(1) %>% ungroup()   # 79,122
	p_outcome0 <- p %>% filter(status==0) %>% inner_join(p_outcome1[,c("parent_id","index_birth_date","stop")], by="parent_id") %>% mutate(dif=abs(index_birth_date.y-index_birth_date.x)) %>% 
	                   group_by(parent_id) %>% filter(dif==min(dif)) %>% sample_n(1) %>% 
	                   select(-index_birth_date.y, -dif) %>% rename(index_birth_date="index_birth_date.x", stop="stop.x") %>% ungroup()
	
	dim(p_outcome0)  # 379644     10
	dim(p_outcome1)  # 379279     10

	## All diagnose before stop (45/50)
	if (mod_pattern=="AFB4550"){
		p_outcome0 <- p_outcome0 %>% select(-stop.y)
	}

	pp <- rbind(p_outcome0, p_outcome1) %>% mutate(disease=ifelse(is.na(age_onset),0,1)) %>% mutate(age=2018-as.numeric(substr(as.character(index_birth_date),1,4)), age2=age^2)
	pp_dif <- intersect(pp[pp$disease==0,"parent_id"],pp[pp$disease==1,"parent_id"])
	ppp <- pp %>% filter(parent_id %in% pp_dif$parent_id)   # also disconcordant on disease status, for reporting N (e.g N_Record) which reflect the sample size contribute to the estimate od disease status.




	###########################################################################
	#  Survival analysis considering disease status as time-varing covariate  #
	###########################################################################

	# if (nrow(unique(ppp[,c("index_id")]))>=30 & disease %!in% c("M13_PANNICULITISNAS")){   # logit_AFB4550_childless_male_50
	if (nrow(unique(ppp[,c("index_id")]))>=30){
		ppp$parent_id <- factor(ppp$parent_id)
		m_mod_sib <- clogit(status ~ disease + age + age2 + strata(parent_id), data=pp)  # use the dataset disconcordant on outcome
		df <- as.data.frame(t(as.data.frame(summary(m_mod_sib)$coeff["disease",])))
		rownames(df) <- "Endpoint"
		df <- df[,c("coef", "exp(coef)", "se(coef)", "z","Pr(>|z|)")]
		colnames(df) <- c("Estimate", "HR", "SE","Z", "P_val")
		df$HR <- summary(m_mod_sib)$conf.int["disease","exp(coef)"]
		df$HR_025 <- summary(m_mod_sib)$conf.int["disease","lower .95"]
		df$HR_975 <- summary(m_mod_sib)$conf.int["disease","upper .95"]
	} else {
		df <- df_NA
	}
	
	df[1,"N_Family"] <- nrow(unique(ppp[,c("parent_id")]))  # 2*2 table for large SE results
	df[1,"N_Sample"] <- nrow(unique(ppp[,c("index_id")]))   
	df[1,"N_Record"] <- nrow(ppp)
	df[1,"N_00"] <- ppp %>% filter(disease==0 & status==0) %>% nrow()
	df[1,"N_11"] <- ppp %>% filter(disease==1 & status==1) %>% nrow()
	df[1,"N_01"] <- ppp %>% filter(disease==0 & status==1) %>% nrow()
	df[1,"N_10"] <- ppp %>% filter(disease==1 & status==0) %>% nrow()
	df[1,"model"] <- paste0("logit_",mod_pattern)
	df[1,"sex"] <- sexs[sex_n] 
	df[1,"LRS"] <- outcomeName
	df[1,"Endpoint"] <- disease

	print(df)
	if (!file.exists(paste0("logit_",mod_pattern, "_", outcomeName ,"_", sexs[sex_n], "_30.tsv"))){
		write.table(df, paste0("logit_",mod_pattern, "_", outcomeName ,"_", sexs[sex_n], "_30.tsv"), append=F, quote=F, sep="\t", row.names=F, col.names=T)
	} else {
		write.table(df, paste0("logit_",mod_pattern, "_", outcomeName ,"_", sexs[sex_n], "_30.tsv"), append=T, quote=F, sep="\t", row.names=F, col.names=F)
	}

}






