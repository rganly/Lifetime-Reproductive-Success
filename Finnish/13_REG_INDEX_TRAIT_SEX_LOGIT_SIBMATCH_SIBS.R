
## conditional logitic regression model for childless using sibling-match design

# To avoid the confounding from reverse causation and the higher prevalence of diseases in older people, 
#    we use a matched pair case-control study design within full-siblings and then apply conditional logistic regression model or cox HR model stratified by full-sib families. 

# The sibling matching processes could be: 
# 1) remove individuals with the outcome event prior to the disease exposure. 
#    By removing individuals with the outcome event prior to the disease exposure, we can avoid assigning potential affected individuals to be unaffected; 
# 2) Keep only families having full-siblings discordant on outcome status, that is at least one sibling with children and one being childless; 
# 3) Within each family, randomly select one sibling with children as control, and the childless siblings with closest birth year with the control as case; 
# 4) If the case and the control are also discordant on disease status at the time point when the event occurs to the case that is the age at first birth for case for childless, 
#    then we enroll this full-sibling pair into our matched pair case-control study;



#########################################################
#           Input variables for each scenario    ´      #
#########################################################

######## Only change this part when running for different scenarios ######## 
# which sex?
# sex_n <- 1  # male
sex_n <- 2    # female
sexs <- c("male","female")

# which outcome?
outcomeName <- "childless" 

# which diagnose to use?
mod_pattern <- "sibmatch"

# which sibs to include? Here, we compare four strategies to select siblings from each family.
# sib_pattern <- "disBoth"
# sib_pattern <- "disOutcome"
# sib_pattern <- "withchildAll"
sib_pattern <- "withchildClosest"

#############################################################################



#########################################################
#           Set working environment                     #
#########################################################

# for disease endpoint with two few cases
df_NA <- matrix(NA, ncol=21, nrow=1)
colnames(df_NA) <- c("Estimate","HR","SE","Z","P_val","HR_025","HR_975","N_Family","N_Sample","N_Record","N_00","N_11","N_01","N_10","N_MissClass10","N_MissClass11","model","sibs","sex","LRS","Endpoint")


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

## Demographic info for index person, the same for all analyses
indexW <- data.frame(get(load(paste0(r_dir, "indexW_fullsib.Rdata")))) %>% filter(index_sex==sex_n) %>% 
      select(index_id, index_sex, parent_id, index_birth_date, endfollowup4550_date, endfollowup4550_age)
nrow(indexW)  # 443,695 for male


## fertility outcome
outcome_age_dat <- data.frame(get(load(paste0(r_dir, "indexW_fullsib_",outcomeName,".Rdata")))) %>% filter(index_id %in% indexW$index_id) %>%
	                     mutate(outcome_age=outcome_age-1)   # change from age at giving birth to age at pregnancy
nrow(outcome_age_dat)  # 317,772


## Date at diagnosis
ry_first_index <- data.frame(get(load(paste0(r_dir,"indexW_fullsib_endpoint.Rdata")))) %>% filter(ID %in% indexW$index_id) 
nrow(ry_first_index)  # 2,086,640


## Disease endpoint list (skip endpoints already done)
disease30_lst <- ry_first_index %>% mutate(ENDPOINT=as.character(ENDPOINT)) %>% group_by(ENDPOINT) %>% count() %>% filter(n>=30)  # 1,253
if (file.exists(paste0("logit_",mod_pattern, "_", outcomeName ,"_", sexs[sex_n], "_SIB", sib_pattern, "_30.tsv"))){
	done <- read.table(paste0("logit_",mod_pattern, "_", outcomeName ,"_", sexs[sex_n], "_SIB", sib_pattern, "_30.tsv"), header=T)
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
	# disease <- "F5_SCHZPHR"
	# disease <- "N14_MALEINFERT"
	print(paste0(sexs[sex_n],": ",which(disease==disease30_lst$ENDPOINT),": ", disease))



	###########################################################################
	#               Add age onset for a specific disease                      #
	###########################################################################

	## Select disease endpoint of interest and merge with outcome_window
	outcome_window_endpoint <- ry_first_index %>% mutate(index_id=as.numeric(as.character(ID))) %>% filter(ENDPOINT==disease) %>% select(index_id, EVENT_F_DATE) %>% 
	      right_join(outcome_window, by="index_id") %>% 
	      mutate(age_onset=round(as.numeric((as.Date(as.character(EVENT_F_DATE),"%Y%m%d") - as.Date(as.character(index_birth_date),"%Y%m%d"))/365.25),3)) %>% 
	      select(-EVENT_F_DATE)


	###################################################################################
	#    Using all siblings from a family including at least one sib with children    #
	###################################################################################

	if (sib_pattern=="withchildAll"){
		## Keep only families having at least one sibling with children, and extract min(age at first birth) among sibs with children as stop.y;
		pp <- outcome_window_endpoint %>% filter(status==1) %>% select(parent_id) %>% unique() %>% inner_join(outcome_window_endpoint, by="parent_id")
		stop_y <- pp %>% filter(status==1) %>% group_by(parent_id) %>% mutate(stop.y=min(stop)) %>% select(parent_id, stop.y) %>% ungroup() %>% unique()
		pp <- pp %>% inner_join(stop_y, by="parent_id") %>% mutate(disease4550=ifelse(is.na(age_onset),0,1))

		## observation window for disease are the same for all siblings; for stop time, use the actual stop time for sib with children and the stop.y (age at first birth for sib with children) for childless individual; 
		if (mod_pattern=="sibmatch"){
			ppp <- pp %>% mutate(age_onset=ifelse(age_onset>stop.y, NA, age_onset), stop=ifelse(status==0 & stop>stop.y, stop.y, stop)) %>% select(-stop.y) %>% 
			              mutate(disease=ifelse(is.na(age_onset),0,1)) %>% mutate(age=2018-as.numeric(substr(as.character(index_birth_date),1,4)), age2=age^2)
		}   # otherwise all diagnose before stop
    
    		## Count how many individuals with diagnose after stop were missclassfied as unsick due to the trunction from using sib-match design    
		N_MissClass10 <- ppp %>% mutate(miss=ifelse(is.na(age_onset) & disease4550==1, 1, 0 )) %>% filter(miss==1 & status==0) %>% nrow()  # for people with children 
		N_MissClass11 <- ppp %>% mutate(miss=ifelse(is.na(age_onset) & disease4550==1, 1, 0 )) %>% filter(miss==1 & status==1) %>% nrow()  # for childless people
		
	}
	
	###################################################################################
	#   closest sibling-pair from a family including at least one sib with children   #
	###################################################################################

	if (sib_pattern=="withchildClosest"){
		## Keep only families having at least one sibling with children;
		parent_outcome1 <- outcome_window_endpoint %>% filter(status==1) %>% select(parent_id) %>% unique()  # 177,327
		p <- outcome_window_endpoint %>% filter(parent_id %in% parent_outcome1$parent_id)
		dim(p)  # 405,857      9


		## Within each family, randomly select one sibling with children as control, and one sibling with closest birth year no matter the outcome status;
		p_outcome1 <- p %>% filter(status==1) %>% group_by(parent_id) %>% sample_n(1) %>% ungroup() %>% mutate(disease4550=ifelse(is.na(age_onset),0,1)) # 177,330		
		p_outcome0 <- p %>% filter(index_id %!in% p_outcome1$index_id) %>% inner_join(p_outcome1[,c("parent_id","index_birth_date")], by="parent_id") %>% mutate(dif=abs(index_birth_date.y-index_birth_date.x)) %>% 
		                   group_by(parent_id) %>% filter(dif==min(dif)) %>% 
		                   sample_n(1) %>% 
		                   select(-index_birth_date.y, -dif) %>% rename(index_birth_date="index_birth_date.x") %>% ungroup() %>% 
		                   mutate(disease4550=ifelse(is.na(age_onset),0,1))

		p_outcome1 <- p_outcome1 %>% filter(parent_id %in% p_outcome0$parent_id)
		dim(p_outcome0)  # 177220     10
		dim(p_outcome1)  # 177220     10
		
		pp <- rbind(p_outcome0, p_outcome1)
		stop_y <- pp %>% filter(status==1) %>% group_by(parent_id) %>% mutate(stop.y=min(stop)) %>% select(parent_id, stop.y) %>% ungroup() %>% unique()
		pp <- pp %>% inner_join(stop_y, by="parent_id")

		## observation window for disease are the same for all siblings; for stop time, use the actual stop time for sib with children and the stop.y for childless individual; 
		if (mod_pattern=="sibmatch"){
			ppp <- pp %>% mutate(age_onset=ifelse(age_onset>stop.y, NA, age_onset), stop=ifelse(status==0 & stop>stop.y, stop.y, stop)) %>% select(-stop.y) %>% 
			              mutate(disease=ifelse(is.na(age_onset),0,1)) %>% mutate(age=2018-as.numeric(substr(as.character(index_birth_date),1,4)), age2=age^2)
		}   # otherwise all diagnose before stop

	  	## Count how many individuals with diagnose after stop were missclassfied as unsick due to the trunction from using sib-match design    		
		N_MissClass10 <- ppp %>% mutate(miss=ifelse(is.na(age_onset) & disease4550==1, 1, 0 )) %>% filter(miss==1 & status==0) %>% nrow()  # for people with children
		N_MissClass11 <- ppp %>% mutate(miss=ifelse(is.na(age_onset) & disease4550==1, 1, 0 )) %>% filter(miss==1 & status==1) %>% nrow()  # for childless people
	}
	
	
	
	###########################################################################
	#         Sibling-pairs disconcordant on outcome                          #
	###########################################################################

	if (sib_pattern=="disOutcome") {
		## Keep only families having full-siblings discordant on outcome status, that is at least one sibling with children and one being childless; 
		parent_outcome1 <- outcome_window_endpoint %>% filter(status==1) %>% select(parent_id) %>% unique()  # 177,327
		parent_outcome0 <- outcome_window_endpoint %>% filter(status==0) %>% select(parent_id) %>% unique()  # 97,080
		p <- outcome_window_endpoint %>% filter(parent_id %in% parent_outcome0$parent_id) %>% filter(parent_id %in% parent_outcome1$parent_id)
		dim(p)  # 188911      9


		## Within each family, randomly select one sibling with children as control, and the childless siblings with closest birth year with the control as case;
		p_outcome1 <- p %>% filter(status==1) %>% group_by(parent_id) %>% sample_n(1) %>% ungroup() %>% mutate(disease4550=ifelse(is.na(age_onset),0,1)) # 79,122
		p_outcome0 <- p %>% filter(status==0) %>% inner_join(p_outcome1[,c("parent_id","index_birth_date","stop")], by="parent_id") %>% mutate(dif=abs(index_birth_date.y-index_birth_date.x)) %>% 
		                   group_by(parent_id) %>% filter(dif==min(dif)) %>% 
		                   sample_n(1) %>% 
		                   select(-index_birth_date.y, -dif) %>% rename(index_birth_date="index_birth_date.x", stop="stop.x") %>% ungroup() %>% 
		                   mutate(disease4550=ifelse(is.na(age_onset),0,1))

		p_outcome1 <- p_outcome1 %>% filter(parent_id %in% p_outcome0$parent_id)
		dim(p_outcome0)  # 379644     10
		dim(p_outcome1)  # 379279     10
	
		## If the case and the control are also discordant on disease status at the time point when the event occurs to the case that is the age at first birth for case for childless, then we enroll this full-sibling pair into our matched pair case-control study; 		
		if (mod_pattern=="sibmatch"){
			### p_outcome0 <- p_outcome0 %>% mutate(age_onset=ifelse(age_onset>stop.y, NA, age_onset)) %>% select(-stop.y)
			p_outcome1 <- p_outcome1 %>% mutate(age_onset=ifelse(age_onset>stop, NA, age_onset))
			p_outcome0 <- p_outcome0 %>% mutate(age_onset=ifelse(age_onset>stop.y, NA, age_onset), stop=ifelse(stop>stop.y, stop.y, stop)) %>% select(-stop.y)
		}   # otherwise all diagnose before stop
			
		
		ppp <- rbind(p_outcome0, p_outcome1) %>% mutate(disease=ifelse(is.na(age_onset),0,1)) %>% 
		            mutate(age=2018-as.numeric(substr(as.character(index_birth_date),1,4)), age2=age^2)
		
		## Count how many individuals with diagnose after stop were missclassfied as unsick due to the trunction from using sib-match design
		N_MissClass10 <- ppp %>% mutate(miss=ifelse(is.na(age_onset) & disease4550==1, 1, 0 )) %>% filter(miss==1 & status==0) %>% nrow()  # for people with children
		N_MissClass11 <- ppp %>% mutate(miss=ifelse(is.na(age_onset) & disease4550==1, 1, 0 )) %>% filter(miss==1 & status==1) %>% nrow()  # for childless people
	}
	
	
	
	###########################################################################
	#         Sibling-pairs disconcordant on both disease and outcome         #
	###########################################################################

	if (sib_pattern=="disBoth") {
		## Keep only families having full-siblings discordant on outcome status, that is at least one sibling with children and one being childless; 
		parent_outcome1 <- outcome_window_endpoint %>% filter(status==1) %>% select(parent_id) %>% unique()  # 177,327
		parent_outcome0 <- outcome_window_endpoint %>% filter(status==0) %>% select(parent_id) %>% unique()  # 97,080
		p <- outcome_window_endpoint %>% filter(parent_id %in% parent_outcome0$parent_id) %>% filter(parent_id %in% parent_outcome1$parent_id)
		dim(p)  # 188911      9
	
	
		## Within each family, randomly select one sibling with children as control, and the childless siblings with closest birth year with the control as case;
		p_outcome1 <- p %>% filter(status==1) %>% group_by(parent_id) %>% sample_n(1) %>% ungroup() %>% mutate(disease4550=ifelse(is.na(age_onset),0,1))  # 79,122
		p_outcome0 <- p %>% filter(status==0) %>% inner_join(p_outcome1[,c("parent_id","index_birth_date","stop")], by="parent_id") %>% mutate(dif=abs(index_birth_date.y-index_birth_date.x)) %>% 
		                   group_by(parent_id) %>% filter(dif==min(dif)) %>% sample_n(1) %>% 
		                   select(-index_birth_date.y, -dif) %>% rename(index_birth_date="index_birth_date.x", stop="stop.x") %>% ungroup() %>% 
		                   mutate(disease4550=ifelse(is.na(age_onset),0,1))
		
		dim(p_outcome0)  # 379644     10
		dim(p_outcome1)  # 379279     10
		
		## If the case and the control are also discordant on disease status at the time point when the event occurs to the case that is the age at first birth for case for childless, then we enroll this full-sibling pair into our matched pair case-control study; 		
		if (mod_pattern=="sibmatch"){
			p_outcome1 <- p_outcome1 %>% mutate(age_onset=ifelse(age_onset>stop, NA, age_onset))
			p_outcome0 <- p_outcome0 %>% mutate(age_onset=ifelse(age_onset>stop.y, NA, age_onset), stop=stop.y) %>% select(-stop.y) 
		}
		
		## All diagnose before stop (45/50 or 36/39)
		if (mod_pattern=="AFB4550"|mod_pattern=="95AFB"){
			p_outcome0 <- p_outcome0 %>% select(-stop.y)
		}
		
		pp <- rbind(p_outcome0, p_outcome1) %>% mutate(disease=ifelse(is.na(age_onset),0,1)) %>% mutate(age=2018-as.numeric(substr(as.character(index_birth_date),1,4)), age2=age^2)
		pp_dif <- intersect(pp[pp$disease==0,"parent_id"],pp[pp$disease==1,"parent_id"])
		ppp <- pp %>% filter(parent_id %in% pp_dif$parent_id)   #  474  12
		
		N_MissClass10 <- ppp %>% mutate(miss=ifelse(is.na(age_onset) & disease4550==1, 1, 0 )) %>% filter(miss==1 & status==0) %>% nrow()  # for people with children
		N_MissClass11 <- ppp %>% mutate(miss=ifelse(is.na(age_onset) & disease4550==1, 1, 0 )) %>% filter(miss==1 & status==1) %>% nrow()  # for childless people
	}



	###########################################################################
	#                         regression analysis                             #
	###########################################################################

	if (ppp %>% select(index_id) %>% unique() %>% nrow()>=30){
		ppp$parent_id <- factor(ppp$parent_id)
		m_mod_sib <- clogit(status ~ disease + age + age2 + strata(parent_id), data=ppp)
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
	
	df[1,"N_Family"] <- ppp %>% select(parent_id) %>% unique() %>% nrow()  # 2*2 table for large SE results
	df[1,"N_Sample"] <- ppp %>% select(index_id) %>% unique() %>% nrow()  
	df[1,"N_Record"] <- nrow(ppp)
	df[1,"N_00"] <- ppp %>% filter(disease==0 & status==0) %>% nrow()
	df[1,"N_11"] <- ppp %>% filter(disease==1 & status==1) %>% nrow()
	df[1,"N_01"] <- ppp %>% filter(disease==0 & status==1) %>% nrow()
	df[1,"N_10"] <- ppp %>% filter(disease==1 & status==0) %>% nrow()
	df[1,"N_MissClass10"] <- N_MissClass10    # only the childless sib have missclassification
	df[1,"N_MissClass11"] <- N_MissClass11    # sib with children can also have missclassification if they are not the youngest one
	df[1,"model"] <- paste0("logit_",mod_pattern)
	df[1,"sibs"] <- sib_pattern
	df[1,"sex"] <- sexs[sex_n] 
	df[1,"LRS"] <- outcomeName
	df[1,"Endpoint"] <- disease

	print(df)
	# write.table(df, paste0("Compare_",mod_pattern, "_", outcomeName ,"_", sexs[sex_n], "_F5_SCHZPHR.tsv"), append=T, quote=F, sep="\t", row.names=F, col.names=F)

	if (!file.exists(paste0("logit_",mod_pattern, "_", outcomeName ,"_", sexs[sex_n], "_SIB", sib_pattern, "_30.tsv"))){
		write.table(df, paste0("logit_",mod_pattern, "_", outcomeName ,"_", sexs[sex_n], "_SIB", sib_pattern, "_30.tsv"), append=F, quote=F, sep="\t", row.names=F, col.names=T)
	} else {
		write.table(df, paste0("logit_",mod_pattern, "_", outcomeName ,"_", sexs[sex_n], "_SIB", sib_pattern, "_30.tsv"), append=T, quote=F, sep="\t", row.names=F, col.names=F)
	}

}





