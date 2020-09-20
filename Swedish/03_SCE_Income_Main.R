## This script is to summarize income data and extract max/mean income at age 25-35 and age 50-60


# Input: "tove_lev_dispink_{1977..2017}.Rdata", "demo.Rdata", "CPI_2017.csv"
# Output: Income_1977_2017.Rdata", "lisa_expM_QCED_b2535_b5060.Rdata", "lisa_expM.Rdata"
# Comments: 


setwd("/home/aoxing/DSGE_LRS/out/registry_edit/")
r_dir <- "/home/aoxing/DSGE_LRS/input/r_files/"

library(tidyverse)

'%!in%' <- function(x,y)!('%in%'(x,y))




############################################
#             Read in data                 #
############################################
## Read LISA data in 
# 1977-1989
for (i in 1977:1989){
	d <- data.frame(get(load(paste0(r_dir, "tove_lev_dispink_", i, ".Rdata"))))
	d[ ,"Ar"] <- i
	colnames(d) <- tolower(colnames(d))
	d <- d[ ,c("lopnr", "ar", "dispink")]
	print(head(d))
	if(i==1977){lisa_1977_1989 <- d} else {lisa_1977_1989 <- rbind(lisa_1977_1989, d)}
}

# 1990-2017
for (i in 1990:2017){
	d <- data.frame(get(load(paste0(r_dir, "tove_lev_lisa_", i, ".Rdata"))))
	d[ ,"Ar"] <- i
	colnames(d) <- gsub("04","",tolower(colnames(d)))
	d <- d[ ,c("lopnr", "ar", "dispinkpersf", "dispink", "dispinkfam")]
	print(head(d))
	if(i==1990){lisa_1990_2017 <- d} else {lisa_1990_2017 <- rbind(lisa_1990_2017, d)}
}

lisa_exp <- bind_rows(lisa_1977_1989, lisa_1990_2017)
nrow(lisa_exp)       # 240,123,580 
length(unique(lisa_exp$lopnr))  # 8,058,198 
save(lisa_exp, file=paste0(r_dir, "Income_1977_2017.Rdata"))


## Demographci for the full population
demo <- data.frame(get(load(paste0(r_dir,"demo.Rdata"))))
demo <- demo[,c("LopNr","FodelseAr","Kon")]  
nrow(demo)   # 9,723,423


## CPI for normalization
normalization_file <- read.csv("/home/aoxing/DSGE_LRS/input/CPI_2017.csv")



############################################
#    Normalization and only after 1990     #
############################################
# Add birth year and sex
lisa_expM <- inner_join(lisa_exp, demo, by=c("lopnr"="LopNr"))
nrow(lisa_expM)        # 240,016,177 rows
length(unique(lisa_expM$lopnr))    # 8,044,228 individuals
save(lisa_expM, file=paste0(r_dir, "lisa_expM.Rdata"))  


# Exclude data before 1990 and normalize to 2017 income
lisa_expM_QCED <- lisa_expM %>% filter(ar>=1990) %>% inner_join(normalization_file, by=c("ar"="Year")) %>% mutate(dispink_2017=dispink*CPI)
nrow(lisa_expM_QCED)   # 165,121,826
length(unique(lisa_expM_QCED$lopnr))  # 7,806,670
rm(lisa_expM)




############################################
#         Age at 25-35, and 50-60          #
############################################

# Extract age at 25-35, and 50-60
lisa_expM_QCED <- lisa_expM_QCED %>% mutate(age=ar-as.numeric(as.character(FodelseAr)))

lisa_expM_QCED_b2535 <- lisa_expM_QCED %>% filter(age>=25 & age<35) %>% select(lopnr,dispink_2017,Kon,age) %>% group_by(lopnr,Kon) %>% 
	summarize(income_Age2535_mean=9.61*mean(dispink_2017,na.rm=T), 
	          income_Age2535_max=9.61*max(dispink_2017,na.rm=T), 
	          N_year_2535=sum(is.na(dispink_2017)==F),
	          Max_age_2535=age[which(dispink_2017==max(dispink_2017))],
	          Max_age_rec_2535=max(age,na.rm=T)) 
nrow(lisa_expM_QCED_b2535)  # 3,804,458


lisa_expM_QCED_b5060 <- lisa_expM_QCED %>% filter(age>=50 & age<60) %>% select(lopnr,dispink_2017,Kon,age) %>% group_by(lopnr,Kon) %>% 
	summarize(income_Age5060_mean=9.61*mean(dispink_2017,na.rm=T), 
	          income_Age5060_max=9.61*max(dispink_2017,na.rm=T), 
	          N_year_5060=sum(is.na(dispink_2017)==F),
	          Max_age_5060=age[which(dispink_2017==max(dispink_2017))],
	          Max_age_rec_5060=max(age,na.rm=T) ) 
nrow(lisa_expM_QCED_b5060)  # 1,726,155


# combine
lisa_expM_QCED_b2535_b5060 <- full_join(lisa_expM_QCED_b2535, lisa_expM_QCED_b5060, by=c("lopnr"="lopnr", "Kon"="Kon"))
nrow(lisa_expM_QCED_b2535_b5060)   # 4,302,780
lisa_expM_QCED_b2535_b5060[,"N_year_2535"] <- ifelse(lisa_expM_QCED_b2535_b5060[,"N_year_2535"]==NA, 0, lisa_expM_QCED_b2535_b5060[,"N_year_2535"])
lisa_expM_QCED_b2535_b5060[,"N_year_5060"] <- ifelse(lisa_expM_QCED_b2535_b5060[,"N_year_5060"]==NA, 0, lisa_expM_QCED_b2535_b5060[,"N_year_5060"])
summary(lisa_expM_QCED_b2535_b5060)
save(lisa_expM_QCED_b2535_b5060, file=paste0(r_dir, "lisa_expM_QCED_b2535_b5060.Rdata"))


# relationship between max and mean
lisa_expM_QCED_b2535_b5060  %>% group_by(Kon) %>% 
    summarize(cor_b2535=cor(income_Age2535_mean, income_Age2535_max, use="complete.obs"), 
              cor_5060=cor(income_Age5060_mean, income_Age5060_max, use="complete.obs"), 
              cor_b2535_b5060_mean=cor(income_Age2535_mean, income_Age5060_mean, use="complete.obs"), 
              cor_b2535_b5060_max=cor(income_Age2535_max, income_Age5060_max, use="complete.obs"), 
              n_b2535=sum(is.na(income_Age2535_mean)==F), 
              n_b5060=sum(is.na(income_Age5060_mean)==F)) 
              
#  Kon   cor_b2535  cor_5060  cor_b2535_b5060_mean  cor_b2535_b5060_max  n_b2535  n_b5060
# 1         0.814    0.813               0.231             0.110         1958050  889615
# 2         0.755    0.781               0.283             0.131         1854810  854187



	          	
