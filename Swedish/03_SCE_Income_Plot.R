## This script is to check raw income data and max/mean income at age 25-35 and age 50-60


# Input: "lisa_expM.Rdata", "lisa_expM_QCED_b2535_b5060.Rdata", "indexW.Rdata"
# Output: "SCE_Income_Raw.pdf", "SCE_Income_Normalization.pdf", "SCE_Income_Age2535_5060.pdf", "stats_by_year.csv"
# Comments: 


setwd("/home/aoxing/DSGE_LRS/out/registry_edit/")
r_dir <- "/home/aoxing/DSGE_LRS/input/r_files/"

library(tidyverse)
'%!in%' <- function(x,y)!('%in%'(x,y))



############################################
#               Raw data                   #
############################################

lisa_expM <- data.frame(get(load(paste0(r_dir, "lisa_expM.Rdata")))) 
pdf("SCE_Income_Raw.pdf", width=16)

# Number of years recorded for each individual
n_years_per_individuals <- lisa_expM %>% group_by(lopnr) %>% count()
# pdf("SCE_Income_n_years_per_individuals.pdf",width=16)
df <- data.frame(table(n_years_per_individuals$n))
ggplot(df, aes(x=Var1, y=Freq)) +geom_bar(stat="identity")+theme_bw()
# dev.off()


# Number of records for year
n_record_per_year <- lisa_expM %>% group_by(ar) %>% count()
# pdf("SCE_Income_n_record_per_year.pdf",width=16)
ggplot(n_record_per_year, aes(x=as.factor(ar), y=n)) +geom_bar(stat="identity")+theme_bw()
# dev.off()


# Lot of NA's in 1977
lisa_expM %>% group_by(ar) %>% summarize(count=sum(is.na(dispink)))
n_zero_per_year <- lisa_expM %>% group_by(ar) %>% summarize(count=sum(dispink==0))
# pdf("SCE_Income_n_zero_per_year.pdf",width=16)
ggplot(n_zero_per_year, aes(x=as.factor(ar), y=count)) +geom_bar(stat="identity")+theme_bw()
# dev.off()


# Sample, otherwise too big
unique_id <- unique(lisa_expM$lopnr)
lisa_expMS <- lisa_expM[lisa_expM$lopnr %in% unique_id[sample(1:length(unique_id),10000)],]


# Median income per year for each individual
median_individual_stat_year <- lisa_expMS %>% group_by(lopnr,ar,Kon) %>% summarise(median_income=median(dispink,na.rm=TRUE))
# pdf("SCE_Income_median_individual_stat_year.pdf",width=12)
median_individual_stat_year$sex <- ifelse(median_individual_stat_year$Kon==1,"Males","Females")
options(scipen=10000)
ggplot(median_individual_stat_year, aes(x=ar, y=median_income, group=lopnr)) +geom_point(size=0.2, shape=1, alpha=0.1) +geom_line(alpha=0.1, size=0.1)+
 scale_y_log10("Median income in SEK (x100)") +theme_minimal() +theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +geom_smooth(se=F, size=1.5, col="red",aes(group = 1)) +scale_x_continuous("Statistical year", breaks=seq(1977,2017,2)) +facet_wrap(~sex)
# dev.off()

dev.off()
 
 


############################################
#             Normalized data              #
############################################

lisa_expM_QCED <- lisa_expM %>% filter(ar>=1990) %>% inner_join(normalization_file, by=c("ar"="Year")) %>% mutate(dispink_2017=dispink*CPI)
rm(lisa_expM)
pdf("SCE_Income_Normalization.pdf", width=16)

# Median income per individual
unique_id <- unique(lisa_expM_QCED$lopnr)
lisa_expM_QCEDS <- lisa_expM_QCED[lisa_expM_QCED$lopnr %in% unique_id[sample(1:length(unique_id),500000)],] 
median_individual <- lisa_expM_QCEDS %>% group_by(lopnr,ar) %>% summarise(median_income=median(dispink_2017,na.rm=TRUE))
sum(is.na(median_individual$median_income)) # 0 


# Median income distribution per year bin
# pdf("SCE_Income_median_individual.pdf",width=12)
median_individual$cut_b_year <- cut(median_individual$ar,5, dig.lab = 4)
options(scipen=10000)
ggplot(median_individual, aes(x=cut_b_year, y=median_income)) +
	geom_violin(trim=FALSE, fill="gray")+
	scale_y_log10("Median income in SEK (x100) - adjusted 2017", breaks=c(1:10 %o% 10^(2:5))) +xlab("Year of birth") +theme_minimal() +theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + geom_boxplot(width=0.1) 
# dev.off()


# Median income per year for each individual
unique_id <- unique(lisa_expM_QCED$lopnr)
lisa_expM_QCEDS <- lisa_expM_QCED[lisa_expM_QCED$lopnr %in% unique_id[sample(1:length(unique_id),10000)],]
median_individual_stat_year <- lisa_expM_QCEDS %>% group_by(lopnr,ar,Kon) %>% summarise(median_income=median(dispink_2017,na.rm=TRUE))
# pdf("SCE_Income_median_individual_stat_year.pdf",width=12)
median_individual_stat_year$sex <- ifelse(median_individual_stat_year$Kon==1,"Males","Females")
options(scipen=10000)
ggplot(median_individual_stat_year, aes(x=ar, y=median_income, group = lopnr)) +geom_point(size=0.2, shape=1, alpha=0.1) +geom_line(alpha=0.1,size=0.1)+
	scale_y_log10("Median income in SEK (x100) - adjusted 2017") +theme_minimal() +theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1)) +geom_smooth(se=F, size=1.5, col="red",aes(group = 1)) +scale_x_continuous("Statistical year", breaks=seq(1990,2017,2)) +facet_wrap(~sex)
# dev.off()


# Table of income by year and sex
Tables_by_year <- lisa_expM_QCED %>% group_by(ar,Kon) %>% summarise(median_income=median(dispink_2017,na.rm=TRUE),mean_income=mean(dispink_2017,na.rm=TRUE),max_income=max(dispink_2017,na.rm=TRUE),min_income=min(dispink_2017,na.rm=TRUE),sd_income=sd(dispink_2017,na.rm=TRUE))
write.csv(Tables_by_year, file="stats_by_year.csv", quote=F, row.names=F)


# Income at age 30 (keep only index persons)
indexW <- data.frame(get(load(paste0(r_dir, "indexW.Rdata"))))
nrow(indexW)  # 2,555,541

sum(!unique(indexW$LopNr) %in% unique(lisa_expM_QCED$lopnr))  # 122,848 in index file but do not have income info
lisa_expM_QCED_30 <- lisa_expM_QCED %>% filter(lopnr %in% indexW$LopNr) %>% mutate(diff=ar-as.numeric(FodelseAr))


# Find age closest to 30
# Can we calculate the income at age 30 for index person?
# pdf("SCE_Income_N_Age30.pdf",width=16)
df <- lisa_expM_QCED_30 %>% group_by(lopnr) %>% filter(abs(diff-30)==min(abs(diff-30))) %>% group_by(diff) %>% count()
ggplot(df, aes(x=as.factor(diff), y=n)) +geom_bar(stat="identity")+theme_bw()
# dev.off()


# relationship between familial income and personal income
unique_id <- unique(lisa_expM_QCED_30$lopnr)
lisa_expM_QCED_30S <- lisa_expM_QCED_30[lisa_expM_QCED_30$lopnr %in% unique_id[sample(1:length(unique_id),10000)],]
# pdf("SCE_Income_Individual_Family.pdf",width=16)
df <- lisa_expM_QCED_30S %>% filter(ar==2017) %>% mutate(sex=ifelse(Kon==1,"Males","Females"))
ggplot(df, aes(x=dispink, y=dispinkfam)) +geom_point(alpha=0.3) +theme_bw() +geom_smooth(se=F, size=1.5, col="red") +facet_wrap(~sex) +geom_abline(slope = 1) +scale_y_log10("Family income in SEK (x100) - 2017") +scale_x_log10("Individual income in SEK (x100) - 2017")
# dev.off()

dev.off()
rm(lisa_expM_QCED)



############################################
#         Age at 25-35, and 50-60          #
############################################

lisa_expM_QCED_b2535_b5060 <- data.frame(get(load(paste0(r_dir, "lisa_expM_QCED_b2535_b5060.Rdata")))) 


pdf("SCE_Income_Age2535_5060.pdf",width=16)
df <- data.frame(lisa_expM_QCED_b2535_b5060) %>% filter(lopnr %in% sample(indexW$LopNr, 10000, replace=F)) 
ggplot(df, aes(x=income_Age2535_mean, y=income_Age2535_max)) +
   geom_point(alpha=0.3) +theme_bw() +geom_smooth(se=F, size=1.5, col="red") +facet_wrap(~Kon) +geom_abline(slope = 1) +scale_y_log10("Individual income in SWE (Euro)-Max of age 25-35") +scale_x_log10("Individual income in SWE (Euro)-Mean of age 25-35")

ggplot(df, aes(x=income_Age5060_mean, y=income_Age5060_max)) +
   geom_point(alpha=0.3) +theme_bw() +geom_smooth(se=F, size=1.5, col="red") +facet_wrap(~Kon) +geom_abline(slope = 1) +scale_y_log10("Individual income in SWE (Euro)-Max of age 50-60") +scale_x_log10("Individual income in SWE (Euro)-Mean of age 50-60")

df <- lisa_expM_QCED_b2535_b5060 %>% group_by(N_year_2535) %>% count()
ggplot(df, aes(x=as.factor(N_year_2535), y=n)) +geom_bar(stat="identity")+theme_bw()

df <- lisa_expM_QCED_b2535_b5060 %>% group_by(N_year_5060) %>% count()
ggplot(df, aes(x=as.factor(N_year_5060), y=n)) +geom_bar(stat="identity")+theme_bw()
dev.off()

