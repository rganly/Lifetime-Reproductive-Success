### MR to detect/quantify causal relationship of diseases and fertility 



####################################################################################
#                             Format summary stats                                 #
####################################################################################

cd  /Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Main_analysis/Biobank/MR/summary_stats/


## disease 
# for i in  J10_CHRONTONSADEN J10_TONSILLITIS J10_PERITONSABSC K11_APPENDACUT M13_KNEEDERANGEMENTS M13_MENISCUSDERANGEMENTS
for i in  J10_TONSILLITIS J10_PERITONSABSC  M13_KNEEDERANGEMENTS M13_MENISCUSDERANGEMENTS
do
echo ${i}
gsutil cp gs://finngen-production-library-green/finngen_R7/finngen_R7_analysis_data/summary_stats/release/finngen_R7_${i}.gz  .
cat finngen_R7_${i}.gz|zcat|awk '$8>7'|tr ' ' '\t' > finngen_R7_${i}.input.tsv
done


## fertility
gsutil cp gs://dsge-aoxing/mocha/LRS/ldsc_rg/SSGC_childless_Imputed.gz  .
gsutil cp gs://dsge-aoxing/mocha/LRS/ldsc_rg/meta_NEB_POOLED_chr1to22_pos_snpid.txt.gz .

gsutil cp gs://dsge-aoxing/mocha/LRS/ldsc_rg/SSGC_Females_childless_Imputed.gz  .
gsutil cp gs://dsge-aoxing/mocha/LRS/ldsc_rg/SSGC_Males_childless_Imputed.gz  .
gsutil cp gs://dsge-aoxing/mocha/LRS/ldsc_rg/meta_NEB_MEN_chr1to22_pos_snpid.txt.gz  .
gsutil cp gs://dsge-aoxing/mocha/LRS/ldsc_rg/meta_NEB_WOMEN_chr1to22_pos_snpid.txt.gz  .

cat SSGC_childless_Imputed.gz|zcat|head -n 1 > SSGC_childles.input.tsv
cat SSGC_childless_Imputed.gz|zcat|awk '$14<0.00000005'|tr ' ' '\t' >> SSGC_childles.input.tsv

cat SSGC_Females_childless_Imputed.gz|zcat|head -n 1 > SSGC_Females_childless.input.tsv
cat SSGC_Females_childless_Imputed.gz|zcat|awk '$14<0.00000005'|tr ' ' '\t' >> SSGC_Females_childless.input.tsv

cat SSGC_Males_childless_Imputed.gz|zcat|head -n 1 > SSGC_Males_childless.input.tsv
cat SSGC_Males_childless_Imputed.gz|zcat|awk '$14<0.00000005'|tr ' ' '\t' >> SSGC_Males_childless.input.tsv



# need to format NEB file since the seprater is '\t' in header and ' ' in remaing lines
cat meta_NEB_POOLED_chr1to22_pos_snpid.txt.gz|zcat|head -n 1 > meta_NEB.tsv
cat meta_NEB_POOLED_chr1to22_pos_snpid.txt.gz|zcat|awk 'NR>1'|tr ' ' '\t' >> meta_NEB.tsv
gzip meta_NEB.tsv
cat meta_NEB_POOLED_chr1to22_pos_snpid.txt.gz|zcat|head -n 1 > SSGC_NEB.input.tsv
cat meta_NEB_POOLED_chr1to22_pos_snpid.txt.gz|zcat|awk '$11<0.00000005'|tr ' ' '\t' >> SSGC_NEB.input.tsv


cat meta_NEB_MEN_chr1to22_pos_snpid.txt.gz|zcat|head -n 1 > meta_NEB_MEN.tsv
cat meta_NEB_MEN_chr1to22_pos_snpid.txt.gz|zcat|awk 'NR>1'|tr ' ' '\t' >> meta_NEB_MEN.tsv
gzip meta_NEB_MEN.tsv
cat meta_NEB_MEN_chr1to22_pos_snpid.txt.gz|zcat|head -n 1 > SSGC_NEB_MEN.input.tsv
cat meta_NEB_MEN_chr1to22_pos_snpid.txt.gz|zcat|awk '$11<0.00000005'|tr ' ' '\t' >> SSGC_NEB_MEN.input.tsv


cat meta_NEB_WOMEN_chr1to22_pos_snpid.txt.gz|zcat|head -n 1 > meta_NEB_WOMEN.tsv
cat meta_NEB_WOMEN_chr1to22_pos_snpid.txt.gz|zcat|awk 'NR>1'|tr ' ' '\t' >> meta_NEB_WOMEN.tsv
gzip meta_NEB_WOMEN.tsv
cat meta_NEB_WOMEN_chr1to22_pos_snpid.txt.gz|zcat|head -n 1 > SSGC_NEB_WOMEN.input.tsv
cat meta_NEB_WOMEN_chr1to22_pos_snpid.txt.gz|zcat|awk '$11<0.00000005'|tr ' ' '\t' >> SSGC_NEB_WOMEN.input.tsv




####################################################################################
#     MR-BASE (https://mrcieu.github.io/TwoSampleMR/articles/introduction.html)    #
####################################################################################

setwd("/Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Main_analysis/Biobank/MR")
# install.packages("remotes")
# remotes::install_github("MRCIEU/TwoSampleMR")
# remotes::install_github("MRCIEU/ieugwasr")
library(TwoSampleMR)
library(ieugwasr)
library(MRInstruments)
library(ggplot2)
library(data.table)
library(dplyr)


## disease -> fertility  --------------------------------
for (disease in c("J10_CHRONTONSADEN","J10_TONSILLITIS","J10_PERITONSABSC","K11_APPENDACUT","M13_KNEEDERANGEMENTS","M13_MENISCUSDERANGEMENTS")) {
	# exposure data
	exposure_dat <- read_exposure_data(paste0("summary_stats/finngen_R7_",disease,".input.tsv"), sep="\t", clump=TRUE, 
		snp_col="rsids", beta_col="beta", se_col="sebeta", eaf_col="af_alt", effect_allele_col="alt", other_allele_col="ref", pval_col="pval", chr_col="#chrom", pos_col="pos")
	if (disease=="J10_CHRONTONSADEN") {exposure_dat$exposure <- "Chronic diseases of tonsils and adenoids"}
	if (disease=="J10_TONSILLITIS") {exposure_dat$exposure <- "Other and unspecified tonssillitis"}
	if (disease=="J10_PERITONSABSC") {exposure_dat$exposure <- "Peritonsillar abscess"}
	if (disease=="K11_APPENDACUT") {exposure_dat$exposure <- "Acute appendicitis"}
	if (disease=="M13_KNEEDERANGEMENTS") {exposure_dat$exposure <- "Internar derangement of knee"}
	if (disease=="M13_MENISCUSDERANGEMENTS") {exposure_dat$exposure <- "Meniscus derangement"}
	
	# outcome data
	# for (fert in c("childless","Females_childless","Males_childless","NEB","NEB_WOMEN","NEB_MEN")) {
	for (fert in c("NEB","NEB_WOMEN","NEB_MEN")) {
		print(paste0("Exposure is ",disease,"; Outcome is ",fert))
		if (fert %in% c("childless","Females_childless","Males_childless")) {
			outcome_dat <- read_outcome_data(snps=exposure_dat$SNP, paste0("summary_stats/SSGC_",fert,"_Imputed.gz"), sep="\t", 
				snp_col="SNP", beta_col="BETA", se_col="SE", effect_allele_col="ALLELE1", other_allele_col="ALLELE0", eaf_col="A1FREQ", pval_col="P_BOLT_LMM_INF")
		} else {
			outcome_dat <- read_outcome_data(snps=exposure_dat$SNP, paste0("summary_stats/meta_",fert,".tsv.gz"), sep="\t", 
				snp_col="MARKER", beta_col="Beta", se_col="SE", effect_allele_col="Allele1", other_allele_col="Allele2", eaf_col="Freq", pval_col="Pvalue")
		}
		
		if (fert %in% c("childless","NEB")) {outcome_dat$outcome <- fert}
		if (fert=="Females_childless") {outcome_dat$outcome <- "childless in women"}
		if (fert=="Males_childless") {outcome_dat$outcome <- "childless in men"}
		if (fert=="NEB_WOMEN") {outcome_dat$outcome <- "NEB in women"}
		if (fert=="NEB_MEN") {outcome_dat$outcome <- "NEB in men"}
		
		# harmonise
		dat <- harmonise_data(exposure_dat, outcome_dat)
		dat <- dat %>% select(SNP, effect_allele.exposure, other_allele.exposure, effect_allele.outcome, other_allele.outcome, beta.exposure, beta.outcome, eaf.exposure,
		                      eaf.outcome, remove, palindromic, ambiguous, id.outcome, se.outcome, pval.outcome,
		                      outcome, mr_keep.outcome, pval_origin.outcome, data_source.outcome, chr.exposure, pos.exposure,
		                      pval.exposure, se.exposure, exposure, mr_keep.exposure, pval_origin.exposure,
		                      id.exposure, data_source.exposure, action, mr_keep, samplesize.outcome)
		
		# write.table(dat, paste0("summary_stats/TwoSampleMR_",disease,"_",fert,"_dat.tsv"), row.names=F, sep="\t", quote=F)
		# if (disease=="J10_CHRONTONSADEN" & fert=="childless"){
		if (!file.exists("summary_stats/TwoSampleMR_dat.tsv")) {
			write.table(dat, "summary_stats/TwoSampleMR_dat.tsv", row.names=F, col.names=T, sep="\t", quote=F, append=F)
		} else {
			write.table(dat, "summary_stats/TwoSampleMR_dat.tsv", row.names=F, col.names=F, sep="\t", quote=F, append=T)
		}
		
		# MR
		mr_results <- mr(dat)
		# write.table(mr_results, paste0("summary_stats/TwoSampleMR_",disease,"_",fert,"_results.tsv"), row.names=F, sep="\t", quote=F)
		# if (disease=="J10_CHRONTONSADEN" & fert=="childless"){
		if (!file.exists("summary_stats/TwoSampleMR_results.tsv")) {
			write.table(mr_results, "summary_stats/TwoSampleMR_results.tsv", row.names=F, col.names=T, sep="\t", quote=F, append=F)
		} else {
			write.table(mr_results, "summary_stats/TwoSampleMR_results.tsv", row.names=F, col.names=F, sep="\t", quote=F, append=T)
		}
		
		# plot
		scat_plot <- mr_scatter_plot(mr_results, dat)
		scat_plot
		ggsave(paste0("plots/TwoSampleMR_",disease,"_",fert,".png"), width=5, height=6)
		
	}
}



## fertility -> disease --------------------------------
# for (fert in c("childless","NEB","Females_childless","Males_childless","NEB_WOMEN","NEB_MEN")) {

for (fert in c("NEB","NEB_WOMEN","NEB_MEN")) {
	# exposure data
	if (fert %in% c("childless","Females_childless","Males_childless")) {
		exposure_dat <- read_exposure_data(paste0("summary_stats/SSGC_",fert,".input.tsv"), sep="\t", clump=TRUE, 
			snp_col="SNP", beta_col="BETA", se_col="SE", effect_allele_col="ALLELE1", other_allele_col="ALLELE0", eaf_col="A1FREQ", pval_col="P_BOLT_LMM_INF")
	} else {
		exposure_dat <- read_exposure_data(paste0("summary_stats/SSGC_",fert,".input.tsv"), sep="\t", clump=TRUE, 
			snp_col="MARKER", beta_col="Beta", se_col="SE", effect_allele_col="Allele1", other_allele_col="Allele2", eaf_col="Freq", pval_col="Pvalue")
	}
	exposure_dat$exposure <- fert
	if (fert %in% c("childless","NEB")) {exposure_dat$exposure <- fert}
	if (fert=="Females_childless") {exposure_dat$exposure <- "childless in women"}
	if (fert=="Males_childless") {exposure_dat$exposure <- "childless in men"}
	if (fert=="NEB_WOMEN") {exposure_dat$exposure <- "NEB in women"}
	if (fert=="NEB_MEN") {exposure_dat$exposure <- "NEB in men"}
	
	# outcome data
	for (disease in c("J10_CHRONTONSADEN","J10_TONSILLITIS","J10_PERITONSABSC","K11_APPENDACUT","M13_KNEEDERANGEMENTS","M13_MENISCUSDERANGEMENTS")) {
		print(paste0("Exposure is ", fert,"; Outcome is ",disease))
		outcome_dat <- read_outcome_data(snps=exposure_dat$SNP, paste0("summary_stats/finngen_R7_",disease,".gz"), sep="\t", 
			snp_col="rsids", beta_col="beta", se_col="sebeta", effect_allele_col="alt", other_allele_col="ref", eaf_col="af_alt", pval_col="pval", chr_col="#chrom", pos_col="pos")
		if (disease=="J10_CHRONTONSADEN") {outcome_dat$outcome <- "Chronic diseases of tonsils and adenoids"}
		if (disease=="J10_TONSILLITIS") {outcome_dat$outcome <- "Other and unspecified tonssillitis"}
		if (disease=="J10_PERITONSABSC") {outcome_dat$outcome <- "Peritonsillar abscess"}
		if (disease=="K11_APPENDACUT") {outcome_dat$outcome <- "Acute appendicitis"}
		if (disease=="M13_KNEEDERANGEMENTS") {outcome_dat$outcome <- "Internar derangement of knee"}
		if (disease=="M13_MENISCUSDERANGEMENTS") {outcome_dat$outcome <- "Meniscus derangement"}
	
		# harmonise
		dat <- harmonise_data(exposure_dat, outcome_dat) %>% mutate(chr.exposure=chr.outcome, pos.exposure=pos.outcome)
		dat <- dat %>% select(SNP, effect_allele.exposure, other_allele.exposure, effect_allele.outcome, other_allele.outcome, beta.exposure, beta.outcome, eaf.exposure,
			                      eaf.outcome, remove, palindromic, ambiguous, id.outcome, se.outcome, pval.outcome,
			                      outcome, mr_keep.outcome, pval_origin.outcome, data_source.outcome, chr.exposure, pos.exposure,
			                      pval.exposure, se.exposure, exposure, mr_keep.exposure, pval_origin.exposure,
			                      id.exposure, data_source.exposure, action, mr_keep, samplesize.outcome)
		write.table(dat, "summary_stats/TwoSampleMR_dat.tsv", row.names=F, col.names=F, sep="\t", quote=F, append=T)
		
		# MR
		mr_results <- mr(dat)
		write.table(mr_results, "summary_stats/TwoSampleMR_results.tsv", row.names=F, col.names=F, sep="\t", quote=F, append=T)
		
		# plot
		scat_plot <- mr_scatter_plot(mr_results, dat)
		scat_plot
		ggsave(paste0("plots/TwoSampleMR_", fert, "_",disease, ".png"), width=5, height=6)
	}
}



####################################################################################
#                  MR-PRESSO (https://github.com/rondolab/MR-PRESSO)               #
####################################################################################

## MR-PRESSO has three components, including:
# 1) detection of horizontal pleiotropy (MR-PRESSO global test)
# 2) correction of horizontal pleiotropy via outlier removal (MR-PRESSO outlier test)
# 3) testing of significant distortion in the causal estimates before and after outlier removal (MR-PRESSO distortion test).


## To install the latest development builds directly from GitHub, run this instead:
# if (!require("devtools")) { install.packages("devtools") } else {}
# devtools::install_github("rondolab/MR-PRESSO")
library(MRPRESSO)

dat <- read.table("summary_stats/TwoSampleMR_dat.tsv", sep="\t", header=T, colClasses="character") %>% 
	mutate(beta.exposure=as.numeric(beta.exposure), beta.outcome=as.numeric(beta.outcome), 
	       se.exposure=as.numeric(se.exposure), se.outcome=as.numeric(se.outcome))
sces <- dat %>% group_by(exposure, outcome) %>% count()

for (sce_n in 1:nrow(sces)) {
	print(paste0("Exposure is ",unlist(sces[sce_n,"exposure"]),"; Outcome is ",unlist(sces[sce_n,"outcome"])))
	dat_i <- dat %>% filter(exposure==unlist(sces[sce_n,"exposure"]) & outcome==unlist(sces[sce_n,"outcome"]))
	# dat_i <- dat[dat$exposure==unlist(sces[sce_n,"exposure"]) & dat$outcome==unlist(sces[sce_n,"outcome"]),]
	# print(dat_i)
	
	if (nrow(dat_i)>=5) {
		mr_results <- mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure = "se.exposure", 
			OUTLIERtest = TRUE, DISTORTIONtest = TRUE, data = dat_i, NbDistribution = 1000,  SignifThreshold = 0.05)
		
		df <- mr_results[[1]] %>% select(-Exposure)
		df[,"exposure"] <- sces[sce_n,"exposure"]
		df[,"outcome"] <- sces[sce_n,"outcome"]
		print(df)
		
		if (!file.exists("summary_stats/MRPRESSO_results.tsv")) {
			write.table(df, "summary_stats/MRPRESSO_results.tsv", row.names=F, col.names=T, sep="\t", quote=F, append=F)
		} else {
			write.table(df, "summary_stats/MRPRESSO_results.tsv", row.names=F, col.names=F, sep="\t", quote=F, append=T)
		}
	}
}




# Load a simulated toy dataset
data(SummaryStats)
# Run MR-PRESSO global method
mr_one <- mr_presso(BetaOutcome = "Y_effect", BetaExposure = "E1_effect", SdOutcome = "Y_se", SdExposure = "E1_se", 
	OUTLIERtest = TRUE, DISTORTIONtest = TRUE, data = SummaryStats, NbDistribution = 1000,  SignifThreshold = 0.05)


# Run MR-PRESSO on a multi-variable MR (MMR) model specifying several exposures
mr_multi <- mr_presso(BetaOutcome = "Y_effect", BetaExposure = c("E1_effect", "E2_effect"), SdOutcome = "Y_se", SdExposure = c("E1_se", "E2_se"), 
	OUTLIERtest = TRUE, DISTORTIONtest = TRUE, data = SummaryStats, NbDistribution = 1000,  SignifThreshold = 0.05)


dat <- read.table("summary_stats/TwoSampleMR_dat.tsv", sep="\t", header=T)
dat_i <- dat %>% filter(exposure=="Acute appendicitis" & outcome=="childless")   ##############
# dat_i <- dat[dat$exposure=="Acute appendicitis" & dat$outcome=="childless", ]
mr_test <- mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure = "se.exposure", 
	OUTLIERtest = TRUE, DISTORTIONtest = TRUE, data = dat_i, NbDistribution = 1000,  SignifThreshold = 0.05)


mr_all_test <- mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure = "se.exposure", 
	OUTLIERtest = TRUE, DISTORTIONtest = TRUE, data = dat, NbDistribution = 1000,  SignifThreshold = 0.05)



### 
dat_i <- read.table("summary_stats/test", sep="\t", header=T)

mr_test <- mr_presso(BetaOutcome = "beta.outcome", BetaExposure = "beta.exposure", SdOutcome = "se.outcome", SdExposure = "se.exposure", 
	OUTLIERtest = TRUE, DISTORTIONtest = TRUE, data = dat_i, NbDistribution = 1000,  SignifThreshold = 0.05)





