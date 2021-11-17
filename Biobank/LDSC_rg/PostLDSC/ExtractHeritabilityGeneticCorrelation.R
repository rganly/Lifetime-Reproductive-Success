############################################
#      Extract h2 & rg for endpoints       #
############################################

cd  /Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Main_analysis/Biobank/GeneticCor
gsutil cp gs://dsge-cromwell/ldsc_rg/78f79657-0530-4e5f-842e-c009c20c851c/call-rg/shard-*/glob*/ldsc*.log   ./LDSC_log_bysex/
gsutil cp gs://dsge-cromwell/ldsc_rg/78f79657-0530-4e5f-842e-c009c20c851c/call-gather/LDSC_PositiveEndpoints_EachSex_FinnGenR8_results.txt  ./LDSC_log_bysex/


setwd("/Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Main_analysis/Biobank/GeneticCor")

# extract h2 for finngen endpoints ---
readRG <- function(log) {
	rg_log <- readLines(log)
	h2_t1 <- rg_log[grep("Heritability of phenotype 1$",rg_log)+2]
	if (length(h2_t1)>0){
		h2_t1 <- strsplit(h2_t1," ")
		h2 <- as.numeric(h2_t1[[1]][5])
		h2_se <- as.numeric(gsub("\\(|\\)","",h2_t1[[1]][6]))
		h2_p <- 2*pnorm(-abs(h2/h2_se))
	
		h2_int_t1 <- rg_log[grep("Heritability of phenotype 1$",rg_log)+5]
		h2_int_t1 <- strsplit(h2_int_t1," ")
		h2_int <- as.numeric(h2_int_t1[[1]][2])
		h2_int_se <- as.numeric(gsub("\\(|\\)","",h2_int_t1[[1]][3]))
	
		Endpoint <- sub("_childless.log","",sub("ldsc_","",p))
		res <- matrix(c(Endpoint, h2, h2_se, h2_int, h2_int_se),nrow=1)
	} else {
		res <- matrix(c(NA, NA, NA, NA, NA),nrow=1)
	}
	colnames(res) <- c("Endpoint", "h2_Endpoint", "h2_se_Endpoint", "h2_int_Endpoint", "h2_int_se_Endpoint")
	return(res)
}


phenolist <- list.files("/Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Main_analysis/Biobank/GeneticCor/LDSC_log_bysex", pattern="^ldsc_")
h2_Endpoint <- NULL
for (p in phenolist) {
	h2_Endpoint <- rbind(h2_Endpoint,readRG(paste0("/Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Main_analysis/Biobank/GeneticCor/LDSC_log_bysex/",p)))
}

h2_Endpoint <- h2_Endpoint %>% data.frame() %>% 
	mutate(Endpoint=gsub("_childless_Female.log", "", Endpoint), 
	       Endpoint=gsub("_childless_Male.log", "", Endpoint), 
	       Endpoint=gsub("_NEB_Female.log", "", Endpoint),
	       Endpoint=gsub("_NEB_Male.log", "", Endpoint)) %>% 
	group_by(Endpoint) %>% slice(1) %>% ungroup()


# read in rg for finngen endpoints ---
rg <- read.table("LDSC_log_bysex/LDSC_PositiveEndpoints_EachSex_FinnGenR8_results.txt", header=T) %>% 
	mutate(p1=gsub("/cromwell_root/dsge-aoxing/childlessness/Positive_Associations/","",p1),
	       p2=gsub("/cromwell_root/dsge-aoxing/childlessness/Positive_Associations/","",p2)) %>% 
	mutate(rg=-rg)  # different effect alleles were used for finngen endpoints and childless/NEB



# combine h2 and rg --- 
rg_h2 <- inner_join(rg, h2_Endpoint, by=c("p1"="Endpoint")) %>% 
	rename(Disease="p1", Reproduction="p2", 
	       rg.se="se", rg.Pval="p", 
	       h2.Reproduction="h2_obs", h2.Reproduction.se="h2_obs_se", h2_int.Reproduction="h2_int", h2_int.Reproduction.se="h2_int_se", 
	       h2.Disease="h2_Endpoint", h2.Disease.se="h2_se_Endpoint", h2_int.Disease="h2_int_Endpoint", h2_int.Disease.se="h2_int_se_Endpoint") %>% 
	mutate(Sex=ifelse(Reproduction %in% c("Females_childless", "NEB_WOMEN"), "Women", "Men"),
	       Reproduction=ifelse(Reproduction %in% c("Females_childless", "Males_childless"), "Childlessness", "NEB"),
	       Disease=gsub("_Female", "", Disease), Disease=gsub("_Male", "", Disease)) %>% 
	mutate(Reproduction=factor(Reproduction, levels=c("NEB","Childlessness"))) %>% 
	mutate(rg_025=rg-1.96*rg.se, rg_975=rg+1.96*rg.se) %>% 
	select(Disease, Reproduction, Sex, rg, rg.se, rg.Pval, rg_025, rg_975,
	       h2.Disease, h2.Disease.se, h2_int.Disease, h2_int.Disease.se,
	       h2.Reproduction, h2.Reproduction.se, h2_int.Reproduction, h2_int.Reproduction.se) %>% arrange(-desc(Disease))
write.table(rg_h2, "LDSC_PositiveEndpoints_EachSex_FinnGenR8_rg_h2_all.tsv", append=F, quote=F, sep="\t", row.names=F, col.names=T)


# Only kept diseases with SNP heritability Z scores >4
rg_h2_qc <- rg_h2 %>% filter(as.numeric(h2.Disease) > 4*as.numeric(h2.Disease.se))
write.table(rg_h2_qc, "LDSC_PositiveEndpoints_EachSex_FinnGenR8_rg_h2_qc.tsv", append=F, quote=F, sep="\t", row.names=F, col.names=T)

