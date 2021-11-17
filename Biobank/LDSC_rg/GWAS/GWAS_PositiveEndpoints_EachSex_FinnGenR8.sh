
#### Run GWAS for each sex separately for endpoints significantly increased the chance of having a child

## Endpoints that significantly increased the chance of having a child -------------
# J10_CHRONTONSADEN: Chronic diseases of tonsils and adenoids
# J10_TONSILLITIS: Other and unspecified tonssillitis
# J10_PERITONSABSC: Peritonsillar abscess
# K11_APPENDACUT: Acute appendicitis
# M13_KNEEDERANGEMENTS: Internar derangement of knee
# M13_MENISCUSDERANGEMENTS: Meniscus derangement



############################################################################
##           Prepare input files for these endpoints in one file           #
############################################################################

# Set up the working environments
gcloud compute ssh "aoxing-mocha" --project "finngen-refinery-dsgelab" --zone europe-west1-b 
cd  /home/aoxliu/mCA/output/FinnGenR8Affymetrix/plot

# mount the data from Google bucket to VM instance
gcsfuse --implicit-dirs  r8_data  r8_data
# fusermount -u  /home/aoxliu/mCA/output/FinnGenR8Affymetrix/plot/r8_data


# PCs and phenotypes (cases 1, controls 0, everyone else NA), and upload the file.
setwd("/home/aoxliu/mCA/output/FinnGenR8Affymetrix/plot")

# install.packages("data.table")
library(data.table)
library(dplyr)
'%!in%' <- function(x,y)!('%in%'(x,y))


## read in phenotype data 
phe <- data.frame(fread("r8_data/pheno/sisuv4/R8_sisuv4_COV_PHENO_V4_1.txt.gz", sep="\t", header=T))
phe <- phe %>% mutate(BL_AGE2=as.numeric(BL_AGE)^2, SMOKE=ifelse(SMOKE3=="never",0,1))
phe <- phe[,c("FINNGENID", "SEX", "BL_AGE", "BL_AGE2", "SMOKE", 
              "J10_CHRONTONSADEN", "J10_TONSILLITIS", "J10_PERITONSABSC", "K11_APPENDACUT", "M13_KNEEDERANGEMENTS", "M13_MENISCUSDERANGEMENTS",
              paste0("PC",1:10), colnames(phe)[substr(colnames(phe),1,14)=="BATCH_AxiomGT1"])]

# Add endpoints specific for each sex
phe <- phe %>% mutate(J10_CHRONTONSADEN_Female=ifelse(SEX=="female", J10_CHRONTONSADEN, NA), 
                      J10_CHRONTONSADEN_Male=ifelse(SEX=="male", J10_CHRONTONSADEN, NA), 
                      J10_TONSILLITIS_Female=ifelse(SEX=="female", J10_TONSILLITIS, NA), 
                      J10_TONSILLITIS_Male=ifelse(SEX=="male", J10_TONSILLITIS, NA), 
                      J10_PERITONSABSC_Female=ifelse(SEX=="female", J10_PERITONSABSC, NA), 
                      J10_PERITONSABSC_Male=ifelse(SEX=="male", J10_PERITONSABSC, NA), 
                      K11_APPENDACUT_Female=ifelse(SEX=="female", K11_APPENDACUT, NA), 
                      K11_APPENDACUT_Male=ifelse(SEX=="male", K11_APPENDACUT, NA), 
                      M13_KNEEDERANGEMENTS_Female=ifelse(SEX=="female", M13_KNEEDERANGEMENTS, NA), 
                      M13_KNEEDERANGEMENTS_Male=ifelse(SEX=="male", M13_KNEEDERANGEMENTS, NA), 
                      M13_MENISCUSDERANGEMENTS_Female=ifelse(SEX=="female", M13_MENISCUSDERANGEMENTS, NA), 
                      M13_MENISCUSDERANGEMENTS_Male=ifelse(SEX=="male", M13_MENISCUSDERANGEMENTS, NA) )

phe %>% group_by(J10_CHRONTONSADEN_Female, J10_CHRONTONSADEN_Male, SEX) %>% count()
phe %>% group_by(J10_TONSILLITIS_Female, J10_TONSILLITIS_Male, SEX) %>% count()
phe %>% group_by(J10_PERITONSABSC_Female, J10_PERITONSABSC_Male, SEX) %>% count()
phe %>% group_by(K11_APPENDACUT_Female, K11_APPENDACUT_Male, SEX) %>% count()
phe %>% group_by(M13_KNEEDERANGEMENTS_Female, M13_KNEEDERANGEMENTS_Male, SEX) %>% count()
phe %>% group_by(M13_MENISCUSDERANGEMENTS_Female, M13_MENISCUSDERANGEMENTS_Male, SEX) %>% count()


phe <- phe %>% select(FINNGENID, SEX, BL_AGE, BL_AGE2, SMOKE,
                      J10_CHRONTONSADEN, J10_CHRONTONSADEN_Female, J10_CHRONTONSADEN_Male, 
                      J10_TONSILLITIS, J10_TONSILLITIS_Female, J10_TONSILLITIS_Male, 
                      J10_PERITONSABSC, J10_PERITONSABSC_Female, J10_PERITONSABSC_Male, 
                      K11_APPENDACUT, K11_APPENDACUT_Female, K11_APPENDACUT_Male, 
                      M13_KNEEDERANGEMENTS, M13_KNEEDERANGEMENTS_Female, M13_KNEEDERANGEMENTS_Male,
                      M13_MENISCUSDERANGEMENTS, M13_MENISCUSDERANGEMENTS_Female, M13_MENISCUSDERANGEMENTS_Male,
                      PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10,
                      BATCH_AxiomGT1_b01_V2P2,BATCH_AxiomGT1_b02_V2P2,BATCH_AxiomGT1_b03_V2P2,BATCH_AxiomGT1_b04_V2P2,BATCH_AxiomGT1_b05_V2P2,BATCH_AxiomGT1_b06_V2P2,BATCH_AxiomGT1_b07_V2P2,BATCH_AxiomGT1_b08_V2P2,BATCH_AxiomGT1_b09_V2P2,BATCH_AxiomGT1_b10_V2P2,BATCH_AxiomGT1_b11_V2P2,BATCH_AxiomGT1_b12_V2P2,BATCH_AxiomGT1_b13_V2P2,BATCH_AxiomGT1_b14_V2P2,BATCH_AxiomGT1_b15_V2P2,BATCH_AxiomGT1_b16_V2P2,BATCH_AxiomGT1_b17_V2P2,BATCH_AxiomGT1_b18_V2P2,BATCH_AxiomGT1_b19_V2P2,BATCH_AxiomGT1_b20_V2P2,BATCH_AxiomGT1_b21_V2P2,BATCH_AxiomGT1_b22_V2P2,BATCH_AxiomGT1_b23_V2P2,BATCH_AxiomGT1_b24_V2P2,BATCH_AxiomGT1_b25_V2P2,BATCH_AxiomGT1_b26_V2P2,BATCH_AxiomGT1_b27_V2P2,BATCH_AxiomGT1_b28_V2P2,BATCH_AxiomGT1_b29_V2P2,BATCH_AxiomGT1_b30_V2P2,BATCH_AxiomGT1_b31_V2,BATCH_AxiomGT1_b3234_V2,BATCH_AxiomGT1_b33_V2,BATCH_AxiomGT1_b35_V2,BATCH_AxiomGT1_b36_V2,BATCH_AxiomGT1_b37_V2,BATCH_AxiomGT1_b38_V2,BATCH_AxiomGT1_b39_V2,BATCH_AxiomGT1_b40_V2,BATCH_AxiomGT1_b41_V2,BATCH_AxiomGT1_b42_V2,BATCH_AxiomGT1_b43_V2,BATCH_AxiomGT1_b44_V2,BATCH_AxiomGT1_b45_V2,BATCH_AxiomGT1_b46_V2,BATCH_AxiomGT1_b47_V2,BATCH_AxiomGT1_b48_V2,BATCH_AxiomGT1_b49_V2,BATCH_AxiomGT1_b50_V2,BATCH_AxiomGT1_b51_V2,BATCH_AxiomGT1_b52_V2,BATCH_AxiomGT1_b53_V3,BATCH_AxiomGT1_b54_V3,BATCH_AxiomGT1_b55_V3,BATCH_AxiomGT1_b56_V3,BATCH_AxiomGT1_b57_V3,BATCH_AxiomGT1_b58_V3,BATCH_AxiomGT1_b59_V3,BATCH_AxiomGT1_b60_V3,BATCH_AxiomGT1_b61_V3,BATCH_AxiomGT1_b62_V3,BATCH_AxiomGT1_b63_V3,BATCH_AxiomGT1_b64_V3,BATCH_AxiomGT1_b65_V4,BATCH_AxiomGT1_b66_V4,BATCH_AxiomGT1_b67_V4,BATCH_AxiomGT1_b68_V4,BATCH_AxiomGT1_b69_V4,BATCH_AxiomGT1_b70_V4,BATCH_AxiomGT1_b71_V4,BATCH_AxiomGT1_b72_V4)
dim(phe)  # 342499    104
write.table(phe, "GWAS_PositiveEndpoints_EachSex_FinnGenR8.tsv", col.names=T, row.names=F, quote=F, sep="\t")

ep <- matrix(c("J10_CHRONTONSADEN_Female", "J10_CHRONTONSADEN_Male",
               "J10_TONSILLITIS_Female","J10_TONSILLITIS_Male",
               "J10_PERITONSABSC_Female","J10_PERITONSABSC_Male",
               "K11_APPENDACUT_Female","K11_APPENDACUT_Male",
               "M13_KNEEDERANGEMENTS_Female","M13_KNEEDERANGEMENTS_Male",
               "M13_MENISCUSDERANGEMENTS_Female","M13_MENISCUSDERANGEMENTS_Male"), ncol=1)
write.table(ep, "GWAS_PositiveEndpoints_EachSex_FinnGenR8_List.txt", col.names=F, row.names=F, quote=F, sep="\t")

system("gsutil  cp  GWAS_PositiveEndpoints_EachSex_FinnGenR8.tsv  gs://dsge-aoxing/childlessness/")
system("gsutil  cp  GWAS_PositiveEndpoints_EachSex_FinnGenR8_List.txt  gs://dsge-aoxing/childlessness/")

system("gsutil cat gs://dsge-aoxing/childlessness/GWAS_PositiveEndpoints_EachSex_FinnGenR8.tsv|head")
system("gsutil cat gs://dsge-aoxing/childlessness/GWAS_PositiveEndpoints_EachSex_FinnGenR8_List.txt|head")


# saige.test_combine.test.samplefile is the list of samples in bgen file
gsutil cat gs://r8_data/bgen/sisuv4/chunks_50k/finngen_R8_sisuv4_1.0.bgen.sample| grep ^FG| awk '{print $1}' > R8_356213_samples.txt
gsutil cp R8_356213_samples.txt gs://dsge-aoxing/mocha/FinnGenR8/
gsutil cat gs://dsge-aoxing/mocha/FinnGenR8/R8_356213_samples.txt| wc -l    # 356,246




############################################
#          Submit SAIGE pipeline           #
############################################ 

gcloud compute ssh "cromwell-dsge" --project "finngen-refinery-dsgelab" --zone europe-west1-b -- -fN -L localhost:8000:localhost:80


## submit original wdl pipeline (error since the 2nd step longflag "analysisType" is not in the scripts)
cd  /Users/aoxliu/Documents/WDL_pipelines
java -jar cromwell-67.jar \
      submit ./SAIGE/saige.wdl \
      -i /Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Main_analysis/Biobank/GWAS/GWAS_PositiveEndpoints_EachSex_FinnGenR8.json \
      --imports /Users/aoxliu/Documents/WDL_pipelines/SAIGE/saige_sub.zip \
      -o options.google.json
curl -X GET http://localhost:8000/api/workflows/v1/49aa04a4-d9e4-471e-8b58-a3ea237a3c90/status
curl -X GET http://localhost:8000/api/workflows/v1/49aa04a4-d9e4-471e-8b58-a3ea237a3c90/metadata | tr ',' '\n'



## submit wdl pipeline which removed analysisType longfalg
cd  /Users/aoxliu/Documents/WDL_pipelines
java -jar cromwell-67.jar \
      submit ./SAIGE/saige_RemoveanalysisType.wdl \
      -i /Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Main_analysis/Biobank/GWAS/GWAS_PositiveEndpoints_EachSex_FinnGenR8.json \
      --imports /Users/aoxliu/Documents/WDL_pipelines/SAIGE/saige_sub_RemoveanalysisType.zip \
      -o options.google.json

curl -X GET http://localhost:8000/api/workflows/v1/a62524b3-aaeb-46ff-9f70-f760161b18df/status
curl -X GET http://localhost:8000/api/workflows/v1/a62524b3-aaeb-46ff-9f70-f760161b18df/metadata | tr ',' '\n'




###################################################################
#    Check results and copy all results to a same bucket          #
###################################################################

gsutil cp gs://dsge-cromwell/saige/a62524b3-aaeb-46ff-9f70-f760161b18df/call-test_combine/shard-*/sub.test_combine/*/call-combine/*.gz  gs://dsge-aoxing/childlessness/Positive_Associations
gsutil ls -lh gs://dsge-aoxing/childlessness/Positive_Associations/*.gz






