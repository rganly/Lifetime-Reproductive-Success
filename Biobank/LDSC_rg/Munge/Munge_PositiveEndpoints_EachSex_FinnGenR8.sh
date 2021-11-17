### estimate rg between fertility and FinnGen(R6) endpoints using LDSC
# No QC for MAF and imputation info given only hapmap common SNPs are used

cd  /Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Main_analysis/Biobank/GeneticCor
gcloud compute ssh "cromwell-dsge" --project "finngen-refinery-dsgelab" --zone europe-west1-b -- -fN -L localhost:8000:localhost:80



##############################
#     munge  sumstats        #
##############################

## FinnGen endpoints-------------
gsutil ls -lh gs://dsge-aoxing/childlessness/Positive_Associations/*.saige.gz
# #chrom	pos	ref	alt	pval	mlogp	beta	sebeta	af_alt	af_alt_cases	af_alt_controls

# alt is the effect allele for finngen endpoints (munge: A1=ref, A2=alt)-------------
cd  /Users/aoxliu/Documents/WDL_pipelines
java -jar cromwell-67.jar \
      submit ./LDSC_rg/LDSC/ldsc_munge_Originalsaige.wdl \
      -i /Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Main_analysis/Biobank/GeneticCor/LDSC_PositiveEndpoints_EachSex_FinnGenR8.json \
      -o options.google.json

curl -X GET http://localhost:8000/api/workflows/v1/15a9d7ea-65e0-4a9b-a17e-d8a968cf09aa/status
curl -X GET http://localhost:8000/api/workflows/v1/15a9d7ea-65e0-4a9b-a17e-d8a968cf09aa/metadata | tr ',' '\n'



## NEB-------------
# WDL: /Users/aoxliu/Documents/WDL_pipelines/LDSC_rg/LDSC/ldsc_munge_meta.wdl
# input: /Users/aoxliu/Documents/WDL_pipelines/LDSC_rg/LDSC/ldsc_munge_meta_NEB.json
# output: gsutil ls -l gs://dsge-cromwell/ldsc_munge/993e253d-2211-4e3a-b5bf-5becba3ae024/call-munge_fg/shard-*/*.ldsc.sumstats.gz



## childless-------------
# WDL: /Users/aoxliu/Documents/WDL_pipelines/LDSC_rg/LDSC/ldsc_munge_boltlmm.wdl
# input: /Users/aoxliu/Documents/WDL_pipelines/LDSC_rg/LDSC/ldsc_munge_boltlmm_childless.json
# output: gsutil ls -l gs://dsge-cromwell/ldsc_munge/a140d5ab-5f58-4cc3-b60c-364602253ea2/call-munge_fg/shard-*/*.ldsc.sumstats.gz

# effect allele for childless is ALLELE1 (munge: A1=ALLELE1, A2=ALLELE0) (NB: effect allele are different for endpoints and childless)
# ALLELE1: first allele in bim file (usually the minor allele), used as the effect allele
# ALLELE0: second allele in bim file, used as the reference allele
# A1FREQ: frequency of first allele




