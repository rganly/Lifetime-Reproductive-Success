####################################################
#           rg Separately for each sex             #
####################################################

# copy all munged summary statistics to one bucket -----------
gsutil cp gs://dsge-cromwell/ldsc_munge/15a9d7ea-65e0-4a9b-a17e-d8a968cf09aa/call-munge_fg/shard-*/*.ldsc.sumstats.gz  gs://dsge-aoxing/childlessness/Positive_Associations/
gsutil cp gs://dsge-cromwell/ldsc_munge/2826cee8-3f4d-4430-8f64-e5e2389a3a55/call-munge_fg/shard-*/*sumstats.gz  gs://dsge-aoxing/childlessness/Positive_Associations/
gsutil cp gs://dsge-cromwell/ldsc_munge/993e253d-2211-4e3a-b5bf-5becba3ae024/call-munge_fg/shard-*/*sumstats.gz  gs://dsge-aoxing/childlessness/Positive_Associations/


# submit the job -----------
cd  /Users/aoxliu/Documents/WDL_pipelines
java -jar cromwell-67.jar \
      submit ./LDSC_rg/LDSC/ldsc_rg_premunged.wdl \
      -i /Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Main_analysis/Biobank/GeneticCor/LDSC_PositiveEndpoints_EachSex_FinnGenR8_premunged.json \
      -o options.google.json

curl -X GET http://localhost:8000/api/workflows/v1/78f79657-0530-4e5f-842e-c009c20c851c/status
curl -X GET http://localhost:8000/api/workflows/v1/78f79657-0530-4e5f-842e-c009c20c851c/metadata | tr ',' '\n'
