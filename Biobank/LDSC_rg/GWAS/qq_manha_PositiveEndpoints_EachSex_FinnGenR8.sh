

gcloud compute ssh "cromwell-dsge" --project "finngen-refinery-dsgelab" --zone europe-west1-b -- -fN -L localhost:8000:localhost:80


cd  /Users/aoxliu/Documents/WDL_pipelines
java -jar cromwell-67.jar \
      submit ./qqmanPlot/qq_manha_list.wdl \
      -i /Users/aoxliu/Documents/Project1_Lifetime_Reproductive_Success/Main_analysis/Biobank/GWAS/qqplot_PositiveEndpoints_EachSex_FinnGenR8.json \
      -o options.google.json

# [2021-11-17 13:20:21,08] [info] Slf4jLogger started
# [2021-11-17 13:20:22,45] [info] Workflow 391f81be-c138-4da0-b410-50090767a438 submitted to http://localhost:8000


curl -X GET http://localhost:8000/api/workflows/v1/391f81be-c138-4da0-b410-50090767a438/status
curl -X GET http://localhost:8000/api/workflows/v1/391f81be-c138-4da0-b410-50090767a438/metadata | tr ',' '\n'



