
### 1. Build docker for LDSC

cd /Users/aoxliu/Documents/WDL_pipelines/LDSC_rg/

git clone https://github.com/FINNGEN/LDSC


cd  /Users/aoxliu/Documents/WDL_pipelines/LDSC_rg/LDSC/docker/

cp. Dockerfile Dockerfile_original          # need to remove extra "&&" from the Dockerfile

cat Dockerfile_original |sed s/"ldsc.git &&"/"ldsc.git"/g > Dockerfile

chmod +x Dockerfile 

gcloud builds submit --tag  eu.gcr.io/finngen-refinery-dsgelab/ldsc_rg:01  --gcs-source-staging-dir=gs://dsge-aoxing/mocha


           
### 2. Run GWAS for 6 disease endpoints (signifcantly reduce childlessness) using FinnGen R8 data in women and men separately




### 3. Munge summary statistics for finngen disease endpoints and childlessness/NEB (Mathieson et al., 2020)



### 4. Run LDSC


### 5. Extract h2 and rg from the log file and only keep traits with SNP heriability Z-score higher than 4



