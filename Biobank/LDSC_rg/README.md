
# Build docker

cd /Users/aoxliu/Documents/WDL_pipelines/LDSC_rg/

git clone https://github.com/FINNGEN/LDSC


cd  /Users/aoxliu/Documents/WDL_pipelines/LDSC_rg/LDSC/docker/

cp. Dockerfile Dockerfile_original  # need to remove extra "&&" from the Dockerfile

cat Dockerfile_original |sed s/"ldsc.git &&"/"ldsc.git"/g > Dockerfile

chmod +x Dockerfile 

gcloud builds submit --tag  eu.gcr.io/finngen-refinery-dsgelab/ldsc_rg:01  --gcs-source-staging-dir=gs://dsge-aoxing/mocha

