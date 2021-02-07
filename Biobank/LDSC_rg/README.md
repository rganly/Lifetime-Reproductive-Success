
# Build docker

git clone https://github.com/FINNGEN/LDSC



chmod +x Dockerfile
gcloud builds submit --tag  eu.gcr.io/finngen-refinery-dsgelab/ldsc_rg:01  --gcs-source-staging-dir=gs://dsge-aoxing/mocha

