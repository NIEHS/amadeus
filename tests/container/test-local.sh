#!/bin/bash

# export CURL_CA_BUNDLE and SSL_CERT_FILE environmental variables to vertify
# servers' SSL certificates during download
export CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

# Run tests via container.sif.
apptainer exec \
  --bind $PWD:/mnt \
  --bind /tmp:/opt/tmp \
  container.sif Rscript test-coverage.R
