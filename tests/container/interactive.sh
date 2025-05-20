############################      CERTIFICATES      ############################
# Export CURL_CA_BUNDLE and SSL_CERT_FILE environmental variables to vertify
# servers' SSL certificates during download.
export CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

# Open interactive session with container.sif
apptainer exec \
  --bind $PWD:/mnt \
  --bind /tmp:/opt/tmp \
  container.sif \
  /usr/local/lib/R/bin/R