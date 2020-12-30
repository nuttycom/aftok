#!/bin/bash

if [ -f ".env" ]; then
  source .env
fi

if [ -z "${AFTOK_HOST}" ]; then 
  AFTOK_HOST="aftok.com"
fi

curl --verbose \
  ${ALLOW_INSECURE} \
  "https://$AFTOK_HOST/api/logout"
