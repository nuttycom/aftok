#!/bin/bash

if [ -f ".env" ]; then
  source .env
fi

if [ -z "${AFTOK_HOST}" ]; then 
  AFTOK_HOST="aftok.com"
fi

read -p "Zcash Address: " ZADDR

curl --verbose \
  ${ALLOW_INSECURE} \
  "https://$AFTOK_HOST/api/validate_zaddr?zaddr=${ZADDR}"
