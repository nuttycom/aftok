#!/bin/bash

if [ -f ".env" ]; then
  source .env
fi

if [ -z "${AFTOK_HOST}" ]; then 
  AFTOK_HOST="aftok.com"
fi

if [ -z "${USER}" ]; then 
  read -p "Username: " USER
  echo
fi

curl --verbose \
  ${ALLOW_INSECURE} \
  --user $USER \
  "https://$AFTOK_HOST/api/login"
