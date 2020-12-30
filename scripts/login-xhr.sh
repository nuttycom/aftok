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

if [ -z "${PASS}" ]; then 
  read -sp "Password: " PASS
  echo
fi

curl --verbose \
  ${ALLOW_INSECURE} \
  --header "Content-Type: application/json" \
  --data "{\"username\": \"${USER}\", \"password\":\"${PASS}\"}" \
  "https://$AFTOK_HOST/api/login"

