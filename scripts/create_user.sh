#!/bin/bash

if [ -f ".env" ]; then
  source .env
fi

if [ -z "${AFTOK_HOST}" ]; then 
  AFTOK_HOST="aftok.com"
fi

read -p "Username: " USER
read -s -p "Password: " PASS
echo
read -p "Email: " EMAIL

curl --verbose \
  ${ALLOW_INSECURE} \
  --header 'Content-Type: application/json' \
  --data "{\"username\":\"$USER\", \"password\":\"$PASS\", \"recoveryType\": \"email\", \"recoveryEmail\": \"$EMAIL\", \"captchaToken\":\"FAKE\"}" \
  "https://$AFTOK_HOST/api/register"

