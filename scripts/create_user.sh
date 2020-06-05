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
read -p "BTC Address: " BTC_ADDR

curl --verbose --insecure \
  --request POST --header 'Content-Type: application/json' \
  --data "{\"username\":\"$USER\", \"password\":\"$PASS\", \"email\":\"$EMAIL\", \"btcAddr\":\"$BTC_ADDR\"}" \
  "https://$AFTOK_HOST/api/register"

