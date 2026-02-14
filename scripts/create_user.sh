#!/bin/bash

source "$(dirname "$0")/_common.sh"

# Note: This endpoint doesn't require authentication (it's for registration)
read -p "Username: " USER
read -s -p "Password: " PASS
echo
read -p "Email: " EMAIL

curl --verbose \
  ${ALLOW_INSECURE} \
  --header 'Content-Type: application/json' \
  --data "{\"username\":\"$USER\", \"password\":\"$PASS\", \"recoveryType\": \"email\", \"recoveryEmail\": \"$EMAIL\", \"captchaToken\":\"FAKE\"}" \
  "${AFTOK_URL}/api/register"
