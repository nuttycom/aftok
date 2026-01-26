#!/bin/bash

source "$(dirname "$0")/_common.sh"

if [ -z "${AFTOK_USER}" ]; then
  read -p "Username: " AFTOK_USER
fi

if [ -z "${AFTOK_PASS}" ]; then
  read -s -p "Password: " AFTOK_PASS
  echo
fi

curl --verbose \
  ${ALLOW_INSECURE} \
  -c cookies.txt \
  -H "Content-Type: application/json" \
  -X POST \
  -d "{\"username\":\"$AFTOK_USER\",\"password\":\"$AFTOK_PASS\"}" \
  "${AFTOK_URL}/api/login"
