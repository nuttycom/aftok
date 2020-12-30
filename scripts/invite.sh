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

if [ -z "${PID}" ]; then
  read -p "Project UUID: " PID
  echo
fi

read -p "Invite: " EMAIL
echo

curl --verbose \
  ${ALLOW_INSECURE} \
  --user $USER \
  --header "Content-Type: application/json" \
  --data '{}' \
  "https://$AFTOK_HOST/api/projects/$PID/invite?email=$EMAIL"
