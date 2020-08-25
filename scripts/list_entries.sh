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

curl --verbose --insecure --user $USER \
  --request GET \
  "https://$AFTOK_HOST/api/projects/$PID/logEntries?after=2020-01-01T00:00:00Z"
