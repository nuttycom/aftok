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

if [ $(uname) == 'Darwin' ]; then
  after=$(date -v-4y +"%Y-%m-%dT%H:%M:%S%z")
else
  after=$(date -Iseconds --date='4 years ago')
fi

echo "Retrieving your log entries for project ${PID} after ${after}..."

curl --verbose \
  ${ALLOW_INSECURE} \
  --user $USER \
  "https://$AFTOK_HOST/api/user/projects/$PID/events?after=${after}&limit=100"
