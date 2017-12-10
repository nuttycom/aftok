#!/bin/bash

if [ -f ".env" ]; then
  source .env
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
  "https://aftok.com/projects/$PROJECT/logEntries?after=$(date -Iseconds --date='1 month ago')"
