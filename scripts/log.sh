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

if [ $(uname) == 'Darwin' ]; then
  after=$(date -v-1m +"%Y-%m-%dT%H:%M:%S%z")
else
  after=$(date -Iseconds --date='1 month ago')
fi

curl --verbose --insecure --user $USER \
  --request GET \
  "https://aftok.com/projects/$PID/logEntries?after=${after}"
