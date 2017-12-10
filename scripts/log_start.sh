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
  --request POST \
  --data '{"schemaVersion": "2.0"}' \
  "https://aftok.com/projects/$PID/logStart"
