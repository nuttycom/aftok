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

read -p "Invite: " EMAIL
echo

curl --verbose --insecure --user $USER \
  --request POST \
  --data '' \
  "https://aftok.com/projects/$PROJECT/invite?email=$EMAIL"
