#!/bin/bash

if [ -f ".env" ]; then
  source .env
fi

if [ -z "${USER}" ]; then 
  read -p "Username: " USER
  echo
fi

curl --verbose --insecure --user $USER \
  --request GET \
  "https://aftok.com/api/projects"
