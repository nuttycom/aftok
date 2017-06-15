#!/bin/bash

read -p "Username: " USER
echo
read -p "Project UUID: " PROJECT

curl --verbose --insecure --user $USER \
  --request POST \
  --data '{"schemaVersion": "2.0"}' \
  "https://aftok.com/projects/$PID/logEnd" 
