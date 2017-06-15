#!/bin/bash

read -p "Username: " USER
echo
read -p "Project UUID: " PID

curl --verbose --insecure --user $USER \
  --request POST \
  --data '{"schemaVersion": "2.0"}' \
  "https://aftok.com/projects/$PID/logStart"
