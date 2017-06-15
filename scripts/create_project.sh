#!/bin/bash

read -p "Username: " USER
echo
read -p "Project Name: " PROJECT

curl --verbose --insecure --user $USER \
  --request POST --header "Content-Type: application/json" \
  --data "{\"projectName\":\"$PROJECT\"}" \
  'https://aftok.com/projects'

