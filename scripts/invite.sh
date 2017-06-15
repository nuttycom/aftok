#!/bin/bash

read -p "Username: " USER
echo
read -p "Project UUID: " PROJECT
echo
read -p "Invite: " EMAIL

curl --verbose --insecure --user $USER \
  --request POST \
  --data '' \
  "https://aftok.com/projects/$PROJECT/invite?email=$EMAIL"
