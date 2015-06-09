#!/bin/bash

read -p "Username: " USER
read -s -p "Password: " PASS
echo
read -p "Project UUID: " PROJECT

curl -v -k -u "$USER:$PASS" -X GET "https://aftok.com/projects/$PROJECT/logEntries?after=$(date -Iseconds --date='1 month ago')"
