#!/bin/bash

read -p "Username: " USER
echo
read -p "Project UUID: " PROJECT

curl -v -k -u $USER -X GET "https://aftok.com/projects/$PROJECT/logEntries?after=$(date -Iseconds --date='1 month ago')"
