#!/bin/bash

read -p "Username: " USER
echo
read -p "Project ID: " PROJECT
read -p "BTC Address: " BTC_ADDR

curl -v -k -u $USER -X POST -d '' "https://aftok.com/projects/$PROJECT/logStart/$BTC_ADDR"
