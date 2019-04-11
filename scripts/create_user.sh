#!/bin/bash

read -p "Username: " USER
read -s -p "Password: " PASS
echo
read -p "Email: " EMAIL
read -p "BTC Address: " BTC_ADDR

curl --verbose --insecure \
  --request POST --header 'Content-Type: application/json' \
  --data "{\"username\":\"$USER\", \"password\":\"$PASS\", \"email\":\"$EMAIL\", \"btcAddr\":\"$BTC_ADDR\"}" \
  'https://aftok.com/api/register'

