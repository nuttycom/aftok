#!/bin/bash

read -p "Username: " USER
read -s -p "Password: " PASS
echo
read -p "Email: " EMAIL
read -p "BTC Address: " BTC_ADDR

curl -k -v -H 'Content-Type: application/json' -d "{\"username\":\"$USER\", \"password\":\"$PASS\", \"email\":\"$EMAIL\", \"btcAddr\":\"$BTC_ADDR\"}" 'https://aftok.com/register'

