#!/bin/bash

read -p "Username: " USER
read -s -p "Password: " PASS

curl -k -u "$USER:$PASS" -X GET "https://aftok.com/projects"
