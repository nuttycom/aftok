#!/bin/bash

read -p "Username: " USER
read -s -p "Password: " PASS
echo
read -p "Project Name: " PROJECT

curl -v -k -u "$USER:$PASS" -X POST -H "Content-Type: application/json" -d "{\"projectName\":\"$PROJECT\"}" 'https://aftok.com/projects'

