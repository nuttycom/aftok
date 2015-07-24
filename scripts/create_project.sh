#!/bin/bash

read -p "Username: " USER
echo
read -p "Project Name: " PROJECT

curl -v -k -u $USER -X POST -H "Content-Type: application/json" -d "{\"projectName\":\"$PROJECT\"}" 'https://aftok.com/projects'

