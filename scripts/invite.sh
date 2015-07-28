#!/bin/bash

read -p "Username: " USER
echo
read -p "Project ID: " PROJECT
echo
read -p "Invite: " EMAIL

curl -v -k -u $USER -X POST -d '' "https://aftok.com/projects/$PROJECT/invite?email=$EMAIL"
