#!/bin/bash

read -p "Username: " USER

curl --verbose --insecure --user $USER \
  --request GET \
  "https://aftok.com/projects"
