#!/bin/bash

read -p "Username: " USER

curl -k -u $USER -X GET "https://aftok.com/projects"
