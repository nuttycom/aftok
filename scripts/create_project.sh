#!/bin/bash

if [ -f ".env" ]; then
  source .env
fi

if [ -z "${USER}" ]; then 
  read -p "Username: " USER
  echo
fi

read -p "Project Name: " PROJECT
echo
read -p "Undepreciated period (months): " UNDEPMON
read -p "Depreciation duration (months): " DEPMON

BODY=$(cat <<END_BODY
{
  "projectName": "$PROJECT", 
  "depf": { 
    "type": "LinearDepreciation",  
    "arguments": {
      "undep": $UNDEPMON,
      "dep": $DEPMON
    }
  }
}
END_BODY
)

curl --verbose --insecure --user $USER \
  --request POST --header "Content-Type: application/json" \
  --data "$BODY" \
  'https://aftok.com/api/projects'

