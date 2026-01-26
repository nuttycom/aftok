#!/bin/bash

source "$(dirname "$0")/_common.sh"
setup_auth

read -p "Project Name: " PROJECT
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

curl --verbose \
  ${ALLOW_INSECURE} \
  ${AUTH_OPTS} \
  --header "Content-Type: application/json" \
  --data "$BODY" \
  "${AFTOK_URL}/api/projects"
