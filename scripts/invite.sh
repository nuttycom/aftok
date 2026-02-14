#!/bin/bash

source "$(dirname "$0")/_common.sh"
setup_auth

if [ -z "${PID}" ]; then
  read -p "Project UUID: " PID
fi

read -p "Invite Name: " GREET_NAME
read -p "Email: " EMAIL

BODY=$(cat <<END_BODY
{
  "greetName": "$GREET_NAME",
  "inviteBy": {"email": "$EMAIL"}
}
END_BODY
)

curl --verbose \
  ${ALLOW_INSECURE} \
  ${AUTH_OPTS} \
  --header "Content-Type: application/json" \
  --data "$BODY" \
  "${AFTOK_URL}/api/projects/${PID}/invite"
