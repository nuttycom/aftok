#!/bin/bash

source "$(dirname "$0")/_common.sh"
setup_auth

if [ -z "${PID}" ]; then
  read -p "Project UUID: " PID
fi

if [ $(uname) == 'Darwin' ]; then
  after=$(date -v-4y +"%Y-%m-%dT%H:%M:%S%z")
else
  after=$(date -Iseconds --date='4 years ago')
fi

echo "Retrieving your log entries for project ${PID} after ${after}..."

curl --verbose \
  ${ALLOW_INSECURE} \
  ${AUTH_OPTS} \
  "${AFTOK_URL}/api/user/projects/${PID}/events?after=${after}&limit=100"
