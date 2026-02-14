#!/bin/bash

source "$(dirname "$0")/_common.sh"
setup_auth

if [ -z "${PID}" ]; then
  read -p "Project UUID: " PID
fi

curl \
  ${ALLOW_INSECURE} \
  ${AUTH_OPTS} \
  "${AFTOK_URL}/api/projects/${PID}"
