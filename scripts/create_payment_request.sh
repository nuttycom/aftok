#!/bin/bash

source "$(dirname "$0")/_common.sh"
setup_auth

if [ -z "${PID}" ]; then
  read -p "Project UUID: " PID
fi

read -p "Billable ID: " BID

curl --verbose \
  ${ALLOW_INSECURE} \
  ${AUTH_OPTS} \
  --header "Content-Type: application/json" \
  --data "{}" \
  "${AFTOK_URL}/api/projects/${PID}/billables/${BID}/paymentRequests"
