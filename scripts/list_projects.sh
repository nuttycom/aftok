#!/bin/bash

source "$(dirname "$0")/_common.sh"
setup_auth

curl \
  ${ALLOW_INSECURE} \
  ${AUTH_OPTS} \
  "${AFTOK_URL}/api/projects"
