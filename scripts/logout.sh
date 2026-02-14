#!/bin/bash

source "$(dirname "$0")/_common.sh"

# Logout and clear the cookies file
curl --verbose \
  ${ALLOW_INSECURE} \
  -b cookies.txt \
  -c cookies.txt \
  -X POST \
  "${AFTOK_URL}/api/logout"

# Remove the cookies file
rm -f cookies.txt
echo "Logged out and removed cookies.txt"
