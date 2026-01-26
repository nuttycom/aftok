#!/bin/bash

source "$(dirname "$0")/_common.sh"

# Note: This endpoint doesn't require authentication
read -p "Zcash Address: " ZADDR

curl --verbose \
  ${ALLOW_INSECURE} \
  "${AFTOK_URL}/api/validate_zaddr?zaddr=${ZADDR}"
