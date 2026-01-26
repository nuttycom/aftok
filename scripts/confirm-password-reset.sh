#!/bin/bash
# Confirm a password reset with token and new password

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/_common.sh"

# This endpoint is public, no auth required

if [ -z "$1" ] || [ -z "$2" ]; then
  echo "Usage: $0 <token> <new_password>"
  echo ""
  echo "Reset password using the token from the password reset email."
  echo ""
  echo "Example:"
  echo "  $0 abc123-token mynewpassword"
  exit 1
fi

TOKEN="$1"
NEW_PASSWORD="$2"

curl -v "${AFTOK_URL}/api/password-reset/reset" \
  -H "Content-Type: application/json" \
  -d "{\"token\": \"${TOKEN}\", \"newPassword\": \"${NEW_PASSWORD}\"}"
