#!/bin/bash
# Request a password reset for a user

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/_common.sh"

# This endpoint is public, no auth required

if [ -z "$1" ]; then
  echo "Usage: $0 <username_or_email>"
  echo ""
  echo "Request a password reset link be sent to the user's email."
  echo ""
  echo "Examples:"
  echo "  $0 myusername"
  echo "  $0 user@example.com"
  exit 1
fi

IDENTIFIER="$1"

# Check if the identifier looks like an email
if [[ "$IDENTIFIER" == *"@"* ]]; then
  JSON_BODY="{\"email\": \"${IDENTIFIER}\"}"
else
  JSON_BODY="{\"username\": \"${IDENTIFIER}\"}"
fi

curl -v "${AFTOK_URL}/api/password-reset/request" \
  -H "Content-Type: application/json" \
  -d "${JSON_BODY}"
