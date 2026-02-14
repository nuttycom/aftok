#!/bin/bash

source "$(dirname "$0")/_common.sh"
setup_auth

read -p "Event ID: " EID

while [ -z "${ATYPE}" ]
do
  read -p "Amendment Type: " ATYPE
  case $ATYPE in
    "TIME")
      AVALUE="timeChange"
      read -p "Event Timestamp (yyyy-MM-ddTHH:mm:ssZ): " ATIME
      ;;
    *)
      echo "$ATYPE is not a amendment type. Please choose \"TIME\""
      ATYPE=""
      ;;
  esac
done

BODY=$(cat <<END_BODY
{
  "schemaVersion": "2.0",
  "amendment": "timeChange",
  "eventTime": "$ATIME"
}
END_BODY
)

curl --verbose \
  ${ALLOW_INSECURE} \
  ${AUTH_OPTS} \
  --header "Content-Type: application/json" \
  --request PUT \
  --data "$BODY" \
  "${AFTOK_URL}/api/events/${EID}/amend"
