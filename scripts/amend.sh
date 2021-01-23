#!/bin/bash

if [ -f ".env" ]; then
  source .env
fi

if [ -z "${AFTOK_HOST}" ]; then 
  AFTOK_HOST="aftok.com"
fi

if [ -z "${USER}" ]; then 
  read -p "Username: " USER
  echo
fi

read -p "Event ID: " EID

while [ -z "${ATYPE}" ]
do 
  read -p "Amendment Type: " ATYPE
  case $ATYPE in
    # "CREDIT_TO")
    #   AVALUE="creditToChange"
    #   read -p "Raise amount, in Bitcoin satoshis: " AMOUNT
    #   ;;
    "TIME")
      AVALUE="timeChange"
      read -p "Event Timestamp (yyyy-MM-ddTHH:mm:ssZ): " ATIME
      ;;
    *)
      echo "$ATYPE is not a amendment type. Please choose \"TIME\"" # or \"CREDIT_TO\""
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
  --user $USER \
  --header "Content-Type: application/json" \
  --request PUT \
  --data "$BODY" \
  "https://$AFTOK_HOST/api/events/$EID/amend"
