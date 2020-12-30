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

if [ -z "${PID}" ]; then
  read -p "Project UUID: " PID
  echo
fi

read -p "Billable Name: " BNAME
read -p "Description: " BDESC

while [ -z "${RECUR}" ]
do 
  read -p "Recurrence Period [A|M|W|O] ((A)nnual, (M)onthly, (W)eekly, (O)ne-time): " RECUR
  case $RECUR in
    "A")
      RECUR="annually"
      read -p "Recur every ? years: " RECUR_COUNT
      ;;
    "M")
      RECUR="monthly"
      read -p "Recur every ? months: " RECUR_COUNT
      ;;
    "W")
      RECUR="weekly"
      read -p "Recur every ? weeks: " RECUR_COUNT
      ;;
    "O")
      RECUR="one-time"
      ;;
    *)
      echo "$RECUR is not a supported recurrence. Please choose \"A\" \"M\", \"W\" or \"O\""
      RECUR=""
      ;;
  esac
done

while [ -z "${CURRENCY}" ]
do 
  read -p "Currency [BTC|ZEC]: " CURRENCY
  case $CURRENCY in
    "BTC")
      read -p "Bill Total (in Satoshis): " AMOUNT
      break
      ;;
    "ZEC")
      read -p "Bill Total (in Zatoshis): " AMOUNT
      break
      ;;
    *)
      echo "$CURRENCY is not a supported currency. Please choose \"BTC\" or \"ZEC\""
      CURRENCY=""
      ;;
  esac
done

read -p "Grace Period (days): " GRACE_PERIOD
read -p "Request Expiry Period (seconds): " REQUEST_EXPIRY

BODY=$(cat <<END_BODY
{
  "schemaVersion": "1.0",
  "name": "$BNAME", 
  "description": "$BDESC",
  "message": "Thank you for your patronage.",
  "recurrence": { "$RECUR": $RECUR_COUNT },
  "currency": "$CURRENCY",
  "amount": $AMOUNT,
  "gracePeriod": $GRACE_PERIOD,
  "requestExpiryPeriod": $REQUEST_EXPIRY
}
END_BODY
)

curl --verbose \
  ${ALLOW_INSECURE} \
  --user $USER \
  --header "Content-Type: application/json" \
  --data "$BODY" \
  "https://$AFTOK_HOST/api/projects/${PID}/billables"

