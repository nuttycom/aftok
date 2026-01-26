#!/bin/bash

source "$(dirname "$0")/_common.sh"
setup_auth

if [ -z "${PID}" ]; then
  read -p "Project UUID: " PID
fi

read -p "Auction Name: " NAME
read -p "Description: " DESC
while [ -z "${CCY}" ]
do
  read -p "Currency: " CCY
  case $CCY in
    "BTC")
      CCY="satoshi"
      read -p "Raise amount, in Bitcoin satoshis: " AMOUNT
      ;;
    "ZEC")
      CCY="zatoshi"
      read -p "Raise amount, in Zcash zatoshis: " AMOUNT
      ;;
    *)
      echo "$CCY is not a supported currency. Please choose \"BTC\" or \"ZEC\""
      CCY=""
      ;;
  esac
done
echo
read -p "Auction start date (yyyy-MM-ddThh:mm:ssZ): " START
read -p "Auction end date (yyyy-MM-ddThh:mm:ssZ): " END

BODY=$(cat <<END_BODY
{
  "auction_name": "$NAME",
  "auction_desc": "$DESC",
  "raise_amount": {
    "$CCY": $AMOUNT
  },
  "auction_start": "$START",
  "auction_end": "$END"
}
END_BODY
)

curl --verbose \
  ${ALLOW_INSECURE} \
  ${AUTH_OPTS} \
  --header "Content-Type: application/json" \
  --data "$BODY" \
  "${AFTOK_URL}/api/projects/${PID}/auctions"
