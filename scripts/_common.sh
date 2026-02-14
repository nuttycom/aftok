# Common configuration for Aftok API scripts
# Source this file at the beginning of each script

if [ -f ".env" ]; then
  source .env
fi

if [ -z "${AFTOK_HOST}" ]; then
  AFTOK_HOST="aftok.com"
fi

# Use HTTP for local development, HTTPS for production
if [[ "${AFTOK_HOST}" == *"localhost"* ]] || [[ "${AFTOK_HOST}" == *":"* && ! "${AFTOK_HOST}" == *"443"* ]]; then
  AFTOK_URL="http://${AFTOK_HOST}"
else
  AFTOK_URL="https://${AFTOK_HOST}"
fi

# Set up authentication options using cookies
# Run login.sh first to authenticate and create cookies.txt
setup_auth() {
  if [ -f "cookies.txt" ]; then
    AUTH_OPTS="-b cookies.txt"
  else
    echo "Error: cookies.txt not found. Run login.sh first to authenticate." >&2
    exit 1
  fi
}
