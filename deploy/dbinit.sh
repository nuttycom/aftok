#!/bin/bash

DEFAULT_PORT=15432

usage() {
    cat <<EOF
Usage: dbinit.sh [-p <port>] [-e]

  -p: the port to use to talk to PostgreSQL (default: $DEFAULT_PORT)
  -e: load environment variables from docker-compose environment for credentials
  -m: perform initial migrations (bootstrap)
EOF

    exit 1
}

while getopts "p:em" OPTION; do
    case $OPTION in
        e)
            eval "$(sed -re 's/POSTGRES_(.*)/export PG\1/' pg.env)"
            ;;
        m)
            MIGRATIONS=bootstrap
            ;;
        p)
            PGPORT=$OPTARG
            ;;
        *)
            usage
            ;;
    esac
done

shift $(( $OPTIND - 1 ))



# We default to the default docker-compose port for PG
echo "Using PostgreSQL port ${PGPORT:=$DEFAULT_PORT}"

echo "About to create the 'aftok' user"
createuser -h localhost -p $PGPORT -U postgres -P aftok

echo "About to create the 'aftok' database"
createdb -h localhost -p $PGPORT -U postgres -O aftok aftok

echo "About to add the uuid-ossp extension to the 'aftok' database"
psql -h localhost -p $PGPORT -U postgres aftok \
     -c 'CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;'

if [ -n "$1" ]; then 
  echo "About to populate the 'aftok' database; you will be prompted for the aftok database password."
  psql -h localhost -p $PGPORT -U aftok -W aftok < $1
fi

if [ "$MIGRATIONS" = "bootstrap" ]; then
  echo "Performing initial migrations to bootstrap"

  read -p "Enter the aftok user's password: " -s DBM_PASSWORD
  
  export DBM_DATABASE_TYPE="postgresql"
  export DBM_DATABASE="host=127.0.0.1 port=$PGPORT user=aftok password=$DBM_PASSWORD dbname=aftok"
  export DBM_MIGRATION_STORE="./migrations"
  export DBM_TIMESTAMP_FILENAMES=on

  moo-postgresql upgrade
fi
