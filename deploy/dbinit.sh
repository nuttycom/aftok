#!/bin/bash

echo "About to create the 'aftok' user; you will be prompted for the postgres superuser password."
createuser -h localhost -U postgres -W -P aftok
echo "About to create the 'aftok' database; you will be prompted for the postgres superuser password."
createdb -h localhost -U postgres -W -O aftok aftok 
echo "About to add the uuid-ossp extension to the 'aftok' database; you will be prompted for the postgres superuser password."
psql -h localhost -U postgres -W aftok \
  -c 'CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;'
if [ -n "$1" ]; then 
  echo "About to populate the 'aftok' database; you will be prompted for the aftok database password."
  psql -h localhost -U aftok -W aftok < $1
fi

