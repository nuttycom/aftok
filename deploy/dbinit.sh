#!/bin/bash

createuser -h localhost -U postgres -W -P aftok
createdb -h localhost -U postgres -W -O aftok aftok 
psql -h localhost -U postgres -W aftok \
  -c 'CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;'
if [ -n "$1" ]; then 
  psql -h localhost -U aftok -W aftok < $1
fi

