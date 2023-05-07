#!/usr/bin/env bash

set -o errexit

if [ ! -f aftok.cabal ]; then
    echo "This script should be run in the root of the aftok repository (home of aftok.cabal). This does not appear to be the case, and you are in $PWD"
    exit
fi

echo "Creating local directories"

# In an attempt to future-proof things, we'll take our initial directory list from the docker-compose.yml sources
for dir in $(awk '/^[[:space:]]*source: .\/local/{ print $2}' docker-compose.yml); do
    if [ ! -d "$dir" ]; then
        echo "Creating '$dir'"
        mkdir -p "$dir"
    fi
done

echo "Starting the database"

docker-compose up -d aftokdb

echo "Initializing the database"

./deploy/dbinit.sh -e -m

# TODO: finish server configuration, client setup

