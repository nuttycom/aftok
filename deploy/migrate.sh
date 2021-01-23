#!/bin/bash

docker exec -it aftok-server ~/.local/bin/moo-postgresql upgrade --config-file /etc/aftok/aftok-migrations.cfg

